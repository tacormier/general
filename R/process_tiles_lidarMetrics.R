# This code calculates metrics on lasfiles. 
# It makes several adjustments to the elevation values, including subtracting the DTM values to account for 
# slope, then finding the ground peak, removing anything beneath it, and adjusting/triming the other heights 
# relative to the ground (Fabio's code).lasdata = las data table/df, res = vertical resolution (m), dtm = high 
# res dtm (raster) for removing slope effects (optional), max.height is the largest height to consider in metrics 
# and is determined by the user based on field data or expert knowledge; heights taller than this value are flagged. 
# min.height is lowest height considered in the metrics and is determined by the user based on field data or expert 
# knowledge; lower heights are flagged.

require(signal)
require(raster)

rasterOptions(tmpdir="/home/tcormier/RasTmpDir/")
source("/mnt/a/tcormier/scripts/general/R/handy_functions_TC.R")

#######################################################################
#Variables
indir <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/las_extract/extract_20150918/txt/"
#indir <- "/mnt/a/tcormier/SE_Asia/Ellis_Paper/lidar/Sumalindo_west/Tiles_20m/"
#indir <- "/mnt/a/tcormier/testing/lidar_processing/windows/Tiles_20m/"
profdir <- paste0(indir, "/profiles/")
dir.create(profdir, showWarnings=F)
tilesize <- 30 #in meters
binsize <- 0.5 #vertical resolution of profile (m)
min_ptden <- 8 #min CartoData = 8 ppm2; min SL Brazil = 4 ppm2
vegflag <- "F" #(T/F): vegetation returns classified?
#Values for flagging bad points or tiles - based on field data or expert knowledge
maxht <- 80 #maximum expected tree height. If less than 1% of points > maxht, these points will be simply removed (outliers). If more than 1% of points > maxht, tile will be flagged as bad and will not be processed
minht <- -3 #if less than 1% of points < minht, these points will be simply removed (outliers). If more than 1% of points < minht, tile will be flagged as bad and will not be processed. We expect some negative heights after normalization. Too many negative heights indicate an issue with the DTM.
minhtamp <- 2 #min normalized height amplitude for tile to be processed. If minhtamp < (max(lasdata$z)-min(lasdata$z)) after removal of outliers, tile will not be processed (no trees)
#do you want to save the profile so you don't have to calc again?
#Usually yes for plot profiles. Will save to indir/profiles
saveprof <- "Y"

#regions
regions <- c("Campeche_Yucatan", "Chiapas", "Chihuahua", "Jalisco", "Oaxaca1", "Oaxaca2", "Oaxaca3")
#dtm.file <- "/mnt/a/fgoncalves/Mexico/dtm_mosaic.tif"
#dtm.file <- "/mnt/a/tcormier/SE_Asia/Ellis_Paper/lidar/DTM_all/DTM_Ellis/se_asia_dtm.vrt"
#dtm.file <- "/Volumes/share-2/fgoncalves/Mexico/dtm_mosaic.tif"
dtmlist <-"/mnt/a/tcormier/Mexico_CMS/lidar/DTMs/lists/cartodata_dtm_vrt_list.txt"
#######################################################################
dtms <- scan(dtmlist, what="character")
# as long as "regions" are listed in the same order as the dtms in dtmlist, this 
# will work - otherwise, need to do some matching based on region - keeping it
# simple for the moment!
dtminfo <- data.frame(region=regions, dtm=dtms, stringsAsFactors = F)

#dtm <- raster(dtm.file)

#get filenames and coordinates (lower-left corner of the tiles)
lasfiles <- list.files(indir, pattern="\\.txt", full.names=T)
dummy <- sub(".txt", "", basename(lasfiles))
#dummy <- basename(dummy)
# dummy <- lapply(strsplit(dummy, "_"), rev)
# xcor <- as.numeric(sapply(dummy, "[", 2))
# ycor <- as.numeric(sapply(dummy, "[", 1))
# plot(xcor,ycor)

#process each tile
#if xcor and ycor are not in file names
xcor <- vector()
ycor <- vector()
  
#define metrics, etc
dummyn1 <- paste0("ptden_lt", min_ptden)
dummyn2 <- paste0("fgt_", maxht, "m_gt1")
dummyn3 <- paste0("fgt_", maxht, "m")
dummyn4 <- paste0("flt_minus", abs(minht), "m_gt1")
dummyn5 <- paste0("flt_minus", abs(minht), "m")
dummyn6 <- paste0("htamp_lt_", minhtamp, "m")
log <- array(0, c(length(lasfiles), 11))
colnames(log) <- c("no_ground", "no_veg", dummyn1, "ptden", dummyn2, dummyn3, dummyn4, dummyn5, dummyn6, "htamp", "nodata")
ptden <- array(NA, length(lasfiles))
freq <- seq(0.01, 0.3, by=0.001) #maybe use 0.0001 to make sure we have no phase jumps? Depends on processing time. If 0.0001 is used, changes are neede below
nf <- length(freq)
amp <- array(NA, c(length(lasfiles), nf))
  colnames(amp) <- gsub("p0.", "p.", paste0("amp", format(freq, digits=2)))
pha <- array(NA, c(length(lasfiles), nf))
  colnames(pha) <- gsub("a0.", "a.", paste0("pha", format(freq, digits=2)))
trad <- array(NA, c(length(lasfiles), 10))
  colnames(trad) <- c("meanh", "stdevh", "h25", "h50", "h75", "mch", "npeaks", "ent", "fslope", "g2t")

#initiate pdf to save profile figs
pdf(file=paste0(profdir, '/profiles.pdf'), width=7, height=7)

#loop
for (p in 1:length(lasfiles)) {
  print(paste0("calculating metrics for file ", p, " of ", length(lasfiles)))
  # big cluster to sort out which dtm (all different UTM zones, so not mosaicked - 
  # solve this later??)
  
  # lasdata file - pull out region -----> can this be done outside the loop? No need to reopen the DTM for every lasfile in the same site
  lf <- basename(lasfiles[p])
  lf.region <- unlist(strsplit(lf, "_"))[1]
  dtm.region <- dtminfo$dtm[pmatch(lf.region,dtminfo$region)]
  dtm <- raster(dtm.region)
  
  lasdata <- read.csv(lasfiles[p], header=F, col.names=c("x","y","z","i","a","n","r","c"))
  # This xcor/ycor calc is only if that info is not in the lasfile file name
  xcor[p] <- min(lasdata$x)
  ycor[p] <- min(lasdata$y)

  #if lasdata is empty, jump to next
  if (nrow(lasdata)==0) {
    log[p,11] <- 1
    next()
  }
  
  #remove rows with no elevation if they exist
  naid <- which(is.na(lasdata$z))
  if (length(naid) > 0) lasdata <- lasdata[-naid,]
  
  #remove topography (i.e., normalize the elevations) after removing points for which the DTM is NA
  ptground <- extract(dtm, lasdata[,1:2])
  naid2 <- which(is.na(ptground))
  if (length(naid2) > 0) {
  		ptground <- ptground[-naid2]
  		lasdata <- lasdata[-naid2,]
  }
  lasdata$z <- lasdata$z - ptground
  
  #tile quality check
  if (!(2 %in% lasdata$c)) {log[p,1] <- 1} #No ground return
    dummy <- c(3,4,5) %in% lasdata$c
  if (vegflag == "T") {
    if (sum(dummy+0) == 0) {log[p,2] <- 1} #No vegetation return
  }
  fgt_maxht <- length(lasdata$z[lasdata$z > maxht]) / length(lasdata$z) * 100
  if (fgt_maxht > 1) {log[p,5] <- 1; log[p,6] <- fgt_maxht} #More than 1% of normalized heights > maxht m
  flt_minht <- length(lasdata$z[lasdata$z < minht]) / length(lasdata$z) * 100
  if (flt_minht > 1) {log[p,7] <- 1; log[p,8] <- flt_minht} #More than 1% of normalized heights < minht m
  
  #get point density
  ptden[p] <- nrow(lasdata)/(tilesize^2)
  if (ptden[p] < min_ptden) {log[p,3] <- 1; log[p,4] <- ptden[p]}
  
  #move to next tile if there is no ground or vegetation return, or if more than 1% of the normalized heights are outliers
  if (sum(log[p, c(1,2,5,7)]) >= 1) next()
  		
  #remove points considered outliers in tiles that passed the 1st quality check
  lasdata <- lasdata[lasdata$z >= minht & lasdata$z <= maxht,]
  		
  #tile quality check after removal of outliers
  if ((max(lasdata$z)-min(lasdata$z)) < minhtamp) {log[p,9] <- 1; log[p,10] <- max(lasdata$z)-min(lasdata$z)} #Maximum height < minhtamp m
  
  #move to next tile if height amp < minhtamp
  if (log[p,9] == 1) next()
  
  
  #get profile and find ground peak and top of ground return
  profile <- makeprof(lasdata, binsize) 
  #gmax0 <- max(lasdata$z[lasdata$c==2], na.rm=T) - min(lasdata$z) #doesn't work... can't trust ground classification
  gmax0 <- ceiling(0.25*max(profile$height)) #find ground peak below 25% of max profile height
  #profile$counts <- profile$counts/max(profile$counts[profile$height > gmax0]) #normalize lidar profile by maximum canopy return
  gcounts <- profile$counts[1:which(profile$height==gmax0)]
  gpeaks <- array(0, length(gcounts))
	if (length(gpeaks) >= 3) {
		for (i in 2:(length(gpeaks)-1)) if (gcounts[i] > gcounts[i-1] & gcounts[i] > gcounts[i+1]) gpeaks[i] <- 1 #find peaks below gmax0
	}
	if (sum(gpeaks) == 0) {
		peakid <- which(gcounts == max(gcounts))[1]
	} else {
		peakid <- which(gpeaks==1)[1]
		if (gcounts[peakid] <= max(gcounts[1:(peakid-1)])) peakid <- which(gcounts == max(gcounts[1:(peakid-1)]))[1]
	}
  cdif <- profile$counts[peakid:which(profile$height==gmax0)] - profile$counts[(peakid+1):(which(profile$height==gmax0)+1)] #find top of ground return (up to 25% of max profile height)
  gtopid <- peakid + which(cdif <= 0)[1] -1
  if (is.na(gtopid)) gtopid <- peakid + which(cdif == min(cdif, na.rm=T))[1] -1

  #save profile if required
  if (saveprof == "Y") {
    profname <- paste0(profdir, unlist(strsplit(basename(lasfiles[p]), "\\."))[1], "_profile.csv")
    write.csv(profile, file=profname, quote = F, row.names=F)
  }
  
  #plot full profile in pdf ---------------WHY PLOTTING FULL PROFILE INSTEAD OF TRIMMED?
  if (p %in% seq(1,length(lasfiles),4)) par(mfrow=c(2,2), oma=c(2.5,2.5,.5,.5), mar=c(1,1,.5,.5), mgp=c(1.5, .3, 0), tcl=-0.3) #c(bottom, left, top, right)
  plot(profile$counts, profile$height, xlab="", ylab="", type="n")
  abline(h=profile$height[gtopid], lty=1, col="gray80")
  abline(h=profile$height[peakid], lty=2, col="red")
  lines(profile$counts, profile$height, col="gray30")
  legend("topright", lasfiles[p], bty="n") 
  if (p %in% seq(1,length(lasfiles),4)) {
	mtext("Counts", 1, outer=T, line=1.2)
	mtext("Height (m)", 2, outer=T, line=1.2)
  }
  
  #get ground and canopy counts before trimming the profile
  #genergy <- sum(profile$counts[profile$height <= gmax0])
  genergy <- sum(profile$counts[profile$height <= profile$height[gtopid]])
  tenergy <- sum(profile$counts)
  
  #make ground peak = 0 m
  profile <- profile[peakid:nrow(profile),] #trim profile
  #gmax0c <- gmax0 - min(profile$height) #correct max ground height
  gmax0c <- profile$height[gtopid] - min(profile$height)
  profile$height <- profile$height - min(profile$height) #correct heights
		
  ##################################
  
  #extract metrics from profile
  prof_a <- profile$counts
  prof_h <- profile$height
  
    #Fourier
      nh <- length(prof_h)
      integ <- array(NA, nh) 
	  for (j in 1:nf) { #loop over frequencies
  		 for (i in 1:nh) integ[i] <- exp(2 * pi * 1i * freq[j] * prof_h[i]) * prof_a[i] #loop over height bins to calc fourier integrand
  		 fourier <- sum(integ) / sum(prof_a)
  		 amp[p,j] <- Mod(fourier) #amplitude
  		 pha[p,j] <- Arg(fourier) #wrapped phase
	  }
    
    #traditional
      trad[p,1] <- sum(prof_h*prof_a)/sum(prof_a) #meanh
      trad[p,2] <-  sqrt(sum(prof_h^2*prof_a)/sum(prof_a) - trad[p,1]^2) #stdevh
      #
      cumprof <- cumsum(prof_a)/sum(prof_a)
      trad[p,3] <- prof_h[which(abs(cumprof-0.25) == min(abs(cumprof-0.25)))[1]] #h25
      trad[p,4] <- prof_h[which(abs(cumprof-0.50) == min(abs(cumprof-0.50)))[1]] #h50
      trad[p,5] <- prof_h[which(abs(cumprof-0.75) == min(abs(cumprof-0.75)))[1]] #h75
      trad[p,6] <- max(prof_h) #mch or h100
      #
      if (length(prof_a) >= 3) {
  			peaks <- array(0, length(prof_a))
  			for (i in 2:(length(peaks)-1)) if (prof_a[i] > prof_a[i-1] & prof_a[i] > prof_a[i+1]) peaks[i] <- 1
  			trad[p,7] <- sum(peaks) #npeaks
          #
  			ap1 <- prof_a[seq(1, length(prof_a), 2)]
  			ap2 <- prof_a[seq(2, length(prof_a), 2)]
			if (length(ap1) == length(ap2)) ent_a <- ap1 + ap2 else ent_a <- ap1 + c(ap2,0)
			ent_a[ent_a == 0] <- 0.000001
			trad[p,8] <- -sum((ent_a/sum(ent_a)) * log(ent_a/sum(ent_a))) #ent
        #
			dummyid <- which(prof_a == max(prof_a[prof_h > gmax0c]))
			if (length(dummyid) > 1) dummyid <- dummyid[length(dummyid)]
			oc_relief <- max(prof_h) - prof_h[dummyid]
			trad[p,9] <- max(prof_a[prof_h > gmax0c]) / oc_relief #fslope
        #
			trad[p,10] <- genergy/tenergy #g2t
	}
}

#close pdf
dev.off()

#unwrap phase
pha_unw <- as.data.frame(pha/NA)
for (pp in 1:nrow(pha)) {
  pha_unw[pp,] <- unwrap(as.numeric(pha[pp,]))
}

#remove extra freqs
freq30 <- seq(.01, .3, by=.01)
freqid <- match(freq30, freq)
pha_unw <- pha_unw[,freqid]
amp <- amp[,freqid]
colnames(pha_unw) <- sub("0.", ".", paste0("pha", format(freq30, digits=2)))
colnames(amp) <- sub("0.", ".", paste0("amp", format(freq30, digits=2)))

#save results
setwd(indir)
save(lasfiles, xcor, ycor, ptden, trad, amp, pha_unw, log, file="R_project.RData")
write.csv(cbind(lasfiles, xcor, ycor, ptden, trad, amp, pha_unw), file="metrics.csv", row.names=F)
write.csv(cbind(lasfiles, log), file="log.csv", row.names=F)

