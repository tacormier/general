require(signal)
require(raster)

rasterOptions(tmpdir="/home/tcormier/RasTmpDir/")
source("/mnt/a/tcormier/scripts/general/R/handy_functions_TC.R")

#######################################################################
# Variables
#indir <- "/mnt/a/tcormier/SE_Asia/Ellis_Paper/model_training/lidar_in_plots/txt/"
indir <- "/mnt/a/tcormier/SE_Asia/Ellis_Paper/lidar/Sumalindo_west/Tiles_20m/"
# indir <- "/mnt/a/tcormier/testing/"
tilesize <- 20 #in meters
binsize <- 0.5 #vertical resolution of profile (m)
min_ptden <- 6 #min CartoData = 8 ppm2; min SL Brazil = 4 ppm2
vegflag <- "T" #(T/F): vegetation returns classified?
# Values for flagging bad points or tiles - based on field data or expert knowledge
max.height <- 82
min.height <- -3
dtm.file <- "/mnt/a/fgoncalves/Mexico/dtm_mosaic.tif"
#dtm.file <- "/mnt/a/tcormier/SE_Asia/Ellis_Paper/lidar/DTM_all/DTM_Ellis/se_asia_dtm.vrt"
# dtm.file <- "/Volumes/share-2/fgoncalves/Mexico/dtm_mosaic.tif"
#######################################################################
dtm <- raster(dtm.file)


#get filenames and coordinates (lower-left corner of the tiles)
lasfiles <- list.files(indir, pattern="\\.txt", full.names=T)
dummy <- sub(".txt", "", basename(lasfiles))
#dummy <- basename(dummy)
dummy <- lapply(strsplit(dummy, "_"), rev)
xcor <- as.numeric(sapply(dummy, "[", 2))
ycor <- as.numeric(sapply(dummy, "[", 1))
# plot(xcor,ycor)

#process each tile
# if xcor and ycor are not in file names
# xcor <- vector()
# ycor <- vector()
  
#define metrics, etc
dummyn <- paste0("ptden_lt", min_ptden)
log <- array(0, c(length(lasfiles), 11))
colnames(log) <- c("no_ground", "no_veg", "wlen_gt60", "wlen", "ground_lt_binsize", dummyn, "ptden", "fgt_gt1", "fgt", "fltm_gt1", "fltm")
ptden <- array(NA, length(lasfiles))
freq <- seq(0.01, 0.3, by=0.001)
nf <- length(freq)
amp <- array(NA, c(length(lasfiles), nf))
  colnames(amp) <- gsub("p0.", "p.", paste0("amp", format(freq, digits=2)))
pha <- array(NA, c(length(lasfiles), nf))
  colnames(pha) <- gsub("a0.", "a.", paste0("pha", format(freq, digits=2)))
trad <- array(NA, c(length(lasfiles), 10))
  colnames(trad) <- c("meanh", "stdevh", "h25", "h50", "h75", "mch", "npeaks", "ent", "fslope", "g2t")

#loop
for (p in 1:length(lasfiles)) {
  print(paste0("calculating metrics for file ", p, " of ", length(lasfiles)))
  lasdata <- read.csv(lasfiles[p], header=F, col.names=c("x","y","z","i","a","n","r","c"))
  # This xcor/ycor calc is only if that info is not in the lasfile file name
#   xcor[p] <- min(lasdata$x)
#   ycor[p] <- min(lasdata$y)

  #tile quality check
  if (!(2 %in% lasdata$c)) {log[p,1] <- 1} #warning("No ground return")
  dummy <- c(3,4,5) %in% lasdata$c
  if (vegflag == "T") {
    if (sum(dummy+0) == 0) {log[p,2] <- 1} #warning("No vegetation return")
  }
  wlen <- max(lasdata$z-min(lasdata$z))
  if (wlen > 60) {log[p,3] <- 1; log[p,4] <- wlen} #warning(paste0("Waveform length = ", wlen, " m"))
  
  #get point density
  ptden[p] <- nrow(lasdata)/(tilesize^2)
  if (ptden[p] < min_ptden) {log[p,6] <- 1; log[p,7] <- ptden[p]}
  
  #get profile and make ground peak = 0 m
  makeprofile <- makeprof(lasdata, binsize, dtm, max.height, min.height)
  profile <- makeprofile[[1]]
  gmax0c <- makeprofile[[2]]
  genergy <- makeprofile[[3]]
  tenergy <- makeprofile[[4]]
  fgt <- makeprofile[[5]]
  fltm <- makeprofile[[6]]
  log.val5 <- makeprofile[[7]]
  ##################################
  log[p,5] <- log.val5
  
  # More tile quality flagging now that profile is completed:
  if (fgt > 1) {log[p,8] <- 1; log[p,9] <- fgt} #warning("More than 1% of normalized heights > 82 m")
  if (fltm > 1) {log[p,10] <- 1; log[p,11] <- fltm} #warning("More than 1% of normalized heights < -3 m")
  
  # move to next tile if there is no ground or vegetation return, or 
  # if more than 1% of the normalizd heights are outliers
  if (sum(log[p, c(1,2,8,10)]) >= 1) next()
  
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
  			for (i in 2:(length(peaks)-1)) if (prof_a[i] > prof_a[i-1] & prof_a[i] > prof_a[i+1]) peaks[i] <- 1 else peaks[i] <- 0
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

#unwrap phase
pha_unw <- pha/NA
for (pp in 1:nrow(pha)) pha_unw[pp,] <- unwrap(as.numeric(pha[pp,]))

#remove extra freqs
freq30 <- seq(.01, .3, by=.01)
freqid <- match(freq30, freq)
pha_unw <- pha_unw[,freqid]
amp <- amp[,freqid]
colnames(pha_unw) <- sub("0.", ".", paste0("pha", format(freq30, digits=2)))
colnames(amp) <- sub("0.", ".", paste0("amp", format(freq30, digits=2)))

#   #extract and check phase heights
#   phaseh_unw <- pha_unw / NA
#   #for (i in 1:30) phaseh_unw[,i] <- pha_unw[,i] / (2*pi*freq30[i]) #line below is better
#   phaseh_unw <- sweep(pha_unw , 2, (2*pi*freq30), `/`)
#   phaseh_max <- array(NA, 30)
#   phaseh_min <- array(NA, 30)
#   phaseh_mean <- array(NA, 30)
#   phaseh_ranges <- array(NA, 30)
#   for (i in 1:30) {
#     phaseh_mean[i] <- mean(phaseh_unw[,i])
#     phaseh_max[i] <- max(phaseh_unw[,i])
#     phaseh_min[i] <- min(phaseh_unw[,i])
#     phaseh_ranges[i] <- max(phaseh_unw[,i]) - min(phaseh_unw[,i])
#   }
#   par(mfrow=c(2,2))
#   plot(freq30, phaseh_max, xlab="Frequency (cyc/m)", ylab="Maximum phase height (m)")
#   plot(freq30, phaseh_mean, xlab="Frequency (cyc/m)", ylab="Mean phase height (m)")
#   plot(freq30, phaseh_min, xlab="Frequency (cyc/m)", ylab="Minimum phase height (m)")
#   plot(freq30, phaseh_ranges, xlab="Frequency (cyc/m)", ylab="Phase height range(m)")


#save results
setwd(indir)
save(lasfiles, xcor, ycor, ptden, trad, amp, pha_unw, log, file="R_project.RData")
write.csv(cbind(lasfiles, xcor, ycor, ptden, trad, amp, pha_unw), file="metrics.csv", row.names=F)
write.csv(cbind(lasfiles, log), file="log.csv", row.names=F)


#check log file
log <- data.frame(log)
if (sum(log$no_ground[log$no_ground==1]) > 0) {
  warning(paste0(length(log$no_ground[log$no_ground==1]), " tiles with no ground return"), immediate.=T)
  cat(paste(lasfiles[log$no_ground==1],"\n"))
}
if (vegflag == "T") {
  if (sum(log$no_veg[log$no_veg==1]) > 0) {
    warning(paste0(length(log$no_veg[log$no_veg==1]), " tiles with no vegetation return:"), immediate.=T)
    cat(paste(lasfiles[log$no_veg==1],"\n"))
  }
}
if (sum(log$wlen_gt60[log$wlen_gt60==1]) > 0) {
  warning(paste0(length(log$wlen_gt60[log$wlen_gt60==1]), " tiles with waveform length > 60 m"), immediate.=T)
  cat(paste(lasfiles[log$wlen_gt60==1],"=>", log$wlen[log$wlen_gt60==1], "m", "\n"), sep="")
}
if (sum(log$ground_lt_binsize[log$ground_lt_binsize==1]) > 0) {
  warning(paste0(length(log$ground_lt_binsize[log$ground_lt_binsize==1]), " tiles with ground height < bin size"), immediate.=T)
  cat(paste(lasfiles[log$ground_lt_binsize==1],"\n"))
}
if (sum(log[,6][log[,6]==1]) > 0) {
  warning(paste0(length(log[,6][log[,6]==1]), " tiles with point density < ", min_ptden), immediate.=T)
  cat(paste(lasfiles[log[,6]==1],"=>", log$ptden[log[,6]==1], "pts/m2", "\n"), sep="")
}



  
  