# This code calculates metrics on lidar files. Currently calculates Fabio's traditional and fourier metrics.
# In the future, I'll add the option to also compute FUSION metrics using the met.type flag that is already
# implemented in the wrapper script.
#
# Expects the following variables to be passed in an rdata file: lf, out.mets, binsize, saveprof, profname, 
# saveWave, wavename, l.format, ht.col, int.col, met.type, tmpdir

require(signal)
require(raster)
require(lidR)

rasterOptions(tmpdir="/home/tcormier/RasTmpDir/")
source("/mnt/a/tcormier/scripts/general/R/handy_functions_TC.R")

#######################################################################
# rdata file for testing
# vars <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170314/metrics_profiles_waveforms/20170404/rdata/TCID_99_CMS_FieldPolygons_updatedFields_20170228_UTM16_Cartodata_Campeche_Yucatan_Q1_UTM16N_groundIndex_metrics_vars.RDATA"
# Get command line arguments 
Args <- commandArgs(trailingOnly=TRUE)
print(Args)

vars <- Args[1]
load(vars)
#######################################################################
# For testing
# lf <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170314/norm_fil/TCID_652_CMS_FieldPolygons_updatedFields_20170228_UTM16_Cartodata_Campeche_Yucatan_Q3_UTM16N_groundIndex.las"    
# out.mets <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170314/metrics_profiles_waveforms/20170404/metrics/CMS_metrics_fourier_20170404_test1.csv"
# binsize=0.5
# saveprof <- 'T'
# saveWave <- 'T'
# profname <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170314/metrics_profiles_waveforms/20170404//profiles/TCID_1_CMS_FieldPolygons_updatedFields_20170228_UTM14_G-LiHT_Hildalgo_May2013_bak_UTM14N_groundIndex_profile.csv"
# wavename <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170314/metrics_profiles_waveforms/20170404//waveforms/TCID_1_CMS_FieldPolygons_updatedFields_20170228_UTM14_G-LiHT_Hildalgo_May2013_bak_UTM14N_groundIndex_waveform.pdf"
# l.format <- 'las'
# ht.col <- "Z"
# int.col <- "Intensity"
  
#define metrics, etc
freq <- seq(0.01, 0.3, by=0.001) #maybe use 0.0001 to make sure we have no phase jumps? Depends on processing time. If 0.0001 is used, changes are needed below
nf <- length(freq)
amp <- as.data.frame(array(NA, c(1, nf)))
colnames(amp) <- gsub("p0.", "p.", paste0("amp", format(freq, digits=2)))

pha <- array(NA, c(1, nf))
colnames(pha) <- gsub("a0.", "a.", paste0("pha", format(freq, digits=2)))

trad <- array(NA, c(1, 10))
colnames(trad) <- c("meanh", "stdevh", "h25", "h50", "h75", "mch", "npeaks", "ent", "fslope", "g2t")

print(paste0("calculating metrics for: ", basename(lf)))
# only want to read the lidar file if we have to - for bigger files, this is one of the time hogs.
if (met.type == 'both' | met.type == 'prof-based' | saveprof == 'Y') {
  if (l.format == "las") {
    las <- readLAS(lf)
    lasdata <- las@data
    } else if (l.format == "txt") {
      lasdata <- read.csv(lasfiles[p], header=F, col.names=c("x","y","z","i","a","n","r","c"))
    }
  #if lasdata is empty, stop with error
  if (nrow(lasdata)==0) {
    stop(paste0(lf, " has no data."))
  }
} # end if/else determining if we need to read the lidar file or not!

# calculate profile and save if applicable.
if (saveprof == "Y" | met.type == 'prof-based' | met.type == 'both') { 
  #get profile and find ground peak and top of ground return
  profile <- makeprof(lasdata, binsize, ht.col, int.col) 
  #save profile if required
  if (saveprof == "Y") {
    write.csv(profile, file=profname, quote = F, row.names=F)
  }
  
  # Find ground peak below 25% of max profile height
  #gmax0 <- max(lasdata$z[lasdata$c==2], na.rm=T) - min(lasdata$z) #doesn't work... can't trust ground classification. Leaving it in for legacy.
  gmax0 <- ceiling(0.20*max(profile$height)) # changed to 20% per Fabio's email on 4/10/2017
  # All ground counts beneath gmax0
  gcounts <- profile$counts[1:which(profile$height==gmax0)]
  
  # Identify the ground peak
  gpeaks <- array(0, length(gcounts))
  if (length(gpeaks) >= 3) {
    # Find peaks below gmax0 and ID them with a 1
    for (i in 2:(length(gpeaks)-1)) if (gcounts[i] > gcounts[i-1] & gcounts[i] > gcounts[i+1]) gpeaks[i] <- 1 
  }
  # If length of gpeaks is less than 3, or if no clear ground peaks identified in for loop above, take 1st max:
  if (sum(gpeaks) == 0) {
    peakid <- which(gcounts == max(gcounts))[1]
  } else {
    # Otherwise, identify the FIRST peak below 25% of the max profile height as the ground peak
    peakid <- which(gpeaks==1)[1]
    # In case there is a higher count beneath peakid - if so, change peakid to this position.
    if (gcounts[peakid] <= max(gcounts[1:(peakid-1)])) peakid <- which(gcounts == max(gcounts[1:(peakid-1)]))[1]
  }
  
  # find top of ground return (up to 25% of max profile height)
  cdif <- profile$counts[peakid:which(profile$height==gmax0)] - profile$counts[(peakid+1):(which(profile$height==gmax0)+1)] 
  gtopid <- peakid + which(cdif <= 0)[1] -1
  
  if (is.na(gtopid)) gtopid <- peakid + which(cdif == min(cdif, na.rm=T))[1] -1
  
  #plot full profile in pdf - quick and less info than ggplot version for field plots
  if (saveWave == 'Y') {
    #initiate pdf to save profile figs.
    pdf(file=wavename, width=7, height=7)
    # if (p %in% seq(1,length(lasfiles),4)) par(mfrow=c(2,2), oma=c(2.5,2.5,.5,.5), mar=c(1,1,.5,.5), mgp=c(1.5, .3, 0), tcl=-0.3) #c(bottom, left, top, right)
    plot(profile$counts, profile$height, xlab="Counts", ylab="Height", type="n")
    abline(h=profile$height[gtopid], lty=1, col="gray80")
    abline(h=profile$height[peakid], lty=2, col="red")
    lines(profile$counts, profile$height, col="gray30")
    legend("topright", basename(lf), bty="n", cex=0.6) 
    
    dev.off()
  }
} # end profile generation if applicable

if (met.type == "prof-based" | met.type == "both") {
  
  #get ground and canopy counts before trimming the profile
  #genergy <- sum(profile$counts[profile$height <= gmax0])
  genergy <- sum(profile$counts[profile$height <= profile$height[gtopid]])
  tenergy <- sum(profile$counts)
  
  #make ground peak = 0 m
  gmax0c <- profile$height[gtopid] - min(profile$height[peakid:nrow(profile)]) #corrected max ground height
  profile <- profile[peakid:nrow(profile),] #trim profile
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
  	 amp[1,j] <- Mod(fourier) #amplitude
  	 pha[1,j] <- Arg(fourier) #wrapped phase
  }
      
  #traditional
  trad[1,1] <- sum(prof_h*prof_a)/sum(prof_a) #meanh
  trad[1,2] <-  sqrt(sum(prof_h^2*prof_a)/sum(prof_a) - trad[1,1]^2) #stdevh
  #
  cumprof <- cumsum(prof_a)/sum(prof_a)
  trad[1,3] <- prof_h[which(abs(cumprof-0.25) == min(abs(cumprof-0.25)))[1]] #h25
  trad[1,4] <- prof_h[which(abs(cumprof-0.50) == min(abs(cumprof-0.50)))[1]] #h50
  trad[1,5] <- prof_h[which(abs(cumprof-0.75) == min(abs(cumprof-0.75)))[1]] #h75
  trad[1,6] <- max(prof_h) #mch or h100
  #
  if (length(prof_a) >= 3) {
  	peaks <- array(0, length(prof_a))
  	for (i in 2:(length(peaks)-1)) if (prof_a[i] > prof_a[i-1] & prof_a[i] > prof_a[i+1]) peaks[i] <- 1
  	trad[1,7] <- sum(peaks) #npeaks
      #
  	ap1 <- prof_a[seq(1, length(prof_a), 2)]
  	ap2 <- prof_a[seq(2, length(prof_a), 2)]
  	if (length(ap1) == length(ap2)) ent_a <- ap1 + ap2 else ent_a <- ap1 + c(ap2,0)
  	ent_a[ent_a == 0] <- 0.000001
  	trad[1,8] <- -sum((ent_a/sum(ent_a)) * log(ent_a/sum(ent_a))) #ent
      #
  	dummyid <- which(prof_a == max(prof_a[prof_h > gmax0c]))
  	if (length(dummyid) > 1) dummyid <- dummyid[length(dummyid)]
  	oc_relief <- max(prof_h) - prof_h[dummyid]
  	trad[1,9] <- max(prof_a[prof_h > gmax0c]) / oc_relief #fslope
      #
  	trad[1,10] <- genergy/tenergy #g2t
  }
  
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
  
} 

if (met.type == "fusion" | met.type == "both") { 
  if (met.type == "fusion") {
    out.metrics <- out.mets
  } else if (met.type == "both") {
    f.outmetrics <- paste0(tmpdir, unlist(strsplit(basename(lf), "\\."))[1], "_fusion.csv")
  }
  
  # Reformat lf for windows tool.
  lf.reform <- paste0(tmpdir, unlist(strsplit(basename(lf), "\\."))[1], "_laslist_wine.txt")
  
  # perform the reformatting and write out new file
  tbl.ref <- gsub("/mnt", "Z:/mnt", lf)
  tbl.ref <- gsub("/", "\\\\\\\\", tbl.ref)
  # one little hack-fix in case we end up with 8 backslashes due to a double fwd slash (I know...ridiculous)
  tbl.ref <- gsub("\\\\\\\\\\\\\\\\", "\\\\\\\\", tbl.ref)
  
  write.table(tbl.ref, lf.reform, quote = F, row.names = F, col.names = F)
  
  
  lf.reform_txt <- gsub("/mnt", "Z:/mnt", lf.reform)
  lf.reform_txt <- gsub("/", "\\\\\\\\", lf.reform_txt)
  lf.reform_txt <- gsub("\\\\\\\\\\\\\\\\", "\\\\\\\\", lf.reform_txt)
  f.outmetrics.reform <- gsub("/mnt", "Z:/mnt", f.outmetrics)
  f.outmetrics.reform <- gsub("/", "\\\\\\\\", f.outmetrics.reform)
  f.outmetrics.reform <- gsub("\\\\\\\\\\\\\\\\", "\\\\\\\\", f.outmetrics.reform)
  
  # Submit to fusion to calc metrics
  cloudmet.cmd <- paste0("/usr/bin/wine \\\\mnt\\\\a\\\\tcormier\\\\FUSION2\\\\cloudmetrics.exe /minht:1.5 /above:3 /new ", lf.reform_txt, " ", f.outmetrics.reform)
  system(cloudmet.cmd)
  
  # end fusion metrics.
} 

if (met.type != "fusion" & met.type != "prof-based" & met.type != "both") {
  stop(paste0("Invalid met.type option. Possible types are 'prof-based', 'fusion', or 'both.' You entered ", met.type))
  } # end met.type if/else

# ***** Combine results (if applicable) and save to one table *****
# if met.type is fusion, the file already saved out as out.mets, but we want to fix the
# weird field names and re-write it out.
if (met.type =='fusion') {
  f.metrics <- read.csv(f.outmetrics)
  names.orig <- names(f.metrics)
  # Fix dumb field names
  name.fix <- gsub("\\.", "_", names.orig)
  name.fix1 <- gsub("X_", "", name.fix)
  name.fix2 <- gsub("_____", "/", name.fix1)
  name.fix3 <- gsub("____", "_times_", name.fix2)
  
  names(f.metrics) <- paste0("fusion_", name.fix3)
  
  # Fix file paths:
  f.metrics$fusion_DataFile <- gsub("Z:", "", f.metrics$fusion_DataFile)
  f.metrics$fusion_DataFile <- gsub("\\\\\\\\", "/", f.metrics$fusion_DataFile)
  
  write.csv(f.metrics, file=out.mets, row.names=F, quote=F)
  
} else if (met.type == 'prof-based') {
  #save results
  prof.metrics <- cbind.data.frame(lf, trad, amp, pha_unw)
  names(prof.metrics) <- paste0("prof-based_", names(prof.metrics))
  write.csv(prof.metrics, file=out.mets, row.names=F, quote=F)
  
} else if (met.type == 'both') {
  prof.metrics <- cbind.data.frame(lf, trad, amp, pha_unw)
  names(prof.metrics) <- paste0("prof-based_", names(prof.metrics))
  
  f.metrics <- read.csv(f.outmetrics)
  names.orig <- names(f.metrics)
  # Fix dumb field names
  name.fix <- gsub("\\.", "_", names.orig)
  name.fix1 <- gsub("X_", "", name.fix)
  name.fix2 <- gsub("_____", "/", name.fix1)
  name.fix3 <- gsub("____", "_times_", name.fix2)
  
  names(f.metrics) <- paste0("fusion_", name.fix3)
  
  # Fix file paths:
  f.metrics$fusion_DataFile <- gsub("Z:", "", f.metrics$fusion_DataFile)
  f.metrics$fusion_DataFile <- gsub("\\\\\\\\", "/", f.metrics$fusion_DataFile)
  
  # Put tables together in harmony
  mets.final <- cbind.data.frame(prof.metrics, f.metrics)
  write.csv(mets.final, file=out.mets, row.names=F, quote=F)
}





