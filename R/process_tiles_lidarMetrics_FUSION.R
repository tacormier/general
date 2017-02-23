# This logs bad tiles, but doesn't exclude them from the metrics because submitting the
# metrics happens outside of the loop. Would be an easy fix, but pressed for time :/
# Need to remove problem tiles from modeling tables after they process. FYI for now!

# This script expects to receive the following variables as part of the RDATA files submitted as
# and argument from process_tiles_lidarMetrics_FUSION_qsubWrapper.R: 
# path.master, lasfiles, dtm.file, minht, min_ptden, vegflag, tilesize, binsize, 
# profdir, synthProf, saveprof, saveWave, out.txt, out.metrics, reform,
require(raster)

rasterOptions(tmpdir="/home/tcormier/RasTmpDir/")
source("/mnt/a/tcormier/scripts/general/R/handy_functions_TC.R")

#######################################################################
# rdata file for testing
# vars <- "/mnt/r/Mex_Lidar/G_LiHT/Hildalgo_May2013_bak/lidar/las/Tiles_30m_metrics_vars_FUSION.RDATA"
# Variables
# Get command line arguments 
Args <- commandArgs(trailingOnly=TRUE)
print(Args)

vars <- Args[1]
load(vars)
#######################################################################
dtm <- raster(dtm.file)

# Tina's log that tracks whether a file was skipped (due to failing a filter or QA check) or processed.
# The total number of entries in this log should = the number of lasfiles 
tc.log <- as.data.frame(matrix(data=NA, ncol=2, nrow=length(lasfiles)))
names(tc.log) <- c("file", "status")
tc.log.file <- paste0(dirname(out.metrics),"/metrics_log_FUSION_TC.csv")
# unlink(tc.log.file)
write.table(paste0("file,", "status"), file=tc.log.file, row.names=F, quote=F, sep=",", col.names=F)

# And another log to track errors in txt2las - these files did not convert to las - check into it!
# on second thought - just write status to tc.log
# txt.log <- vector()
# txt.log.file <- paste0(dirname(out.metrics),"/metrics_log_txt2las_ERRORS.csv")
# unlink(txt.log.file)

# Setting up some logs
dummyn1 <- paste0("ptden_lt", min_ptden)
dummyn2 <- paste0("fgt_gt1")
dummyn3 <- paste0("fgt")
dummyn4 <- paste0("flt_minus", abs(minht), "m_gt1")
dummyn5 <- paste0("flt_minus", abs(minht), "m")
# dummyn6 <- paste0("htamp_lt_", minhtamp, "m")
log <- array(0, c(length(lasfiles), 11))
# We aren't using all of these, so I should clean this up later - for right now, I just want
# to get metrics running and for things to write into the correct columns
colnames(log) <- c("no_ground", "no_veg", dummyn1, "ptden", dummyn2, dummyn3, dummyn4, dummyn5, "dummyn6", "htamp", "nodata")
ptden <- array(NA, length(lasfiles))

#initiate pdf to save profile figs
if (saveWave == 'Y') pdf(file=paste0(profdir, '/profiles.pdf'), width=7, height=7)

# Folder for normalized lasfiles
outlasnorm.dir <- paste0(path.master, "Tiles_30m_norm/")
dir.create(outlasnorm.dir, showWarnings = F)

# loop
p1 <- proc.time()

# vector to hold paths to normalized las files (so I can avoid list.files on many thousands of files)
lasnormlist <- vector()

for (p in 1:length(lasfiles)) {
  print(paste0("calculating metrics for file ", p, " of ", length(lasfiles)))
  tc.log$file[p] <- lasfiles[p]
  lasdata <- read.csv(lasfiles[p], header=F, col.names=c("x","y","z","i","a","n","r","c"))
  
  #if lasdata is empty, jump to next
  if (nrow(lasdata)==0) {
    tc.log$status[p] <- "failed_QA"
    write.table(tc.log[p,], file=tc.log.file, row.names=F, quote=F, append = T, sep=",", col.names=F)
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
  
  #if lasdata is empty, jump to next (TC added this again here based on some G-LiHT errors)
  if (nrow(lasdata)==0) {
    tc.log$status[p] <- "failed_QA"
    write.table(tc.log[p,], file=tc.log.file, row.names=F, quote=F, append = T, sep=",", col.names=F)
    next()
  }
  
  #tile quality check
  if (!(2 %in% lasdata$c)) {log[p,1] <- 1} #No ground return
  dummy <- c(3,4,5) %in% lasdata$c
  if (vegflag == "T") {
    if (sum(dummy+0) == 0) {log[p,2] <- 1} #No vegetation return
  }
  
  maxht <- quantile(lasdata$z, c(0.99))
  fgt_maxht <- length(lasdata$z[lasdata$z > maxht]) / length(lasdata$z) * 100
  if (fgt_maxht > 2) {log[p,5] <- 1; log[p,6] <- fgt_maxht} #More than 2% of normalized heights > maxht m
  flt_minht <- length(lasdata$z[lasdata$z < minht]) / length(lasdata$z) * 100
  if (flt_minht > 2) {log[p,7] <- 1; log[p,8] <- flt_minht} #More than 2% of normalized heights < minht m
  
  #get point density
  ptden[p] <- nrow(lasdata)/(tilesize^2)
  if (ptden[p] < min_ptden) {log[p,3] <- 1; log[p,4] <- ptden[p]}
  
  #move to next tile if there is no ground or vegetation return, or if more than 2% of the normalized heights are outliers
  if (sum(log[p, c(1,2,5,7)]) >= 1) {
    tc.log$status[p] <- "failed_QA"
    write.table(tc.log[p,], file=tc.log.file, row.names=F, quote=F, append = T, sep=",", col.names=F)
    next()
  }
  
  #remove points considered outliers in tiles that passed the 1st quality check
  lasdata <- lasdata[lasdata$z >= minht & lasdata$z <= maxht,]
  
  #tile quality check after removal of outliers - FOR FUSION, WE USE MINHT=1.5, so we don't need this part.
#   if ((max(lasdata$z)-min(lasdata$z)) < minhtamp) {log[p,9] <- 1; log[p,10] <- max(lasdata$z)-min(lasdata$z)} #Maximum height < minhtamp m
#   
#   #move to next tile if height amp < minhtamp
#   if (log[p,9] == 1) next()
  
  if (synthProf == 'Y') {
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
    
    #plot full profile in pdf
    if (saveWave == 'Y') {
      if (p %in% seq(1,length(lasfiles),4)) par(mfrow=c(2,2), oma=c(2.5,2.5,.5,.5), mar=c(1,1,.5,.5), mgp=c(1.5, .3, 0), tcl=-0.3) #c(bottom, left, top, right)
      plot(profile$counts, profile$height, xlab="", ylab="", type="n")
      abline(h=profile$height[gtopid], lty=1, col="gray80")
      abline(h=profile$height[peakid], lty=2, col="red")
      lines(profile$counts, profile$height, col="gray30")
      legend("topright", basename(lasfiles[p]), bty="n") 
      if (p %in% seq(1,length(lasfiles),4)) {
        mtext("Counts", 1, outer=T, line=1.2)
        mtext("Height (m)", 2, outer=T, line=1.2)
      }
    }
  }# End synthProf if
  
  #save normalized, filtered LAS
  lasdata$z <- round(lasdata$z, digits=4)
  outtxtnorm <- paste0(outlasnorm.dir, basename(lasfiles[p]))
  write.table(lasdata, file=outtxtnorm, sep=",", row.names=F, col.names=F, quote=F)
  
  # convert txt to las for FUSION use LAStools
  outlasnorm <- paste0(outlasnorm.dir, unlist(strsplit(basename(lasfiles[p]), "\\."))[1], ".las")
  txt2las.cmd <- paste0("/mnt/a/tcormier/LAStools/bin/txt2las.exe -parse xyzianrc -i ", outtxtnorm, " -o ", outlasnorm)
  system(txt2las.cmd)
  
  if (file.exists(outlasnorm)) {
    lasnormlist[p] <- outlasnorm
    tc.log$status[p] <- "processed_to_las"
  } else {
    tc.log$status[p] <- "txt2las_failed"
  }
  write.table(tc.log[p,], file=tc.log.file, row.names=F, quote=F, append = T, sep=",", col.names=F)
  
}# End plot loop

#close pdf
if (saveWave == 'Y') dev.off()

# Reformat table for windows tool?
if (reform == "Y") {
  # tbl <- list.files(outlasnorm.dir, "*.las$", full.names=T)
  lasfiles_reform <- paste0(stripExt(out.txt), "_wine.txt")
  
  # perform the reformatting and write out new file
  tbl.ref <- gsub("/mnt", "Z:/mnt", lasnormlist)
  tbl.ref <- gsub("/", "\\\\\\\\", tbl.ref)
  
  write.table(tbl.ref, lasfiles_reform, quote = F, row.names = F, col.names = F)
}# end reform if


lasfiles_reform_txt <- gsub("/mnt", "Z:/mnt", lasfiles_reform)
lasfiles_reform_txt <- gsub("/", "\\\\\\\\", lasfiles_reform_txt)
out.metrics.reform <- gsub("/mnt", "Z:/mnt", out.metrics)
out.metrics.reform <- gsub("/", "\\\\\\\\", out.metrics.reform)

# Submit to fusion to calc metrics
cloudmet.cmd <- paste0("/usr/bin/wine \\\\mnt\\\\a\\\\tcormier\\\\FUSION2\\\\cloudmetrics.exe /minht:1.5 /above:3 /new ", lasfiles_reform_txt, " ", out.metrics.reform)
system(cloudmet.cmd)
  
p2 <- proc.time()
print(p2-p1)

write.csv(cbind.data.frame(lasfiles, log), file=paste0(dirname(out.metrics),"/metrics_log_FUSION.csv"), row.names=F)
# decided to write line by line so I can be a stalker! :) despite the I/O of writing to the file during
# every iteration, I have to know if things are going horribly wrong!
# write.csv(tc.log, tc.log.file, row.names=F, quote=F)

