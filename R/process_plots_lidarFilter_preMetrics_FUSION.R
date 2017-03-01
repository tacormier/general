# This logs bad tiles/plots in a log file, and if they are really bad 
# (i.e., fail multiple quality flags), it # excludes them from the metrics.
# Potentially need to remove other flagged/problem tiles from modeling tables after they process. 
# FYI for now!

# This script expects to receive the following variables as part of the RDATA files submitted as
# and argument from process_plots_lidarMetrics_FUSION_qsubWrapper.R: 
# path.master, fildir, lasfiles, minht, min_ptden, vegflag, binsize, profdir, 
# synthProf, saveprof, saveWave, out.txt, reform, wavepdf

# IMPORTANT: for plots, since their lasfiles are typically all in one directory, they must be
# normalized before running this code. Unlike the tile code, this script will not normalize for you!
require(raster)
require(lidR)

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


#initiate pdf to save profile figs
# if (saveWave == 'Y') pdf(file=wavepdf, width=7, height=7)

# loop
p1 <- proc.time()

# Setting up some logs
tc.log <- as.data.frame(matrix(data=NA, ncol=2, nrow=1))
names(tc.log) <- c("file", "status")

# Now Fabio's log
dummyn1 <- paste0("ptden_lt", min_ptden)
dummyn2 <- paste0("fgt_gt1")
dummyn3 <- paste0("fgt")
dummyn4 <- paste0("flt_minus", abs(minht), "m_gt1")
dummyn5 <- paste0("flt_minus", abs(minht), "m")
# dummyn6 <- paste0("htamp_lt_", minhtamp, "m")
log <- array(0, c(length(lasfile), 11))
# We aren't using all of these, so I should clean this up later - for right now, I just want
# to get metrics running and for things to write into the correct columns
colnames(log) <- c("no_ground", "no_veg", dummyn1, "ptden", dummyn2, dummyn3, dummyn4, dummyn5, "dummyn6", "htamp", "nodata")
ptden <- array(NA, length(lasfile))

# for (p in 1:length(lasfiles)) {
print(paste0("calculating metrics for: ", lasfile))
tc.log$file <- lasfile
#lasdata <- read.csv(lasfile, header=F, col.names=c("x","y","z","i","a","n","r","c"))
# Read in normalized las plot file
lasnorm <- readLAS(lasfile)
names.orig <- names(lasnorm@data)

lasdata <- as.data.frame(lasnorm@data)
# changing names to match the rest of our code - hardcoded
names(lasdata) <- c("x", "y", "z", "gpstime", "i", "r", "n", "c", "a", "pulseID")
# lastest <- LAS(lasdata)

#if lasdata is empty, jump to next
if (nrow(lasdata)==0) {
  tc.log$status <- "failed_QA"
  write.table(tc.log, file=tc.log.file, row.names=F, quote=F, sep=",")
  log <- cbind.data.frame(lasfiles=lasfile, log)
  write.table(log, file=fg.log.file, row.names=F, quote=F, sep=",")
  next()
}

#remove rows with no elevation if they exist
naid <- which(is.na(lasdata$z))
if (length(naid) > 0) lasdata <- lasdata[-naid,]

#tile quality check
if (!(2 %in% lasdata$c)) {log[,1] <- 1} #No ground return
dummy <- c(1,3,4,5) %in% lasdata$c
if (vegflag == "T") {
  if (sum(dummy+0) == 0) {log[,2] <- 1} #No vegetation return
}

maxht <- quantile(lasdata$z, c(0.99))
fgt_maxht <- length(lasdata$z[lasdata$z > maxht]) / length(lasdata$z) * 100
if (fgt_maxht > 2) {log[,5] <- 1; log[,6] <- fgt_maxht} #More than 2% of normalized heights > maxht m
flt_minht <- length(lasdata$z[lasdata$z < minht]) / length(lasdata$z) * 100
if (flt_minht > 2) {log[,7] <- 1; log[,8] <- flt_minht} #More than 2% of normalized heights < minht m

#get point density
ptden <- nrow(lasdata)/(lasarea(lasnorm))
if (ptden < min_ptden) {log[,3] <- 1; log[,4] <- ptden}

#move to next tile if there is no ground or vegetation return, or if more than 2% of the normalized heights are outliers
if (sum(log[, c(1,2,5,7)]) >= 1) {
  tc.log$status <- "failed_QA"
  write.table(tc.log, file=tc.log.file, row.names=F, quote=F, sep=",")
  log <- cbind.data.frame(lasfiles=lasfile, log)
  write.table(log, file=fg.log.file, row.names=F, quote=F, sep=",")
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
    profname <- paste0(profdir, unlist(strsplit(basename(lasfile), "\\."))[1], "_profile.csv")
    write.csv(profile, file=profname, quote = F, row.names=F)
  }
  
#     #plot full profile in pdf
#     if (saveWave == 'Y') {
#       if (p %in% seq(1,length(lasfiles),4)) par(mfrow=c(2,2), oma=c(2.5,2.5,.5,.5), mar=c(1,1,.5,.5), mgp=c(1.5, .3, 0), tcl=-0.3) #c(bottom, left, top, right)
#       plot(profile$counts, profile$height, xlab="", ylab="", type="n")
#       abline(h=profile$height[gtopid], lty=1, col="gray80")
#       abline(h=profile$height[peakid], lty=2, col="red")
#       lines(profile$counts, profile$height, col="gray30")
#       legend("topright", basename(lasfile), bty="n") 
#       if (p %in% seq(1,length(lasfiles),4)) {
#         mtext("Counts", 1, outer=T, line=1.2)
#         mtext("Height (m)", 2, outer=T, line=1.2)
#       }
#     }
}# End synthProf if

#save normalized, filtered LAS
lasdata$z <- round(lasdata$z, digits=4)
outlasfil <- paste0(fildir, basename(lasfile))
# write.table(lasdata, file=outtxtnorm, sep=",", row.names=F, col.names=F, quote=F)
# Convert back to las file and save
# Put names back first
names(lasdata) <- names.orig
df2las <- LAS(lasdata)
# Create header from original file and then overwrite fields that are updated
lasheader <- lasnorm@header@data
lasheader$`Number of point records` <- df2las@header@data$`Number of point records`
lasheader$`Number of 1st return` <- df2las@header@data$`Number of 1st return`
lasheader$`Number of 2nd return` <- df2las@header@data$`Number of 2nd return`
lasheader$`Number of 3rd return` <- df2las@header@data$`Number of 3rd return`
lasheader$`Number of 4th return` <- df2las@header@data$`Number of 4th return`
lasheader$`Number of 5th return` <- df2las@header@data$`Number of 5th return`
lasheader$`Min X` <- df2las@header@data$`Min X`
lasheader$`Min Y` <- df2las@header@data$`Min Y`
lasheader$`Min Z` <- df2las@header@data$`Min Z`
lasheader$`Max X` <- df2las@header@data$`Max X`
lasheader$`Max Y` <- df2las@header@data$`Max Y`
lasheader$`Max Z` <- df2las@header@data$`Max Z`

df2las@header@data <- lasheader
writeLAS(df2las, outlasfil)

#   # convert txt to las for FUSION use LAStools
#   outlasnorm <- paste0(outlasnorm.dir, unlist(strsplit(basename(lasfile), "\\."))[1], ".las")
#   txt2las.cmd <- paste0("/mnt/a/tcormier/LAStools/bin/txt2las.exe -parse xyzianrc -i ", outtxtnorm, " -o ", outlasnorm)
#   system(txt2las.cmd)
if (file.exists(outlasfil)) {
  tc.log$status <- "filtered_las"
  write.table(outlasfil, out.txt, append=T, row.names=F, col.names = F, quote=F)
} else {
  tc.log$status <- "txt2las_failed"
}

log <- cbind.data.frame(lasfiles=lasfile, log)

write.table(tc.log, file=tc.log.file, row.names=F, quote=F, sep=",")
write.table(log, file=fg.log.file, row.names=F, quote=F, sep=",")

# }# End plot loop

#close pdf
# if (saveWave == 'Y') dev.off()

# # Reformat table for windows tool?
# if (reform == "Y") {
#   # tbl <- list.files(outlasnorm.dir, "*.las$", full.names=T)
#   lasfiles_reform <- paste0(stripExt(out.txt), "_wine.txt")
#   
#   # perform the reformatting and write out new file
#   tbl.ref <- gsub("/mnt", "Z:/mnt", lasfil.list)
#   tbl.ref <- gsub("/", "\\\\\\\\", tbl.ref)
#   
#   write.table(tbl.ref, lasfiles_reform, quote = F, row.names = F, col.names = F)
# }# end reform if
# 
# 
# lasfiles_reform_txt <- gsub("/mnt", "Z:/mnt", lasfiles_reform)
# lasfiles_reform_txt <- gsub("/", "\\\\\\\\", lasfiles_reform_txt)
# out.metrics.reform <- gsub("/mnt", "Z:/mnt", out.metrics)
# out.metrics.reform <- gsub("/", "\\\\\\\\", out.metrics.reform)
# 
# # Submit to fusion to calc metrics
# cloudmet.cmd <- paste0("/usr/bin/wine \\\\mnt\\\\a\\\\tcormier\\\\FUSION2\\\\cloudmetrics.exe /minht:1.5 /above:3 /new ", lasfiles_reform_txt, " ", out.metrics.reform)
# system(cloudmet.cmd)
# 
# p2 <- proc.time()
# print(p2-p1)
  
# decided to write line by line so I can be a stalker! :) despite the I/O of writing to the file during
# every iteration, I have to know if things are going horribly wrong!
# write.csv(tc.log, tc.log.file, row.names=F, quote=F)

