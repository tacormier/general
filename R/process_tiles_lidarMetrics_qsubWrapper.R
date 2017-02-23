# This code submits jobs to the cluster to calculate metrics for lasfiles.
# Calls process_tiles_lidarMetrics.R

#######################################################################

# A parameter file that lists the directory to each site's lasfiles (path), 
# as well as the following fields, which will be passed to the metrics script:
# minhtamp, minht, maxht, vegflag, min_ptden, tilesize, binsize, and dtm. See 
# "/mnt/a/tcormier/Mexico_CMS/lidar/parameter_files/CMS_metrics/CMS_metrics_params_20170216.csv"
# as an example (this file has other fields as well, and that is fine - and I am using those other 
# fields to make life easier!). This param file was created from a combo of CMS_makeLists_plots_lidarIndex_utm.R
# and CMS_generate_lidarMetrics_paramFile.R

# Some notes about the fields:
# min_ptden: min CartoData = 8 ppm2; min SL Brazil = 4 ppm2
# vegflag: (T/F): vegetation returns classified?
# maxht: maximum expected tree height. If less 
#        than 1% of points > maxht, these points will be simply removed (outliers). If more than 1% 
#        of points > maxht, tile will be flagged as bad and will not be processed.
# minht: if less than 1% of points < minht, these points will be simply removed (outliers). If more 
#        than 1% of points < minht, tile will be flagged as bad and will not be processed. We expect 
#       some negative heights after normalization. Too many negative heights indicate an issue with the DTM.
# minhtamp: min normalized height amplitude for tile to be processed. If minhtamp < (max(lasdata$z)-min(lasdata$z)) 
#           after removal of outliers, tile will not be processed (no trees)
# tilesize: in meters
# binsize: vertical resolution of profile (m)


# User Variables
paramfile <- "/mnt/a/tcormier/Mexico_CMS/lidar/parameter_files/CMS_metrics/CMS_metrics_params_20170216.csv"

#do you want to save the profile so you don't have to calc again?
#Usually yes for plot profiles. Will save to indir/profiles
saveprof <- "Y"

# Do you want plots of the waveforms for every tile? CAUTION: this could become a huge PDF!
# You can always do this later for some or all of the tiles if you are saving the profiles.
saveWave <- "N"

# Where to save grid engine log files (metrics code saves its own logfiles as well!)
# The script will create a subdir for each site within the directory provided
QLOG_main <- "/mnt/a/tcormier/scripts/logs/CMS_metrics/"
#######################################################################

params <- read.csv(paramfile, stringsAsFactors = F)

for (p in (1:nrow(params))) {
  param <- params[p,]
  
  # Set up variables to send off to the metrics script
  # indir assumes tiling has been completed and assumes a certain folder structure 
  # (a bit specific to CMS, but likely how we would structure for other projects).
  tilesize <- param$tilesize
  indir <- paste0(param$path, "Tiles_", tilesize, "m/") 
  binsize <- param$binsize
  min_ptden <- param$min_ptden
  vegflag <- param$vegflag
  # maxht <- param$maxht  # THIS is now calculated, not a fixed height.
  minht <- param$minht
  minhtamp <- param$minhtamp
  dtm.file <- param$dtm
  
  profdir <- paste0(param$path, "Tiles_", tilesize, "m_profiles/")
  wavepdf <- paste0(param$path, "Tiles_", tilesize, "m_waveforms.pdf")
  if (saveprof == 'Y')  dir.create(profdir, showWarnings=F)
  
  # Save variables to rdata file - these are all of the variables required by process_tiles_lidarMetrics.R
  out.rdata <- paste0(param$path, "Tiles_", tilesize, "m_metrics_vars.RDATA")
  save(indir, tilesize, binsize, min_ptden, vegflag, maxht, minht, minhtamp, profdir, saveprof, saveWave, dtm.file, file=out.rdata)
  
  # Submit job to cluster!
  # This piece is there in case the site has no subsection, like Q1, T1 etc. - a bit specific to CMS.
  if (is.na(param$subsection)) { 
    QLOG <- paste0(QLOG_main, param$subdir, "/")
    jobname <- param$subdir
  } else {
    QLOG <- paste0(QLOG_main, param$subdir, "_", param$subsection, "/")
    jobname <- paste0(param$subdir, "_", param$subsection)
  } 
  dir.create(QLOG, showWarnings = F)
  
  sys.call <- paste("/net/share-2/export/HomeDir/sge6.2/bin/lx24-amd64/qsub -b yes -l rdisk=2G -q nondev.q -V -N", jobname, "-o",QLOG, "-e", QLOG, 
                    "/mnt/s/bin/jqsub 'R --vanilla --slave < /mnt/a/tcormier/scripts/general/R/process_tiles_lidarMetrics.R' --args", out.rdata)
  system(sys.call)
  
}



