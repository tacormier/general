# This code submits jobs to the cluster to calculate metrics for lasfiles using FUSION.
# Calls process_tiles_lidarMetrics_FUSION.R

#######################################################################

# A parameter file that lists the directory to the lasfiles (path), 
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

# Using the same parameter file we use for Fabio's metrics code for now.
# User Variables
paramfile <- "/mnt/a/tcormier/Mexico_CMS/lidar/parameter_files/CMS_metrics/CMS_plot_metrics_params_20170223.csv"

# Do the laslists need to be reformated to run windows tool with wine? (see function documentation)? Y or N?
reform <- 'Y'

# Do you want to synthesize a profile? Might be a good idea for plots, but maybe not for tiles (slows it down).
synthProf <- 'Y'

#do you want to save the profile so you don't have to calc again?
#Usually yes for plot profiles. Will save to indir/profiles
saveprof <- "Y"

# Do you want plots of the waveforms for every tile? CAUTION: this could become a huge PDF!
# You can always do this later for some or all of the tiles if you are saving the profiles.
saveWave <- "Y"

# Where to save grid engine log files (metrics code saves its own logfiles as well!)
# The script will create a subdir for each site within the directory provided
QLOG <- "/mnt/a/tcormier/scripts/logs/CMS_Plot_metrics_FUSION/"
#######################################################################
dir.create(QLOG, showWarnings = F)
params <- read.csv(paramfile, stringsAsFactors = F)

# for now, there are 3 sites with no lasfiles - remove from params
# params <- params[-c(23, 50, 53),]

for (p in (1:nrow(params))) {
  # for (p in c(1,12,29,63)) {
  param <- params[p,]
  print(param$path)
  fildir <- paste0(dirname(param$path), "/norm_fil/")
  # }
  # Set up variables to send off to the metrics script
  # indir assumes tiling has been completed and assumes a certain folder structure 
  # (a bit specific to CMS, but likely how we would structure for other projects).
  path.master <- param$path
  binsize <- param$binsize
  min_ptden <- param$min_ptden
  vegflag <- param$vegflag
  # maxht <- param$maxht  # THIS is now calculated, not a fixed height.
  minht <- param$minht
  minhtamp <- param$minhtamp
  # output metrics file
  out.metrics <- paste0(fildir, "plot_metrics_FUSION.csv")
  
  # List las txt files and write to a txt file in param$path
  lasfiles <- list.files(path.master, "*.las$", full.names=T)
  # generate ID for table
#   dummy <- sub(".las", "", basename(lasfiles))
#   # dummy <- basename(dummy)
#   dummy <- lapply(dummy, strsplit, "_")
#   id <- as.numeric(sapply(dummy, "[", 2))
#   ycor <- as.numeric(sapply(dummy, "[", 1))
#   id <- paste0(xcor, "_", ycor)
  
  
  out.txt <- paste0(fildir, "filtered_laslist.txt")
  write.table(lasfiles, out.txt, row.names = F, col.names = F, quote=F)
  
  profdir <- paste0(fildir, "filtered_las_profiles/")
  wavepdf <- paste0(fildir, "filtered_las_waveforms.pdf")
  if (saveprof == 'Y')  dir.create(profdir, showWarnings=F)
  
  
  
  # Save some stuff to submit to metrics script:
  out.rdata <- paste0(fildir, "filtered_las_plot_metrics_vars_FUSION.RDATA")
  save(path.master, lasfiles, minht, min_ptden, vegflag, binsize, profdir, synthProf, saveprof, saveWave, wavepdf, out.txt, out.metrics, reform, file=out.rdata)
  
  # Submit job to cluster
  dir.create(QLOG, showWarnings = F, recursive = T)
  today <- format(Sys.time(), '%Y%m%d_%H%M%S')
  jobname <- paste0("CMS_Plot_metrics_FUSION_", today)
  # Using prod.q queue here bc it's a weekend, but change it back to nondev.q
  sys.call <- paste("/net/share-2/export/HomeDir/sge6.2/bin/lx24-amd64/qsub -b yes -q prod.q -V -N", jobname, "-l rdisk=2G", "-o",QLOG, "-e", QLOG, 
                    "/mnt/s/bin/jqsub 'R --vanilla --slave < /mnt/a/tcormier/scripts/general/R/process_plots_lidarMetrics_FUSION.R' --args", out.rdata)
  system(sys.call)
  
}


