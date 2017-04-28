# This code submits jobs to the cluster to calculate metrics for lasfiles.
# Calls process_tiles_lidarMetrics.R

#######################################################################
# This script submits individual las files to the cluster for metrics calculations. It expects a
# parameter file, which can be generated using CMS_generate_lidarMetrics_paramFile.R

# What's in the parameter file? Paths to directories containing lidar files on which to calculate metrics ('path'), 
# the 'binsize' for profile generation, desired metric types ('metType'), Y/N whether to save profiles and waveforms 
# ('saveprof', 'saveWave'), name of the height column in the lidar files ('htcol'), name of the intensity column in the
# lidar files ('intcol').
# 
source("/mnt/a/tcormier/scripts/general/R/handy_functions_TC.R")

# User Variables
# paramfile <- "/mnt/a/tcormier/Mexico_CMS/lidar/parameter_files/CMS_metrics/CMS_plot_metrics_params_20170410_ALL.csv"
paramfile <- "/mnt/a/tcormier/Mexico_CMS/lidar/parameter_files/CMS_metrics/CMS_tile_metrics_params_20170410_ALL.csv"

# Where to save grid engine log files (metrics code saves its own logfiles as well!)
# The script will create a subdir for each site within the directory provided
QLOG <- "/mnt/a/tcormier/scripts/logs/CMS_metrics_ALL_tiles/"
#######################################################################
dir.create(QLOG)

params <- read.csv(paramfile, stringsAsFactors = F)
p=33
for (p in (1:nrow(params))) {
  param <- params[p,]
  outdir.m <- paste0(param$outdir, "/metrics/")
  dir.create(outdir.m, recursive = T, showWarnings = F)
  outdir.rdata <- paste0(param$outdir, "/rdata/")
  dir.create(outdir.rdata, showWarnings = F)
  # Make a temp dir for storing intermediate products (if applicable)
  tmpdir <- paste0(param$outdir, "/temp/")
  dir.create(tmpdir, showWarnings = F)
  
  # set and create output directories
  saveprof <- param$saveprof
  if (saveprof == "Y") {
    # profile output directory
    profdir <- paste0(param$outdir, "/profiles/")
    dir.create(profdir, showWarnings = F)
  }
  
  saveWave <- param$saveWave
  if (saveWave == "Y") {
    # profile output directory
    wavedir <- paste0(param$outdir, "/waveforms/")
    dir.create(wavedir, showWarnings = F)
  } 

  ht.col <- param$htcol
  int.col <- param$intcol
  met.type <- param$metType
  binsize <- param$binsize
  
  ##### Stopped here 4/10 - need to go back to param file and make $path have the Tiles_30m subdir
  
  # find lasfiles in param$path
  # For now, assumes working only with lasfiles bc we have no need to convert to text now!
  lasfiles <- list.files(param$path, "*.las$", full.names=T)
  
  for (i in lasfiles) {
    lf <- i
    out.mets <- paste0(outdir.m, stripExtBase(i), "_metrics.csv")
    l.format <- unlist(strsplit(i, "\\."))[2]
    
    # set and create output directories
    if (saveprof == "Y") {
      profname <- paste0(profdir, stripExtBase(i), "_profile.csv")
    } else {
      profname <- NULL
    }
    
    saveWave <- param$saveWave
    if (saveWave == "Y") {
      wavename <- paste0(wavedir, stripExtBase(i), "_waveform.pdf")
    } else {
      wavename <- NULL
    }
    
    out.rdata <- paste0(outdir.rdata, stripExtBase(i), "_metrics_vars.RDATA")
    save(lf, out.mets, binsize, saveprof, profname, saveWave, wavename, l.format, ht.col, int.col, met.type, tmpdir, file=out.rdata)
    
    jobname <- paste0(stripExtBase(i), "_plotMetrics")
    
    sys.call <- paste("/net/share-2/export/HomeDir/sge6.2/bin/lx24-amd64/qsub -b yes -l rdisk=2G -q nondev.q -V -N", jobname, "-o",QLOG, "-e", QLOG, 
                      "/mnt/s/bin/jqsub 'R --vanilla --slave < /mnt/a/tcormier/scripts/general/R/process_lidarMetrics_byFile.R' --args", out.rdata)
    system(sys.call)
  }
} # end param loop