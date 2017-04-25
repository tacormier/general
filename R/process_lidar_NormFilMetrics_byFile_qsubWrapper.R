source("/mnt/a/tcormier/scripts/general/R/handy_functions_TC.R")

# User Variables
paramfile <- "/mnt/a/tcormier/Mexico_CMS/lidar/parameter_files/CMS_metrics/CMS_tile_metrics_params_20170410_ALL_Jalisco.csv"
# grid engine log file directory 
QLOG <- "/mnt/a/tcormier/scripts/logs/CMS_TEST/"
#######################################################################
dir.create(QLOG, showWarnings = F)
# dir.create(lasnormdir, showWarnings = F)

params <- read.csv(paramfile, stringsAsFactors = F)

# p=33
for (p in (1:nrow(params))) {
  
  param <- params[p,]
  lasdir <- param$path
  dtm.file <- param$dtm
  normalize <- param$normalize
  QA.flags <- param$QAflags
  binsize <- param$binsize
  min_ptden <- param$min_ptden
  vegflag <- param$vegflag
  absmaxht <- param$absmaxht  
  minht <- param$minht
  minhtamp <- param$minhtamp
  calc.mets <- param$calcMets
  saveprof <- param$saveprof
  saveWave <- param$saveWave
  
  # make some directories as needed
  if (normalize == 'Y') {
    dir.create(param$normdir, showWarnings = F)
    normlog.dir <- paste0(param$normdir, "norm_logs/")
    dir.create(normlog.dir, showWarnings = F)
  } else if (normalize == 'N') {
    # These varibles still get passed to the worker, so need to be set to something.
    dtm.file <- normdir <- normlog.dir <- NA
  } else {
    stop("The 'normalize' variable must be 'Y' or 'N' (case sensitive)")
  }
  
  if (QA.flags == 'Y') {
    dir.create(param$fildir, showWarnings = F)
    QAlog.dir <- paste0(param$fildir, "QA_logs/")
    dir.create(QAlog.dir, showWarnings = F)
  } else if (QA.flags == 'N') {
    # These variables still get passed to the worker, so need to be set to something. 
    # These should already be set by param file generation code, but just in case user
    # made their own param file. A bit of added protection.
    fildir <- QAlog.dir <- binsize <- absmaxht <- minhtamp <- minht <- vegflag <- min_ptden <- NA
  } else {
    stop("The 'QA.flags' variable must be 'Y' or 'N' (case sensitive)")
  }

  rdata.dir <- paste0(dirname(lasdir), "/Tiles_30m_las_RDATA/")
  dir.create(rdata.dir, showWarnings = F)
  
  if (saveprof == 'Y') {
    profdir <- paste0(param$metdir, "/profiles/")
    dir.create(profdir, showWarnings=F, recursive=T)
    binsize <- param$binsize
  } else if (saveprof == 'N') {
    profdir <- binsize <- NA
  } else {
    stop("The 'saveprof' variable must be 'Y' or 'N' (case sensitive)")
  }
  if (saveWave == 'Y') {
    wavedir <- paste0(param$metdir, "/waveforms/")
    dir.create(wavedir, showWarnings=F, recursive=T)
  } else if (saveWave == 'N') {
    wavedir <- NA
  } else {
    stop("The 'saveWave' variable must be 'Y' or 'N' (case sensitive)")
  }
  if (calc.mets == 'Y') {
    if (param$metType == 'prof-based' | param$metType == 'both') {
      binsize <- param$binsize
    } else {
      binsize <- NA
    }
    met.dir <- paste0(param$metdir, "/metrics/")
    dir.create(met.dir, showWarnings = F, recursive=T)
    # Make a temp dir for storing intermediate products (if applicable)
    tmpdir <- paste0(param$metdir, "/temp/")
    dir.create(tmpdir, showWarnings = F)
    metlog.dir <- paste0(met.dir, "met_logs/")
    dir.create(metlog.dir, showWarnings = F)
  } else if (calc.mets == 'N') {
    met.dir <- tmpdir <- metlog.dir <- NA
  } else {
    stop("The 'calc.mets' variable must be 'Y' or 'N' (case sensitive)")
  }
  
  # Now work per las file
  lasfiles <- list.files(lasdir, "*.las$", full.names=T)
  
  
  for (i in lasfiles) {
     lf <- i
####################### NORMALIZATION SET UP ####################################   
    
    if (normalize == 'Y') {
      las2norm.file <- lf
      dtm.file <- param$dtm
      outnorm <- paste0(param$normdir, basename(lf))
    } else if (normalize == 'N') {
      las2norm.file <- outnorm <- NA
    } else {
      stop("the 'normalize' variable must be 'Y' or 'N' (case sensitive)")
    }
    
####################### FILTERING SET UP ####################################    
    if (QA.flags == 'Y') {
      # If we normalized, we want to use the normalized las file as the input to
      # the QA flag. If not, we use lf.
      if (normalize == 'Y') {
        las2qa.file <- outnorm
      } else {
        las2qa.file <- lf
      }
      
      outfil <- paste0(param$fildir, basename(lf))
      
      } else if (QA.flags == 'N') {
        las2qa.file <- outfil <- NA
      } else {
      stop("the 'QA.flags' variable must be 'Y' or 'N' (case sensitive)")
    }
####################### METRICS SET UP ####################################
    # Determine the input to metrics code.
    if (calc.mets == 'Y') {
      if (QA.flags == 'N' & normalize == 'N') {
        # This is the only situation where we have to pass las2mets.file to the worker.
        # Otherwise, it's defined in the worker code.
        las2mets.file <- lf
      } else {
        # To be determined by worker code
        las2mets.file <- NA
      }
      
      ht.col <- param$htcol
      int.col <- param$intcol
      met.type <- param$metType
      binsize <- param$binsize
      
      out.mets <- paste0(met.dir, stripExtBase(lf), "_metrics.csv")
      
      # set other output file names
      if (saveprof == "Y") {
        profname <- paste0(profdir, stripExtBase(i), "_profile.csv")
      } else {
        profname <- NA
      }
      
      saveWave <- param$saveWave
      if (saveWave == "Y") {
        wavename <- paste0(wavedir, stripExtBase(i), "_waveform.pdf")
      } else {
        wavename <- NA
      }
      
    } else if (calc.mets == 'N') {
      las2mets.file <- out.mets <- NA
    } else {
      stop("the 'calc.mets' variable must be 'Y' or 'N' (case sensitive)") 
    }
    
    out.rdata <- paste0(rdata.dir, stripExtBase(lf), "_normFilMetrics_vars.RDATA")
    save(normalize, las2norm.file, dtm.file, outnorm, normlog.dir, QA.flags, las2qa.file, outfil, QAlog.dir, 
             binsize, min_ptden, vegflag, absmaxht, minhtamp, minht, las2mets.file, out.mets, saveprof, 
             profname, saveWave, wavename, met.type, ht.col, int.col, tmpdir, metlog.dir, calc.mets, file=out.rdata)
    
    jobname <- paste0(stripExtBase(lf), "_normFilMetrics")
    
    sys.call <- paste("/net/share-2/export/HomeDir/sge6.2/bin/lx24-amd64/qsub -b yes -l rdisk=2G -q dev.q -V -N", jobname, "-o",QLOG, "-e", QLOG, 
                      "/mnt/s/bin/jqsub 'R --vanilla --slave < /mnt/a/tcormier/scripts/general/R/process_lidar_NormFilMetrics_byFile.R' --args", out.rdata)
    system(sys.call)
    
  }
}