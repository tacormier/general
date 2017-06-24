source("/mnt/a/tcormier/scripts/general/R/handy_functions_TC.R")

# User Variables
paramfile <- "/mnt/a/tcormier/Mexico_CMS/lidar/parameter_files/CMS_metrics/CMS_plot_metrics_params_20170615_ALL.csv"

# How many lasfiles do you want to submit to the worker script at once?
# batchsize <- 1

# grid engine log file directory 
QLOG <- "/mnt/a/tcormier/scripts/logs/CMS_normFilMetrics_PLOTS_byFile/"
#######################################################################
dir.create(QLOG, showWarnings = F)
# dir.create(lasnormdir, showWarnings = F)

params <- read.csv(paramfile, stringsAsFactors = F)

p=1
for (p in (1:nrow(params))) {
  
  param <- params[p,]
  lasdir <- param$path
  # dtm.file <- param$dtm
  normalize <- param$normalize
  QA.flags <- param$QAflags
  binsize <- param$binsize
  # min_ptden <- param$min_ptden
  vegflag <- param$vegflag
  absmaxht <- param$absmaxht  
  minht <- param$minht
  minhtamp <- param$minhtamp
  calc.mets <- param$calcMets
  saveprof <- param$saveprof
  saveWave <- param$saveWave
  # eco.shp <- param$eco_shp
  # laslist <- param$laslist
  
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

  rdata.dir <- paste0(dirname(lasdir), "/plotMetrics_RDATA/")
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
  lasfiles.all <- list.files(lasdir, "*.las$", full.names=T)
  # lasfiles.all <- scan(laslist, what='character')
  
  # Grab a batch of files to submit to worker
  # batches <- split((1:length(lasfiles.all)), ceiling(seq_along((1:length(lasfiles.all)))/batchsize))
#################### Copy files from storage to VM ############################
  
  
  
  
  #################################################################################
  # Loop over batches of lasfiles, write out text file of rdata paths for each las file
  # in the batch, then submit to worker.
#   for (b in (1:length(batches))) {
#     lasfiles <- lasfiles.all[batches[[b]]]
    # rdata.list <- vector()
  
  # This file will help us ID the proper UTM zone and epsg for each plot, which
  # will help us ID the right ecoregion file to intersect. 
  m.file <- "/mnt/a/tcormier/Mexico_CMS/lidar/LAS_Tiles30m_directories_UTMzones.csv"
  m <- read.csv(m.file, stringsAsFactors = F)
  
    for (i in lasfiles.all) {
       lf <- i
       # some housekeeping and setting up variables
       if (grepl("Cartodata", lf) == T) {
         utm.match <- regexpr(lf, pattern = "UTM[[:digit:]]{2}")
         utm <- gsub("UTM", "", regmatches(lf, utm.match))
         epsg <- m$epsg[m$source == 'Cartodata' & m$Zone_Num == utm][1]
         # so which eco.shp?
         eco.shp <- paste0("/mnt/a/tcormier/general/boundaries/Mex_EcoRegions/terrestrial_Ecoregions_updated_WGS84_clipMEX_", epsg, ".shp")
        
         min_ptden <- 8
         
         
       } else if (grepl("G-LiHT", lf) == T) {
         utm.match <- regexpr(lf, pattern = "UTM[[:digit:]]{2}")
         utm <- gsub("UTM", "", regmatches(lf, utm.match))
         epsg <- m$epsg[m$source == 'G-LiHT' & m$Zone_Num == utm][1]
         # so which eco.shp?
         eco.shp <- paste0("/mnt/a/tcormier/general/boundaries/Mex_EcoRegions/terrestrial_Ecoregions_updated_WGS84_clipMEX_", epsg, ".shp")
         
         min_ptden <- 4
         
       } else {
         stop("Did not find Cartodata or G-LiHT in file name. Cannot identify pt density or ecoregions shapefile")
       }
  ####################### NORMALIZATION SET UP ####################################   
      
      if (normalize == 'Y') {
        las2norm.file <- lf
        # Need to FIND dtm file. First, is this plot in cartodata or g-liht?
        if (grepl("Cartodata", las2norm.file) == T) {
          # Well THIS is overly complicated. haha. Works.
          dtm.file <- sub("_\\/", "\\/", gsub("[QT][1-9]{1,}", "", paste0("/mnt/r/Mex_Lidar/Cartodata/", unlist(strsplit(unlist(strsplit(stripExtBase(las2norm.file), "_Cartodata_"))[2], "_UTM"))[1], "/Deliverables/Mosaics/DTM_mosaic_rel.vrt")))
        } else if (grepl("G-LiHT", las2norm.file) == T) {
          dtm.file <- paste0("/mnt/r/Mex_Lidar/G_LiHT/", unlist(strsplit(unlist(strsplit(stripExtBase(las2norm.file), "_G-LiHT_"))[2], "_UTM"))[1], "/lidar/geotiff/DTM_mosaic_rel.vrt")
        } else {
          stop("Did not find Cartodata or G-LiHT in file name. Cannot identify DTM.")
        }
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
          profname <- paste0(profdir, stripExtBase(lf), "_profile.csv")
        } else {
          profname <- NA
        }
        
        saveWave <- param$saveWave
        if (saveWave == "Y") {
          wavename <- paste0(wavedir, stripExtBase(lf), "_waveform.pdf")
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
               profname, saveWave, wavename, met.type, ht.col, int.col, tmpdir, metlog.dir, calc.mets, eco.shp, file=out.rdata)
      
      # This is pointless really, but allows me to stop writing new code and use existing!
      rdata.txt <- paste0(rdata.dir, stripExtBase(lf), "_normFilMetrics_rdataList.txt")
      write.table(out.rdata, rdata.txt, col.names = F, quote = F, row.names = F)
    
    # grid engine job name
    jobname <- paste0(stripExtBase(lf), "_norm_fil_mets")
    sys.call <- paste("/net/share-2/export/HomeDir/sge6.2/bin/lx24-amd64/qsub -b yes -l rdisk=2G -q prod.q -V -N", jobname, "-o",QLOG, "-e", QLOG, 
                      "/mnt/s/bin/jqsub 'R --vanilla --slave < /mnt/a/tcormier/scripts/general/R/process_lidar_NormFilMetrics_byBatch_addEcoregions.R' --args", rdata.txt)
    system(sys.call)
    }# end i loop
} # end p loop
  





