library(raster)
# Using the metrics parameter file submitted to process_lidar_NormFilMetrics_byFile.R, 
# check if all lidar files were processed and produced a mets file. If not,
# continue putting the table together, but give a warning and write to fail
# file for further investigation.

paramfile <- "/mnt/a/tcormier/Mexico_CMS/lidar/parameter_files/CMS_metrics/CMS_tile_metrics_params_20170504_ALL.csv"

# Do you want to link up your field data now? (Y/N = case sensitive)
link <- "Y"

# If link==Y, provide name of field points shapefile with the fields you want!
# Plot shapefile - the aggregated points are fine - we just need to get the biomass!
pts.file <- "/mnt/a/tcormier/Mexico_CMS/field/points_wgs84_updatedFields_points/CMS_FieldPoints_wgs84_updatedFields_20170302_ecoRegions.shp"

# If link == Y, provide the name of the field containing the /path/filename input las file used to generate the metrics
lf.field <- "prof.based_lf"

#######

params <- read.csv(paramfile, stringsAsFactors = F)
fails <- as.data.frame(matrix(data=NA, nrow=0, ncol=5), stringsAsFactors = F)
names(fails) <- c("region", "input_file", "fail_step", "fail_status", "reason")

# If Link=Y, load shapefile once here
if (link == 'Y') {
  pts <- shapefile(pts.file)
}

p=21
for (p in (1:nrow(params))) {
  param <- params[p,]
  
  if (param$source == 'Cartodata') {
    region <- paste0(param$subdir, "_", param$subsection)
  } else {
    region <- param$subdir
  }

  
  # This part is taken from process_plots_lidarMetrics_byFile_qsubWrapper.R - may need to be tweaked for tiles.
  # outdir.m <- paste0(param$outdir, "/metrics/")
  # lasfiles <- list.files(param$path, "*.las$", full.names=T)
  # No longer need to list files because I have pre-listed them in text files (param$laslist)
  lasfiles <- scan(param$laslist, what='character')
  
  if (param$normalize == 'Y') {
    # check if all tiles were normalized
    normdir <- param$normdir
    normfiles <- list.files(normdir, "*.las$", full.names=T)
    norm.num <- length(normfiles)
    
    if (length(lasfiles) == length(normfiles)) {
      print("Normalization: well that's a good sign - same number of files in both directories!")
    } else {
      print("Normalization: Uh oh - different number of input and output files!")
      # Ok, not all files normalized - check out why!
      # First, figure out which ones did not go.
      norm.fail <- lasfiles[which(!(basename(lasfiles) %in% basename(normfiles)))]

      norm.logs.all <- list.files(paste0(param$normdir, "/norm_logs/"), full.names=T)
      fail.logs <- norm.logs.all[which(gsub('_normlog',"", sapply(norm.logs.all,stripExtBase)) %in% sapply(norm.fail,stripExtBase))]
      
      if (length(fail.logs) != length(norm.fail)) {
        print('Normalization: some files failed, but did not produce a log')
        nologs <- norm.fail[which(!(sapply(norm.fail,stripExtBase) %in% gsub('_normlog',"", sapply(norm.logs.all,stripExtBase))))]
        fail.nolog <- cbind.data.frame(region, nologs, "normalization", "failed to generate normfile and logfile", "unknown_nolog", stringsAsFactors=F)
        names(fail.nolog) <- names(fails)
        fails <- rbind.data.frame(fails, fail.nolog)
      }
      
      # Only want norm.fail files that weren't already added to the fails table.
      norm.fail <- norm.fail[which(!(norm.fail %in% fails$input_file))]
      fail.df <- cbind.data.frame(region, norm.fail, "normalization", "failed to generate normfile", "unknown", stringsAsFactors=F)
      names(fail.df) <- names(fails)
      fails <- rbind.data.frame(fails, fail.df)
    }
  }
  
    # Filtering QA check
    if (param$QAflags == 'Y') {
    if (param$normalize == 'Y' & param$QAflags == 'Y') {
      infiles <- normfiles
    } else if (param$normalize == 'N' & param$QAflags == 'Y') {
      infiles <- lasfiles
    }
    
    # Look for files that failed QA (filtering ran, but file did not pass all QA flags)
    qa.logdir <- paste0(param$fildir, "/QA_logs/")
    qa.tc <- list.files(qa.logdir, "*_TC.csv$", full.names=T)
    # qa.fg <- list.files(qa.logdir, "*_FG.csv$", full.names=T)
    
    # R takes forever to get through ALL of the logs, so let's cheat and use system
    # to cat and grep for "failed_QA' and just make a sublist
    fail.loglist <- paste0(qa.logdir, "QA_fail_list.csv")
    cmd <- paste0("echo ", paste0(qa.logdir, "/*_TC.csv"), " | xargs cat | grep 'failed_QA' >> ", fail.loglist)
    system(cmd)
    qa.fail <- read.csv(fail.loglist, stringsAsFactors=F, header=F)
    names(qa.fail) <- c("lf","QA_status")

    for (ql in 1:(nrow(qa.fail))) {
      tc <- qa.fail[ql,]
      # Find accompanying fg log file (Fabio)
      fg.file <- paste0(dirname(tc$lf),"_fil/QA_logs/", stripExtBase(tc$lf), "_QA_log_FG.csv")
      fg <- read.csv(fg.file, stringsAsFactors = F)
      fail.flags <- fg[which(fg >= 1)]
      f.reason <- paste0(names(fail.flags), " = ", fail.flags[1,], collapse="; ")
      df.enter <- cbind.data.frame(region, tc$lf, "Filtering", "failed QA flag", f.reason, stringsAsFactors=F)
      names(df.enter) <- names(fails)
      fails <- rbind.data.frame(fails, df.enter)
    }
    
    # Now one more check to make sure there are no missing files that maybe didn't write a log? 
    qa.files <- list.files(param$fildir, "*.las$", full.names=T)
    
    # IF some files didn't QA - check out why!
    # First, figure out which ones did not run at all by checking which input files
    # do not match an output, then check fails df to see if we already logged it there.
    # If not, need to figure out why it didn't run.
    # gah, infiles has double /. 
    infiles <- gsub("//", "/", infiles)
    qa.norun <- infiles[which(!(basename(infiles) %in% basename(qa.files)))]
    if (length(qa.norun != 0)) {
      # are they already in fails df?
      qa.norun.new <- qa.norun[which(!(qa.norun %in% fails$input_file))]
      if (length(qa.norun.new) > 0) {
        qa.nr.df <- cbind.data.frame(region, qa.norun.new, "Filtering", "failed to run filtering code", "unknown")
        fails <- rbind.data.frame(fails, qa.nr.df)
      }
    }
    
    if (param$calcMets == 'Y') {
      if (param$QAflags == 'Y') {
        m.infiles <- qa.files
      } else if (param$normalize == 'Y' & param$QAflags == 'N') {
        m.infiles <- normfiles
      } else if (param$normalize == 'N' & param$QAflags == 'N') {
        m.infiles <- lasfiles
      }
      
      # check if all tiles were normalized
      metdir <- paste0(param$metdir, "metrics/")
      metfiles <- list.files(metdir, "*.csv$", full.names=T)
      met.num <- length(metfiles)
      
      
      if (length(metfiles) == length(m.infiles)) {
        print("Metrics: well that's a good sign - same number of files in both directories!")
      } else {
        print("Metrics: Uh oh - different number of input and output files!")
        # Ok, not all files normalized - check out why!
        # First, figure out which ones did not go.
        met.fail <- m.infiles[which(!(sapply(m.infiles, stripExtBase) %in% gsub("_metrics", "", sapply(metfiles, stripExtBase))))]
        
        met.logs.all <- list.files(paste0(param$metdir, "metrics/met_logs/"), "*.txt", full.names=T)
        m.fail.logs <- met.logs.all[which(gsub('_metlog',"", sapply(met.logs.all,stripExtBase)) %in% sapply(met.fail,stripExtBase))]
        
        if (length(m.fail.logs) != length(met.fail)) {
          print('Metrics: some files failed, but did not produce a log')
          m.nologs <- met.fail[which(!(sapply(met.fail,stripExtBase) %in% gsub('_metlog',"", sapply(met.logs.all,stripExtBase))))]
          m.fail.nolog <- cbind.data.frame(region, m.nologs, "metrics", "failed to generate metrics file and logfile", "unknown_nolog", stringsAsFactors=F)
          names(m.fail.nolog) <- names(fails)
          fails <- rbind.data.frame(fails, m.fail.nolog)
        }
        
        # Only want met fail files that weren't already added to the fails table.
        met.fail <- met.fail[which(!(met.fail %in% fails$input_file))]
        m.fail.df <- cbind.data.frame(region, met.fail, "metrics", "failed to generate metrics file", "unknown", stringsAsFactors=F)
        names(m.fail.df) <- names(fails)
        fails <- rbind.data.frame(fails, m.fail.df)
      }
    }
    ################## Done QAing metrics run ###################
    
    # Concatenate all individual metrics files into one table and move indiv files to new folder.
    if (param$metType != 'both') {
      met.type <- param$metType
    } else if (param$metType == 'both') {
      met.type <- "prof-based_andFusion"
    }
    
    today <- format(Sys.Date(), format="%Y%m%d")
    outdir.m <- param$metdir
    all.mets.file <- paste0(outdir.m, "metrics_", param$metType, "_", today, ".csv")
    
    # Read them all in!
    # Use bash to concatenate all files
    # first write out the list of text files
    outmetlist <- paste0(outdir.m, param$subdir, "_", param$subsection, "_indiv_metfiles.txt")
    write.table(metfiles, outmetlist, row.names=F, col.names=F, quote=F)
    cat.cmd <- paste("/mnt/a/tcormier/scripts/general/bash/merge_textfiles.sh", outmetlist, all.mets.file, "/mnt/a/tcormier/Mexico_CMS/lidar/parameter_files/CMS_metrics_header.csv")
    system.time(system(cat.cmd))
    
    
    # ONLY FOR PLOTS
    # read it back in:
    mets.all <- read.csv(all.mets.file, stringsAsFactors = F)
    
    # This took 9.6 minutes to run on Jalisco T2 (~30,000 records). The bash solution
    # took 2.23 mins on the same data if I "find" the data. 1.72 mins if I send the script a list of the files.
#     system.time(all.mets <- lapply(metfiles, read.csv, stringsAsFactors=F))
#     all.mets.df <- do.call(rbind.data.frame, all.mets)
  
    # make new dir to house indiv metrics files
#     indiv.dir <- paste0(outdir.m, "/indiv/")
#     dir.create(indiv.dir)
#   
#   metfiles.mv <- paste0(indiv.dir, unlist(lapply(metfiles, stripExtBase)), ".csv")
#   file.rename(metfiles, metfiles.mv)
    
#     # link bio data
#     mets.bn <- sapply(mets.all$fusion_DataFile, stripExtBase)
#     mets.all$ID_TC <- as.numeric(sapply(strsplit(mets.bn, "_"), function(x) x[[2]]))
#     
#     mets.merge <- merge(pts@data, mets.all, by='ID_TC')
#     
#     # Let's clean up and swap a couple of columns to prevent issues opening in excel, which vomits if you start your first column name with "ID" (sigh)
#     mets.merge$id <- NULL
#     mets.merge <- mets.merge[,c(2,1,3:ncol(mets.merge))]
#     
#     out.merge <- paste0(unlist(strsplit(all.mets.file, "\\."))[1], "_fieldMerge.csv")
#     write.csv(mets.merge, out.merge, row.names=F, quote=F)  
    
    
  
