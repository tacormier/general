# Using the metrics parameter file submitted to process_lidarMetrics_byFile.R, 
# check if all lidar files were processed and produced a mets file. If not,
# continue putting the table together, but give a warning and write to fail
# file for further investigation.

paramfile <- "/mnt/a/tcormier/Mexico_CMS/lidar/parameter_files/CMS_metrics/CMS_plot_metrics_params_20170410_ALL.csv"

# Do you want to link up your field data now? (Y/N = case sensitive)
link <- "Y"

# If link==Y, provide name of field points shapefile with the fields you want!
# Plot shapefile - the aggregated points are fine - we just need to get the biomass!
pts.file <- "/mnt/a/tcormier/Mexico_CMS/field/points_wgs84_updatedFields_points/CMS_FieldPoints_wgs84_updatedFields_20170302_ecoRegions.shp"

# If link == Y, provide the name of the field containing the /path/filename input las file used to generate the metrics
lf.field <- "prof.based_lf"

#######

params <- read.csv(paramfile, stringsAsFactors = F)

for (p in (1:nrow(params))) {
  param <- params[p,]
  # This part is taken from process_plots_lidarMetrics_byFile_qsubWrapper.R - may need to be tweaked for tiles.
  outdir.m <- paste0(param$outdir, "/metrics/")
  lasfiles <- list.files(param$path, "*.las$", full.names=T)
  
  metfiles <- list.files(outdir.m, "*_metrics.csv$", full.names=T)
  
  if (length(lasfiles) == length(metfiles)) {
    print("well that's a good sign - same number of files in both directories!")
  } else {
    print("Uh oh - different number of input and out put files!")
  }
  
  # Check to make sure every lidar file has a matching metrics file:
  lf.bn <- unlist(lapply(lasfiles, stripExtBase))
  met.bn <- sub("_metrics", "", unlist(lapply(metfiles, stripExtBase)))
  
  missing <- lasfiles[which(!lf.bn %in% met.bn)]  
  # which(!lf.bn %in% met.bn)
  
  # Now write the missing files (if any) to a table in outdir.m
  if (length(missing) > 0) {
    missing.file <- paste0(outdir.m, "metrics_missing.csv")
    write.table(missing, missing.file, quote=F, row.names=F, col.names=F)
    warning(paste0("Files are missing from the metrics calculations. Concatenating table with files that ran successfully.\nSee ", missing.file, " for list of missing files."))
  } else {
    print("All files processed - nothing missing!")
  }
  
  # Concatenate all individual metrics files into one table and move indiv files to new folder.
  if (param$metType != 'both') {
    met.type <- param$metType
  } else if (param$metType == 'both') {
    met.type <- "prof-based_andFusion"
  }
  
  today <- format(Sys.Date(), format="%Y%m%d")
  all.mets.file <- paste0(outdir.m, "metrics_", param$metType, "_", today, ".csv")
  
  # Read them all in!
  all.mets <- lapply(metfiles, read.csv, stringsAsFactors=F)
  all.mets.df <- do.call(rbind.data.frame, all.mets)
  
  # make new dir to house indiv metrics files
  indiv.dir <- paste0(outdir.m, "/indiv/")
  dir.create(indiv.dir)
  
  metfiles.mv <- paste0(indiv.dir, unlist(lapply(metfiles, stripExtBase)), ".csv")
  file.rename(metfiles, metfiles.mv)
  
  #### ADD SECTION HERE TO SCAN TC LOG FILES FOR "FAIL" QUALITY FLAG. MAKE NEW FIELD TO INDICATE PASS OR FAIL.
  #### EVENTUALLY ADD NEW FIELD FOR THE REASON FOR FAIL BY MINING FABIO'S LOGS
  # Temporarily, I made a text file via the shell and wrote this crap to link it up with the metrics. 
  filfail <- read.table("/mnt/a/tcormier/Mexico_CMS/lidar/laslists/CMS_20170410_lasFilFail.txt", sep=",", stringsAsFactors = F)
  filfail <- filfail[,1]
  fail.id1 <- lapply(filfail, stripExtBase)
  fail.id2 <- sapply(fail.id1, strsplit, "_")
  fail.id <- unlist(sapply(fail.id2, function(x) x[2]))
  
  # vector of fail, not fail.
  all.mets.df$flag <- "pass_QA"
  all.mets.df$flag[]
  which(all.mets.df$ID_TC %in% as.numeric(fail.id))
  
  ############################################################################
  
  if (link == "N") {  
    write.csv(all.mets.df, all.mets.file, quote=F, row.names=F)
  } else if (link == 'Y') {
    library(raster)
    
    pts <- shapefile(pts.file)
    mets.bn <- sapply(all.mets.df$prof.based_lf, stripExtBase)
    all.mets.df$ID_TC <- as.numeric(sapply(strsplit(mets.bn, "_"), function(x) x[[2]]))
    
    mets.merge <- merge(pts@data, all.mets.df, by='ID_TC')
    
    # Let's clean up and swap a couple of columns to prevent issues opening in excel, which vomits if you start your first column name with "ID" (sigh)
    mets.merge$id <- NULL
    mets.merge <- mets.merge[,c(2,1,3:ncol(mets.merge))]
    
    out.merge <- paste0(unlist(strsplit(all.mets.file, "\\."))[1], "_fieldMerge.csv")
    write.csv(mets.merge, out.merge, row.names=F, quote=F)  
  }
}