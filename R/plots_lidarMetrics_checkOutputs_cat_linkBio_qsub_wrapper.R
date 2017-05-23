# This script is just a wrapper that will loop over the parameter file and submit each subsection
# as its own job to grid engine.

paramfile <- "/mnt/a/tcormier/Mexico_CMS/lidar/parameter_files/CMS_metrics/CMS_tile_metrics_params_20170504_ALL.csv"

# Where to write fail logs
fail.dir <- "/mnt/r/Mex_Lidar/metrics_fail_logs/indiv_section_fail_logs/"

# outdir - where to store concatenated metrics files
outdir.metcat <- "/mnt/r/Mex_Lidar/metrics_csvs/"

# Do you want to link up your field data now? (Y/N = case sensitive)
link <- "N"

# If link==Y, provide name of field points shapefile with the fields you want!
# Plot shapefile - the aggregated points are fine - we just need to get the biomass!
pts.file <- "/mnt/a/tcormier/Mexico_CMS/field/points_wgs84_updatedFields_points/CMS_FieldPoints_wgs84_updatedFields_20170302_ecoRegions.shp"

# If link == Y, provide the name of the field containing the /path/filename input las file used to generate the metrics
lf.field <- "prof.based_lf"

QLOG <- "/mnt/a/tcormier/scripts/logs/CMS_QA_cat_metrics/"
#######################################
dir.create(QLOG, showWarnings = F)
dir.create(fail.dir, showWarnings = F)

params <- read.csv(paramfile, stringsAsFactors = F)

# If Link=Y, load shapefile once here
if (link == 'Y') {
  pts <- shapefile(pts.file)
}

# p=21
# p=74
for (p in (1:nrow(params))) {
  param <- params[p,]
  
  # Need to do some hardcoded file path trickery for CMS gcp outputs
  if (param$source == 'Cartodata') {
    param$path_new <- paste0("/mnt/r/Mex_Lidar/gcp_outputs/outputs/full_run2/", param$subdir, param$subsection, "/mnt/lidar_data/lasdata/", param$subdir, "/LAS/", param$subsection,"/")
  } else if (param$source == 'G-LiHT') {
    param$path_new <- paste0("/mnt/r/Mex_Lidar/gcp_outputs/outputs/full_run2/", param$subdir, param$subsection, "/mnt/lidar_data/lasdata/", param$subdir, "/lidar/las/")
  } else {
    stop('param$source must be "Cartodata" or "G-LiHT"')
  }
  param$normdir <- paste0(param$path_new, "Tiles_30m_las_norm/")
  param$fildir <- paste0(param$path_new, "Tiles_30m_las_norm_fil/")
  param$metdir <- paste0(param$path_new, "Tiles_30m_las_metrics/20170503/")
  
  rdata.dir <- paste0(fail.dir, "/rdata/")
  dir.create(rdata.dir, showWarnings = F)
  rdata.file <- paste0(rdata.dir, param$subdir, "_", param$subsection, ".rdata") 
  save(param, fail.dir, outdir.metcat, link, pts.file, lf.field, file=rdata.file)
  
  jobname <- paste0(param$subdir, "_", param$subsection, "_metrics_QA_cat")
  sys.call <- paste("/net/share-2/export/HomeDir/sge6.2/bin/lx24-amd64/qsub -b yes -l rdisk=4G -q nondev.q -V -N", jobname, "-o",QLOG, "-e", QLOG, 
                    "/mnt/s/bin/jqsub 'R --vanilla --slave < /mnt/a/tcormier/scripts/general/R/plots_lidarMetrics_checkOutputs_cat_linkBio_qsub.R' --args", rdata.file)
  system(sys.call)
}
  