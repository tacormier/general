# This code submits jobs to the cluster to calculate metrics for lasfiles.

#######################################################################
# User Variables
# Input directory that 
indir <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/las_extract/extract_20150918/txt/"

profdir <- paste0(indir, "/profiles/")
dir.create(profdir, showWarnings=F)
tilesize <- 30 #in meters
binsize <- 0.5 #vertical resolution of profile (m)
min_ptden <- 8 #min CartoData = 8 ppm2; min SL Brazil = 4 ppm2
vegflag <- "F" #(T/F): vegetation returns classified?
#Values for flagging bad points or tiles - based on field data or expert knowledge
maxht <- 80 #maximum expected tree height. If less than 1% of points > maxht, these points will be simply removed (outliers). If more than 1% of points > maxht, tile will be flagged as bad and will not be processed
minht <- -3 #if less than 1% of points < minht, these points will be simply removed (outliers). If more than 1% of points < minht, tile will be flagged as bad and will not be processed. We expect some negative heights after normalization. Too many negative heights indicate an issue with the DTM.
minhtamp <- 2 #min normalized height amplitude for tile to be processed. If minhtamp < (max(lasdata$z)-min(lasdata$z)) after removal of outliers, tile will not be processed (no trees)
#do you want to save the profile so you don't have to calc again?
#Usually yes for plot profiles. Will save to indir/profiles
saveprof <- "Y"

#regions
regions <- c("Campeche_Yucatan", "Chiapas", "Chihuahua", "Jalisco", "Oaxaca1", "Oaxaca2", "Oaxaca3")
#dtm.file <- "/mnt/a/fgoncalves/Mexico/dtm_mosaic.tif"
#dtm.file <- "/mnt/a/tcormier/SE_Asia/Ellis_Paper/lidar/DTM_all/DTM_Ellis/se_asia_dtm.vrt"
#dtm.file <- "/Volumes/share-2/fgoncalves/Mexico/dtm_mosaic.tif"
dtmlist <-"/mnt/a/tcormier/Mexico_CMS/lidar/DTMs/lists/cartodata_dtm_vrt_list.txt"
#######################################################################