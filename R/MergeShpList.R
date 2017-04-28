library(raster)
library(pbapply)
# merge.dir <- "/Users/tcormier/Documents/871_MexicoCMS/local_copy/ground_coverage_lastools_noholes/wgs84/"
merge.dir <- "/Users/tcormier/Documents/871_MexicoCMS/local_copy/ground_index_noholes/wgs84/"
# out.merge <- "/Users/tcormier/Documents/871_MexicoCMS/local_copy/ground_coverage_lastools_noholes/CMS_lidar_groundCoverage_ALL_WGS84.shp"
out.merge <- "/Users/tcormier/Documents/871_MexicoCMS/local_copy/ground_index_noholes/wgs84/CMS_ground_index_noHoles_wgs84.shp"

merge.files <- list.files(merge.dir, "*.shp$", full.names=T)
# source.g <- grep('G-LiHT', merge.files)
# source.c <- grep('Cartodata', merge.files)
# source.all <- vector(length = length(merge.files))
# source.all[source.g] <- 'G-LiHT'
# source.all[source.c] <- 'Cartodata'
read.merge <- pblapply(merge.files, shapefile)
# read.merge2 <- mapply(cbind, read.merge, "Source"=(source.all), SIMPLIFY=F)
# m.names <- c('FID', 'Source')
# read.merge2 <- lapply(read.merge2, setNames, m.names)
# Nor sure why "Source" didn't flow through as the column name

merge.all <- do.call(bind, read.merge)
shapefile(merge.all, out.merge, overwrite=T)
