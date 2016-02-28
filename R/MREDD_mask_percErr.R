library(raster)
library(rgdal)

mexbound <- "/Users/tcormier/Documents/general/boundaries/gadm_Mexico_dissolve"
percerr <- "/Users/tcormier/Documents/858_MREDD/bio_data_Ale/percerr.tif"

mb <- readOGR(dirname(mexbound), basename(mexbound), stringsAsFactors = F)
pe <- raster(percerr)

mask <- mask(pe, mb)
writeRaster(mask, "/Users/tcormier/Documents/858_MREDD/bio_data_Ale/percerr_masked.tif", datatype='INT1U', overwrite=T)
