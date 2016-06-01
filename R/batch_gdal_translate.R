library(raster)
library(gdalUtils)

# Set some variables
# indir <- "/file/path/with/trailing/slash/"
indir <- "/Users/tcormier/Documents/871_MexicoCMS/choosing_field_sites/Mex_highBiomass_lidar50m/"
# outdir <- "/file/path/with/trailing/slash/"
outdir <- "/Users/tcormier/Documents/test/"  


# find all the header files associated with the envi images
hfiles <- list.files(indir, "*.hdr$", full.names = T)

# Now strip off the .hdr because we want the actual envi image file
efiles <- unlist(lapply(hfiles, function(x) unlist(strsplit(x, "\\."))[1]))

# Now we can loop
for (i in efiles) {
  # since these are envi files with no extension to start, this epic text
  # manipulation is not necessary...but I put it in in case you ever need to do this
  # with other types of files that DO have extensions.
  out.tif <- paste0(outdir, unlist(strsplit(basename(i), "\\."))[1], ".tif")
  gdal_translate(i, out.tif, of = 'GTiff')
} # end efile loop
