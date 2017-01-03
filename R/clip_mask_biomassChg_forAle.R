library(raster)
library(maptools)
library(gdalUtils)
rasterOptions(tmpdir="/home/tcormier/RasTmpDir/")
gdal_setInstallation(search_path="/mnt/s/gdal-1.10.1/bin/",rescan=T)


# biodir <- "/mnt/a/change/modis/america/out3b/"
biodir <- "/mnt/a/change/modis/america/luis/annualbio/"
masklossfile <- "/mnt/a/change/modis/america/luis/out9/pixlossc9i.tif"
maskgainfile <- "/mnt/a/change/modis/america/luis/out9/pixgainc9i.tif"
boundfile <- "/mnt/a/RAISG/RAISGlimits_sin.shp"
outdir <- "/mnt/a/tcormier/for_Ale/bio_mask_clip/"


years.ref <- c(2012:2014, 2003:2011) 
years.filenm <- c(10:12, 1:9)

alltiles <- list.files(biodir, "*.bsq$", full.names=T)

for (yr in c(1:(length(years.filenm)))) {
  actual.yr <- years.ref[yr]
  file.yr <- years.filenm[yr]
  
  pat <- glob2rx(paste0("*b_", file.yr, "h*"))
  yr.tiles <- alltiles[grep(pat, alltiles)]
  out.mos <- paste0(outdir, "mosaics/de", actual.yr, ".vrt")
  print(paste0("mosaicking ", length(yr.tiles), " tiles for year ", actual.yr, " which correpsonds to file name ", file.yr))
  
  # Mosaic using gdalbuildvrt from the gdalUtils package
  gdalbuildvrt(gdalfile = yr.tiles, out.mos, overwrite=T, te= c(-12231455.716, -2601964.2163, -3335851.559, 2601964.2163))
  # vrt <- raster(out.mos)
  # mosaic_rasters(gdalfile = yr.tiles, dst_dataset = out.mos, of="GTiff", overwrite=F)
} # Done mosaicking


pat.mos <- glob2rx("de20*.vrt")
mosdir <- paste0(outdir, "mosaics/")
mos.files <- list.files(mosdir, pat.mos, full.names=T)

# x <- raster(bio.files[1])
g <- raster(maskgainfile)
l <- raster(masklossfile)
b <- readShapePoly(boundfile)

proj <- projection(g)
# create one mask - crop it first to reduce data crunching, then we'll
# mask the final by b to get the exact polygon boundary (only want to do that once!)
g.crop <- crop(g, b)
l.crop <- crop(l, b)

# beginCluster()
# g.mask <- clusterR(g.crop, mask, args=list(mask=b))
# l.mask <- clusterR(l.crop, mask, args=list(mask=b))
# endCluster()
# outras.g <- paste0(outdir, "mosaics_masked_clipped/Biomass_2003-2014_gain.tif")
# outras.l <- paste0(outdir, "mosaics_masked_clipped/Biomass_2003-2014_loss.tif")
# writeRaster(g.mask, outras.g, datatype=dataType(g), overwrite=T)
# writeRaster(l.mask, outras.l, datatype=dataType(l), overwrite=T)


fun <- function(x) { x[x!=0] <- 1; return(x) }
beginCluster()

g1 <- clusterR(g.crop, calc, args=list(fun=fun))
l1 <- clusterR(l.crop, calc, args=list(fun=fun))
# done with cluster object
# endCluster()

# Not the most efficient way of doing this, but for some reason, simply summing them
# is resulting in all NAs???
m1 <- l1
m1[g1 == 1] <- 1

# A little QA to make sure the mask is correct - these should both return TRUE:
# takes a bit of time, but need to be sure!
# identical(m1[g1==1], g1[g1==1])
# identical(m1[l1==1], l1[l1==1])

# masking by complex polygon boundary is time intensive, so only want to do it once here - 
# not to individual inputs and not as an extra step at the end.
# beginCluster()
# m <- mask(m1, b)
m <- clusterR(m1, mask, args=list(mask=b))
endCluster()


system.time(for (i in mos.files) {
  print(basename(i))
  ras <- raster(i)
  # test <- resample(ras, m)
  # writeRaster(test, paste0(outdir, "test.tif"))
  # ras.crop <- crop(ras,b)
  ras.crop <- crop(ras,m)
  # r.mask1 <- mask(ras.crop, b)
  ras.mask <- mask(ras.crop, m)
  
  # These lines are for the unmasked but clipped 2003 file.
  # beginCluster()
  # ras.mask <- clusterR(ras.crop, mask, args=list(mask=b))
  # endCluster()
  
  # dataType(ras)
  # Setting proj necessary here bc I used maptools instead of rgdal (does not carry proj info into object)
  projection(ras.mask) <- proj
  outras <- paste0(outdir, "mosaics_masked_clipped/", unlist(strsplit(basename(i), "\\."))[1], "_mc.tif")
  writeRaster(ras.mask, filename = outras, overwrite=T, datatype=dataType(ras))
  print(paste0("finished processing ", basename(i)))
}# end bio.files loop
)

# outfiles <- list.files(outdir, "*.tif$", full.names=T)
# for (x in outfiles) {
#   r <- raster(x)
#   projection(r) <- proj
#   writeRaster(r, filename = x, overwrite=T, datatype=dataType(r))
#   
#   
# }

# Stack mosaicked, masked, clipped annual biomass layers:
st.files <- list.files(paste0(outdir, "mosaics_masked_clipped/"), "*_mc.tif$", full.names=T)
br <- stack(st.files)
writeRaster(br, paste0(outdir, "final/Biomass_2003-2014.tif"), datatype="INT2U", overwrite=T)





