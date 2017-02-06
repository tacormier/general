library(raster)
library(maptools)
library(gdalUtils)
library(ggplot2)
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


####################
# 1/23/2017 Some additional code working specifically on 2007.
# Wayne asked for the total of the 2007 biomass in the amazon basin from the change analysis
# AND the 2007 total from the original pan-tropical biomass map ca. 2007.
library(ggplot2)
library(hexbin)


yr=8
biodir <- "/mnt/a/change/modis/america/luis/annualbio/"
boundfile <- "/mnt/a/RAISG/RAISGlimits_sin.shp"
outdir <- "/mnt/a/tcormier/for_Ale/bio_mask_clip/check_annVsOrigPantrop/"
dir.create(outdir)

chg.2007 <- raster("/mnt/a/tcormier/for_Ale/bio_mask_clip/mosaics_masked_clipped/de2007_mc.tif")
chg.2007.all <- raster("/mnt/a/tcormier/for_Ale/bio_mask_clip/mosaics/de2007.vrt")
orig.2007 <- raster("/mnt/a/biov2/america/am_biov2ct1.tif")

b <- readShapePoly(boundfile)

# crop orig to chg
orig.crop <- crop(orig.2007, b)
# Line up the pixels (tried to do this in parallel, but it was VERY messed up)
orig.resamp <- resample(orig.crop, chg.2007, method='ngb')

orig.resamp[is.na(chg.2007)] <- NA
chg.2007[is.na(orig.resamp)] <- NA

# Let's write these out to files JUST to be sure they are masked properly (they are).
# identical(is.na(orig.resamp), is.na(chg.2007)) # This was FALSE - but true as a vector
writeRaster(orig.resamp, paste0(outdir, "orig_2007_mc.tif"), datatype=dataType(orig.2007), overwrite=T)
writeRaster(chg.2007, paste0(outdir, "chgAnalysis_2007_mc.tif"), datatype=dataType(chg.2007), overwrite=T)

orig.vec <- as.vector(orig.resamp)
chg.vec <- as.vector(chg.2007)

ona <- is.na(orig.vec)
cna <- is.na(chg.vec)
identical(ona, cna)

# Sum in total tons (so multiply sum by area of a pixel = 463.3127^2)
summary(orig.vec)
summary(chg.vec)

orig.vec <- orig.vec[!is.na(orig.vec)]
chg.vec <- chg.vec[!is.na(chg.vec)]

# Sum in the basin = MG/ha
orig.sum <- sum(orig.vec)
chg.sum <- sum(chg.vec)

# Try it one more way
stats.orig <- cellStats(orig.resamp, stat='sum', na.rm=T)
stats.chg <- cellStats(chg.2007, stat='sum', na.rm=T)

# In millions of metric tons of Carbon. /area of a pixel by 10000 to get to ha, then by a million to get millions
# of metric tons of biomass, then by 2 to get millions of metric tons of Carbon
orig.totbio <- (orig.sum*((463.3127^2)/10000))/1000000/2
chg.totbio <- (chg.sum*((463.3127^2)/10000))/1000000/2

################################
# Do the same thing with the whole basin - not just change
chg.all <- crop(chg.2007.all, b)
# m <- mask(m1, b)
beginCluster()
chg.mask <- clusterR(chg.all, mask, args=list(mask=b))
endCluster()

# line up pixels
orig.resamp2 <- resample(orig.crop, chg.mask, method='ngb')
orig.resamp2[is.na(chg.mask)] <- NA
chg.mask[is.na(orig.resamp2)] <- NA

#
orig.vec2 <- as.vector(orig.resamp2)
chg.vec2 <- as.vector(chg.mask)

ona2 <- is.na(orig.vec2)
cna2 <- is.na(chg.vec2)
identical(ona2, cna2)

orig.vec2 <- orig.vec2[!is.na(orig.vec2)]
chg.vec2 <- chg.vec2[!is.na(chg.vec2)]

# Sum in the basin = MG/ha
orig.sum2 <- sum(orig.vec2)
chg.sum2 <- sum(chg.vec2)


# In millions of metric tons of Carbon. /area of a pixel by 10000 to get to ha, then by a million to get millions
# of metric tons of biomass, then by 2 to get millions of metric tons of Carbon
orig.totbio2 <- (orig.sum2*((463.3127^2)/10000))/1000000/2
chg.totbio2 <- (chg.sum2*((463.3127^2)/10000))/1000000/2

# Try it one more way - all checks out!
stats.orig2 <- cellStats(orig.resamp2, stat='sum', na.rm=T)
stats.chg2 <- cellStats(chg.mask, stat='sum', na.rm=T)
stats.orig.totbio2 <- (stats.orig2*((463.3127^2)/10000))/1000000/2
stats.chg.totbio2 <- (stats.chg2*((463.3127^2)/10000))/1000000/2

df <- cbind.data.frame(orig.vec2, chg.vec2)
names(df) <- c("original", "changeAnalysis")

# bin<-hexbin(orig.vec2, chg.vec2, xbins=50) 
# plot(bin, main="Hexagonal Binning", xlim=c(0,400))

hb <- ggplot(df, aes(original, changeAnalysis)) + xlim(0,425) + ylim(0,425)
hb + stat_binhex(aes(colour = ..value..)) + geom_abline(intercept = 0, color="red")

# Take a thousand random samples
s <- sample(nrow(df), 1000)
df.samp <- df[s,]
p <- ggplot(df.samp, aes(original, changeAnalysis)) + xlim(0,375) + ylim(0,375) + geom_point() + geom_abline(intercept = 0, color="red")
p

###############
# Ok, so issues. Now we are going to the raw data. 
# Instructions from Ale: could you please take a random sample 
# from band 11 in  /mnt/a/change/modis/america/inputs2/b.2007.h11v09.bip  
# and plot it versus band 11  in /mnt/a/mod68b/h11v09.bip

chg.raw <- as.vector(raster("/mnt/a/change/modis/america/inputs2/b.2007.h11v09.bip", band=11))
orig.raw <- as.vector(raster("/mnt/a/mod68b/h11v09.bip", band=11))
chg.raw2 <- as.vector(raster("/mnt/a/change/modis/america/inputs2/b.2007.h12v10.bip", band=11))
orig.raw2 <- as.vector(raster("/mnt/a/mod68b/h12v10.bip", band=11))

s <- sample(length(chg.raw), 5000)

chg.r.s <- chg.raw[s]
orig.r.s <- orig.raw[s]
df.raw <- cbind.data.frame(orig.r.s, chg.r.s)
names(df.raw) <- c("raw_original", "raw_change")

p <- ggplot(df.raw, aes(raw_original, raw_change)) + geom_point() + geom_abline(intercept = 0, color="red")
p

# Now for the second tile
s2 <- sample(length(chg.raw2), 5000)

chg.r.s2 <- chg.raw[s2]
orig.r.s2 <- orig.raw[s2]
df.raw2 <- cbind.data.frame(orig.r.s2, chg.r.s2)
names(df.raw2) <- c("raw_original", "raw_change")

p2 <- ggplot(df.raw2, aes(raw_original, raw_change)) + geom_point() + geom_abline(intercept = 0, color="red")
p2

# And all together now...
df.raw3 <- rbind.data.frame(df.raw, df.raw2)
p3 <- ggplot(df.raw3, aes(raw_original, raw_change)) + geom_point() + geom_abline(intercept = 0, color="red")
p3

lm <- lm(df.raw3$raw_change ~ df.raw3$raw_original)
summary(lm) 

