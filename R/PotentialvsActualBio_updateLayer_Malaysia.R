library(raster)
library(sp)

act.file <- "/Users/tcormier/Documents/misc_projects/Malaysia_maps/data/as_biov2ct1.tif"
pt.file <- "/Users/tcormier/Documents/misc_projects/Malaysia_maps/data/Potential_Biomass_Tropical_Asia.tif"

act <- raster(act.file)
pt <- raster(pt.file)

pt.proj <- projectRaster(pt, act, method = 'ngb')
writeRaster(pt.proj, filename = "/Users/tcormier/Documents/misc_projects/Malaysia_maps/data/Potential_Biomass_Tropical_Asia_sinusoidal_nn.tif",
datatype=dataType(act), overwrite=T)

act.v <- as.vector(act)
pt.v <- as.vector(pt.proj)


# Replace where potential is < actual, write in actual.
pt.v.sub <- ifelse(pt.v < act.v, act.v, pt.v)
upd <- pt.proj
upd <- setValues(upd, pt.v.sub)
writeRaster(upd, filename = "/Users/tcormier/Documents/misc_projects/Malaysia_maps/data/Potential_Biomass_Tropical_Asia_sinusoidal_nn_replacedPotLTact.tif", 
            datatype=dataType(act), overwrite=T)



#######################
# Now need to replace NAs in potential with data from actual (this was added on later)
pt.upd <- raster("/Users/tcormier/Documents/misc_projects/Malaysia_maps/data/Potential_Biomass_Tropical_Asia_sinusoidal_nn_replacedPotLTact.tif")
pt.uv <- as.vector(pt.upd)
act.v <- as.vector(act)

pt.uv.sub2 <- ifelse(is.na(pt.uv) & !is.na(act.v), act.v, pt.uv)

upd2 <- pt.upd
upd2 <- setValues(upd2, pt.uv.sub2)

writeRaster(upd2, filename = "/Users/tcormier/Documents/misc_projects/Malaysia_maps/data/Potential_Biomass_Tropical_Asia_sinusoidal_nn_replacedPotLTact_andNA.tif", 
            datatype=dataType(pt.upd), overwrite=T)


#############################################
# Calcs on layers re: Tom's email from 1/26
pixloss <- raster("/Users/tcormier/Documents/misc_projects/Malaysia_maps/data/pixlossc9.tif")
pixgain <- raster("/Users/tcormier/Documents/misc_projects/Malaysia_maps/data/pixgainc9.tif")
b <- shapefile("/Users/tcormier/Documents/misc_projects/Malaysia_maps/data/Malaysia_Boundaries.shp")

# Crop and Mask every layer to the boundaries
#act
act.c <- crop(act, b)
beginCluster(4)
act.m <- clusterR(act.c, mask, args=list(mask=b))
# endCluster()

# upd2
upd2.c <- crop (upd2, b)
# beginCluster(4)
upd2.m <- clusterR(upd2.c, mask, args=list(mask=b))
# endCluster()

# diff
diff <- raster("/Users/tcormier/Documents/misc_projects/Malaysia_maps/data/PotentialMinusCurrent.tiff")
diff.c <- crop (diff, b)
# beginCluster(4)
diff.m <- clusterR(diff.c, mask, args=list(mask=b))
# endCluster()

# pixloss
loss.c <- crop (pixloss, b)
# beginCluster(4)
loss.m <- clusterR(loss.c, mask, args=list(mask=b))
# endCluster()

# pixgain
gain.c <- crop (pixgain, b)
# beginCluster(4)
gain.m <- clusterR(gain.c, mask, args=list(mask=b))
endCluster()

# Sums in t/ha
act.sum <- cellStats(act.m, stat = sum, na.rm=T)
pot.sum <- cellStats(upd2.m, stat = sum, na.rm=T)
diff.sum <- cellStats(diff.m, stat = sum, na.rm=T)
loss.sum <- cellStats(loss.m, stat = sum, na.rm=T)
gain.sum <- cellStats(gain.m, stat = sum, na.rm=T)

# Function to convert t/ha biomass to millions of metric tons
tpa2t <- function(x, pixres.m) {
  res.ha <- pixres.m^2/10000
  x.tot <- x*res.ha/1000000
  return(x.tot)
}

pixres <- 463.3127
act.totbio <- tpa2t(act.sum, pixres)
act.totC <- act.totbio/2

pot.totbio <- tpa2t(pot.sum, pixres)
pot.totC <- pot.totbio/2

# Decided to just take the difference mathematically because
# the diff in the rasters is lower than the mathematical difference. I believe 
# that's because there is no data in some cities in the actual, and we want to 
# account for that - it gets lost in the difference layer.
# diff.totbio <- tpa2t(diff.sum, pixres)
# diff.totC <- diff.totbio/2
diff.totbio <- pot.totbio-act.totbio
diff.totC <- pot.totC-act.totC

loss.totbio <- tpa2t(loss.sum, pixres)
loss.totC <- loss.totbio/2

gain.totbio <- tpa2t(gain.sum, pixres)
gain.totC <- gain.totbio/2

net.bio <- gain.totbio + loss.totbio
net.c <- gain.totC + loss.totC

df <- cbind.data.frame(act.totbio, act.totC, pot.totbio, pot.totC, diff.totbio, diff.totC, loss.totbio, loss.totC, gain.totbio, gain.totC, net.bio, net.c)
names(df) <- c("Present Biomass", "Present Carbon", "Potential Biomass", "Potential Carbon", "Difference Biomass", "Difference Carbon", "Biomass Loss 2003-2014", 
               "Carbon Loss 2003-2014", "Biomass Gain 2003-2014", "Carbon Gain 2003-2014", "Net Biomass Change 2003-2014", "Net Carbon Change 2003-2014")

df.bio <- df[,c(1,3,5,7,9,11)]
df.c <- df[,c(2,4,6,8,10,12)]

write.csv(df, "/Users/tcormier/Documents/misc_projects/Malaysia_maps/layer_summaries.csv", row.names=F, quote=F)
write.csv(df.bio, "/Users/tcormier/Documents/misc_projects/Malaysia_maps/layer_summaries_bio.csv", row.names=F, quote=F)
write.csv(df.c, "/Users/tcormier/Documents/misc_projects/Malaysia_maps/layer_summaries_c.csv", row.names=F, quote=F)



#############################################
# THIS did not work - did not replace the values, and no errors though.
# mask <- pt.proj
# mask[mask < act] <- -9999
# writeRaster(pt.proj, filename = "/Users/tcormier/Documents/misc_projects/Malaysia_maps/data/Potential_Biomass_Tropical_Asia_sinusoidal_nn_mask.tif", 
#             datatype=dataType(act), overwrite=T)
# 
# m <- mask(pt.proj, mask, maskvalue=-9999, updatevalue=act)
# 
# writeRaster(pt.proj, filename = "/Users/tcormier/Documents/misc_projects/Malaysia_maps/data/Potential_Biomass_Tropical_Asia_sinusoidal_nn_replacedPotLTact.tif", 
#             datatype=dataType(act), overwrite=T)
