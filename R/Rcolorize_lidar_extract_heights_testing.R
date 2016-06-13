library(raster)
library(rgl)
library(data.table)

######################################
imagefile <- "/mnt/a/tcormier/Mexico_CMS/lidar_animations/Jalisco/mosaic_23_45.tif"
lidarfile <- "/mnt/a/tcormier/Mexico_CMS/lidar_animations/Jalisco/merge.txt"



p <- proc.time()
lidar <- fread(lidarfile)
print(paste0("Opening lidar file completed in ", round(proc.time()[1]+proc.time()[2] - p[1] - p[2], digits=4), " seconds."))

names(lidar) <- c("x","y","z","i","a","n","r","c")
img <- brick(imagefile)
names(img) <- c("r","g","b")

# remove the scatter outlier(s) - not sure if I need to do this? 
# lidar <- lidar[lidar$z >= 255 ,]

# Coerce to sp spatialPointsDataFrame object
coordinates(lidar) <- ~x+y  

# subsample data (makes more tractable but not necessary) - not sure yet if I need
# to do this
# n=10000 
# lidar <- lidar[sample(1:nrow(lidar),n),]

# Assign RGB values from raster to points
system.time(lidar@data <- data.frame(lidar@data, extract(img, lidar)))
names(lidar) <- c("z","i","a","n","r","c","R","G","B")
# now get back to df
coords <- coordinates(lidar)
lidar.sub <- as.data.frame(cbind(coords, lidar@data$z, lidar@data$i, lidar@data$c, lidar@data$R, lidar@data$G, lidar@data$B))
names(lidar.sub) <- c("x","y","z","i","c","R","G","B")

write.table(lidar.sub, "/mnt/a/tcormier/Mexico_CMS/lidar_animations/Jalisco/merge_rgb.txt", quote=F, row.names=F, sep=",")


######################
# Attaching tree height from DSM
#dsmfile <- "/mnt/r/Mex_Lidar/Cartodata/Campeche_Yucatan/Deliverables/DSM/T1/DSM_T1_R0042C0005.tif"
# dtmfile <- "/mnt/r/Mex_Lidar/Cartodata/Campeche_Yucatan/Deliverables/DTM/T1/DTM_T1_R0042C0005.tif"
# lidarRGBfile <- "/mnt/a/tcormier/Mexico_CMS/lidar_animations/Yucatan/T1_R0042C0005_rgb.txt"

# dtmfile <- "/mnt/r/Mex_Lidar/Cartodata/Jalisco/Deliverables/Mosaics/DTM.TIF"
dtmfile <- "/mnt/r/Mex_Lidar/Cartodata/Campeche_Yucatan/Deliverables/Mosaics/T1/T1_DTM.tif"
# lidarRGBfile <- "/mnt/a/tcormier/Mexico_CMS/lidar_animations/Jalisco/merge_rgb.txt"
lidarRGBfile <- "/mnt/a/tcormier/Mexico_CMS/lidar_animations/Yucatan/T1_R0042C0005_rgb_noscale.txt"


#dsm <- raster(dsmfile)
dtm <- raster(dtmfile)
lidarRGB <- fread(lidarRGBfile)
names(lidarRGB) <- c("x","y","z","i","c","R","G","B")
# Coerce to sp spatialPointsDataFrame object
coordinates(lidarRGB) <- ~x+y  
# Assign RGB values from raster to points
# system.time(lidarRGB@data <- data.frame(lidarRGB@data, extract(dsm, lidarRGB)))
system.time(lidarRGB@data <- data.frame(lidarRGB@data, extract(dtm, lidarRGB)))
names(lidarRGB) <- c("z","i","c","R","G","B","DTM")
# now get back to df
coords <- coordinates(lidarRGB)
lidarRGB.sub <- as.data.frame(cbind(coords, lidarRGB@data$z, lidarRGB@data$i,lidarRGB@data$c, lidarRGB@data$R, lidarRGB@data$G, 
                                    lidarRGB@data$B, lidarRGB@data$DTM))

names(lidarRGB.sub) <- c("x","y","z","i","c","R","G","B","DTM")
lidarRGB.sub$height <- lidarRGB.sub$z - lidarRGB.sub$DTM

# There are some points with NA DSM/DTM - remove those
lidarRGB.sub <- lidarRGB.sub[complete.cases(lidarRGB.sub),]

# Change points classified as ground and ones iwth negative heights to 0.
lidarRGB.sub2 <- lidarRGB.sub
lidarRGB.sub2$height[(lidarRGB.sub$c == 2 | lidarRGB.sub$height < 0),] <- 0

write.table(lidarRGB.sub, paste0(dirname(lidarRGBfile), "/", unlist(strsplit(basename(lidarRGBfile), "\\."))[1], "_height.txt"), quote=F, row.names=F, sep=",")

# 3D plot?
cols <- rgb(lidarRGB.sub2[,6:8], maxColorValue = 255)
plot3d(coordinates(lidarRGB.sub2)[,1],coordinates(lidarRGB.sub2)[,2],lidarRGB.sub2$z, col=cols,
       pch=18, size=0.35, type="s", xlab="x", ylab="x", zlab="elevation")
