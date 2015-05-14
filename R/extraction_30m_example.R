#Author: mfarina
#Date: 4/27/2015
#Description: example script of extracting continuous, 30m data for GLAS Lidar shots with 35m radius.
#Multiple pixels fall within each GLAS shot footprint.  For each shot footprint, this code extracts the
#values from *all* pixels that fall inside the footprint, whether completely or partially, and then calculates
#the weighted mean value, with weights being the proportion of the pixel within the footprint.

require(raster)
require(maptools)
require(rgdal)
require(rgeos)
library(geosphere,lib.loc="/home/mfarina/R/x86_64-redhat-linux-gnu-library/3.0")
rasterOptions(tmpdir="/home/mfarina/RasTmpDir2/")

# 1. Read in GLAS data, with ShotID (ShotID = REC_NDX * 100 + SHOT_NUM) and shot lat/long data:
myGLAS = read.csv('my_glas_data.csv')
#Example:
myGLAS=read.csv("/mnt/a/mfarina/Extratropics_field_site_selection/tropical//asia/csvs_with_hansen_tile_coords/Data_organized_by_hansen_tile/asia_00N_090E/asia_hansen_00N_090E.csv")
myGLAS = myGLAS[1:11,]

# 2. Get lat/long coords:
coords = data.frame(myGLAS$LON,myGLAS$LAT)


# 3. Create circular polygons with 35m radius around each glas shot (i.e., a shapefile of shot footprints):
        #-------------------------------
        # Make a list, each element of the list has one SpatialPolygons polygon (one circle)
        list.poly=list()
        for (i in 1:length(coords[,1])) {
          myCircle = data.frame(matrix(NA,361,2))
          for (angle in 0:360) { myCircle[(angle+1),] = destPoint(coords[i,],angle,35) }
          list.poly[i] = SpatialPolygons(list(Polygons(list(Polygon(SpatialPoints(myCircle))),1))) }
        #----------------------------
        # Get the ID names of each polygon...intially they are all the same, but each polygon needs a unique ID:
        IDs <- sapply(list.poly, function(x) slot(slot(x, "polygons")[[1]], "ID"))
        # Give each polygon a unique ID  (1: number of list elements)
        Spol1 <- SpatialPolygons(lapply(1:length(list.poly), function(i) {
          Pol <- slot(list.poly[[i]], "polygons")[[1]]
          slot(Pol, "ID") <- as.character(i)
          Pol } ) )
        # Get the IDs, and set the row.names of the polygons as these IDs
        IDs = sapply(slot(Spol1, "polygons"), function(x) slot(x, "ID"))
        row.names(Spol1) = IDs
        #As a check, this should be true now:
        length(unique(IDs)) == length(list.poly)
        #----------------------------
        # Build a data.frame to attach to the SpatialPolyons
        data.df = data.frame(IDs,myGLAS);  row.names(data.df)=IDs
        spol.df = SpatialPolygonsDataFrame(Spol1,data = data.df)
        projection(spol.df) = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
        #assign the final variable, "polys.wgs84.35m" as the spatial polygons dataframe:
        polys.wgs84.35m=spol.df



# 4. Get image/raster data (4 band stack in this example):
first.1 =  raster(paste0("/mnt/a/tm/mattmo/Hansen_GFC2013_first_00N_090E.tif"),1)
first.2 =  raster(paste0("/mnt/a/tm/mattmo/Hansen_GFC2013_first_00N_090E.tif"),2)
first.3 =  raster(paste0("/mnt/a/tm/mattmo/Hansen_GFC2013_first_00N_090E.tif"),3)
first.4 =  raster(paste0("/mnt/a/tm/mattmo/Hansen_GFC2013_first_00N_090E.tif"),4)
myBands = stack(first.1,first.2,first.3,first.4)


# 5. Extract the data, specifying number of layers:
extract.all = extract(myBands,polys.wgs84.35m,nl=4,weights=TRUE)


# 6.  Compute the weighted means of the landsat bands for each glas shot
#     Here, the weights are the *proportions* of each pixel inside the glas shot. 
#     length(extract.all) should give you the total number of glas shots
      results = data.frame(matrix(NA,length(extract.all),4))   
      for (i in 1:length(extract.all)) {
          #In "current", each row corresponds to a single pixel inside the glas shot.
          # (ie, 8 rows means there are 8 total pixels inside this shot)
          current = data.frame(extract.all[i])
          #Get the weighted means:
          results[i,1] = weighted.mean(current[,1],current$weight,na.rm=F)  #first.1
          results[i,2] = weighted.mean(current[,2],current$weight,na.rm=F)  #first.2
          results[i,3] = weighted.mean(current[,3],current$weight,na.rm=F)  #first.3 
          results[i,4] = weighted.mean(current[,4],current$weight,na.rm=F)  #first.4 
      }
          
          
  

  
  
  
  