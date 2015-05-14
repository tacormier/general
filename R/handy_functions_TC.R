# A compilation of helpful functions that can be used across projects
# Tina Cormier

#####################################
# Rescale a raster to new min and max
# Arguments: r=raster layer; newmin = new minimum value (number); newmax = new maximum value (number)
rescaleRas <- function(r, newmin, newmax) {
  require(raster)
  newrange <- c(newmin, newmax)
  r.res <- (r - cellStats(r, "min"))/diff(c(cellStats(r, "min"), cellStats(r, "max"))) * diff(newrange) + newrange[1]
  return(r.res)
} # end rescaleRas function


#####################################
# From Fabio's calc metrics code:
#function to produce pseudo-waveforms
makeprof <- function(mydata, res) { #mydata = las data, res = vertical resolution (m)
  elev <- mydata$z
  elev0 <- elev - min(elev) #note that 0 here is the lowest elevation, not the ground peak
  int <- mydata$i
  clas <- mydata$c #2 - ground, 3 - low vegetation, 4 - medium vegetation, 5 - high vegetation, etc
  breaks <- seq(min(elev0), max(elev0)+res, by=res)
  z <- breaks[2:length(breaks)]
  gmax <- max(elev0[clas==2])
  #if (gmax < z[1]) warning("ground height < bin size")
  
  #get number of points per height interval
  hist_elev0 <- hist(elev0, breaks=seq(min(elev0), max(elev0)+res, by=res), include.lowest=T, right=F, plot=F) 
  counts <- hist_elev0$counts
  
  #get total intensity per height interval
  int_sum <- rep(0, times=length(z))
  int_n <- rep(0, times=length(z))
  for (i in 1:length(z)) { #inefficient... change it later
    for (j in 1:length(elev0)) if (elev0[j] >= breaks[i] & elev0[j] < breaks[i+1]) { 
      int_sum[i] <- int_sum[i] + int[j]
      int_n[i] <- int_n[i] + 1
    }
  }
  
  #check: int_n should be equal to counts
  if (sum(int_n - counts) != 0) stop("Check the code... we have a problem") 
  
  #return
  data.frame(height=z, counts=counts, intensity=int_sum)
}

#
#
# STILL IN DEV:
# THIS IS CODE from Fabio that I can use as a ref to make the 
# ground peak 0m - need to work this into plotWaveform function.

# plotlas.files <- list.files(indir, ("*.txt$"), full.names=T)
# plotlas <- lapply(plotlas.files, read.table, sep=",",col.names = c("x","y","z","i","a","n","r","c"))
# plots <- read.csv(plotfile)
# 
# # Calc waveforms here and plop them into a list, so we don't have to keep re-running it to try different plots.
# profile <- lapply(plotlas.csvs, makeprof, res=binsize)
# 
# # Make ground peak 0m
# profdata <- array(data=NA, dim=c(length(seq(0,100,binsize)), length(plotlas.files)))
# plotids <- array(NA, length(plotlas.files))
# gmax0 <- lapply(plotlas, function(lasdata) max(lasdata$z[lasdata$c==2], na.rm=T) - min(lasdata$z))
# 
# for (g in 1:length(gmax0)){
#   if (gmax0[[g]] < profile[[g]]$height[1]) {
#     peakid <- 1
#   } else {
#     peakid <- which(profile[[g]]$counts == max(profile[[g]]$counts[profile[[g]]$height <= gmax0[[g]]])) #find ground peak
#     if (length(peakid) > 1) peakid <- peakid[1]
#   }
#   
#   profile[[g]] <- profile[[g]][peakid:nrow(profile[[g]]),] #trim profile
#   profile[[g]]$height <- profile[[g]]$height - min(profile[[g]]$height) #correct heights
#   
#   # This puts all profiles into one dataframe. Could be super useful later. Right now, I have them all in a list,
#   # and the plotting code is expecting a list.  
#   #profdata[1:nrow(profile[[g]]),g] <- profile[[g]]$counts
#   
#   dummy <- gsub(".txt", "", plotlas.files[g])
#   dummy <- strsplit(dummy, split="m_")
#   plotids[g] <- dummy[[1]][2]
# }
#####################################

# This function makes a plot of a lidar profile and requires ggplot2. The function finds also finds the
# ground peak, removes anything beneath it, and adjust/trims the other heights relative to the ground (Fabio's code).
# waveformDF = resulting DF after executing makeprof function; nameHeightCol = name of column containing heights;
# nameCountCol = name of column containing counts; smoothFactor controls the degree of waveform line smoothing 
# (should generally be less than 1 to really show peaks in lidar data, but test different factors!).
# title = optional text for plot title; leg.txt = optional text to be printed at the bottom right of the 
# plot (e.g., Field collected height and/or biomass).
#
plotWaveform <- function(waveformDF, nameHeightCol, nameCountCol, smoothFactor=0.25, plot.title=NULL, leg.txt=NULL) {
  require(ggplot2)
  wave <- waveformDF
  #calculate the smooth line
  smooth_vals = predict(loess(get(nameCountCol)~get(nameHeightCol),wave, span=smoothFactor),wave$height)
  sm.line <- as.data.frame(cbind(smooth_vals, wave[[nameHeightCol]]))
  names(sm.line) <- c("counts", "height")
  
  p <- ggplot(wave, aes(counts, height)) + ylim(0,100)+ geom_point(size=1.5) + geom_path(data=sm.line, col="chartreuse4") + 
    theme_bw() + theme(legend.position="none") 
  
  # If we want to use a color gradient on the line! Would change the color in geom_path to aes(colour=wave$height)
  # + scale_colour_gradient2(low="tan4", mid="darkkhaki", high="chartreuse4",midpoint=sm.line$height[median(sm.line$counts)]) +
  
  if (!is.null(plot.title)) {
    p <- p + ggtitle(plot.title) 
  }
  
  if (!is.null(leg.txt)) {
    #     gtxt <- grobTree(textGrob(leg.txt, x=.75,  y=0.1, hjust=0,vjust=0,
    #                            gp=gpar(col="gray16")))
    #     p <- p + annotation_custom(gtxt, size=1.5)
    
    p <- p + annotate("text",x=max(wave$counts),y=min(wave$height),hjust=1.0,vjust=.1,label=leg.txt, size=2.75)
  } #end annotation if
  
  return(p)
  
} #end plotWaveform function


################## distOK ################## 
# function to determine if point is far enough from raster cell border
# Originally written by WW, but updated by TC to include all variables as args
# (was originally in a script and called variables from the script)
#
# INPUTS: xy - dataframe of xy points to check (first column must be x coord, second must be y coord); 
# image - image used to determine if xy points are far enough from the border of the cells in which they fall;
# dist - distance (in map units) from the edge of a raster cell to exclude plot - usually the plot radius (dist).
#
# Returns dataframe containing a T/F for each xy pair. 
#
distOK <- function(xy, image, dist) {
  # Required Packages
  require(raster)
  
  # Calcualte image extent and min/max X and Y
  ext <- extent(image)
  x_min <- ext@xmin
  y_min <- ext@ymin
  x_res <- xres(image)
  y_res <- yres(image)
  
  # x and y from xy (coords list)
  v <- vector(mode="logical", length = length(xy[,1]))
  for (n in 1:length(xy[,1])) {
    #v[n] <- distOK(xy[n,])
  
    x <- xy[n,1]
    y <- xy[n,2]
    # using the remainder here lets us determine, relative to the individual pixel, how far the point is
    # from the western pixel boundary (dist_west_x), then subtract that from the pixel resolution to get how far
    # the point is from the eastern edge of the pixel (dist_east_x). If either of those are less than the 
    # specified "dist" (plot radius), then we don't use it!
    dist_west_x <- (x - x_min) %% x_res
    dist_east_x <- x_res - dist_west_x
    # Calculate minimum distance between plot center and raster cell edge
    x_dist <- (min(dist_west_x, dist_east_x) >= dist)
    if (!x_dist) {
      result = FALSE
    } else {
        dist_south_y <- (y - y_min) %% y_res
        dist_north_y <- y_res - dist_south_y
        result <- (min(dist_south_y, dist_north_y) >= dist)
      }
    v[n] <- result
  }# end v loop
  return(v)
}

################## zeroToNA ################## 
# Define function to convert zeros to NA
zeroToNA <- function(x) { x[x==0] <- NA; return(x)}
#
################## writeMDDB_multPlotsPerPixel ################## 
# PURPOSE: Write MDDB from plot/glas data and image inputs with coarser resolution than plot diameter. 
# It removes sample points (plot centers) that are outside of a specified distance from the edge 
# of an image pixel's edge. Any negative biomass values are converted to zero. Writes a shapefile 
# containing only points that are greater than the distance specificed by the "dist" variable.
# The function returns the mddb as an object and also writes it to a file.

# INPUTS This function takes the following inputs: Name of the input image or stack of predictors (inImg), vector
# of predictor names (must match nlayers(inImg) (predNames), name of the file (can be shp or csv file) 
# containing the input plot data (inPlot), the name of the column containing the x coordinate (xcol), the name of
# the column containing the y coordinate (ycol),Name of the output Shapefile with the averaged plot data (outPlotMns),
# Name of the output MDDB csv (outMDDB), the name (case sensitive and in quotes) of the field containing modeling response values
# in the plot data (e.g. "biomass") (reponseCol), the minimum number of samples to be considered to calculate the average within the pixel
# (minshots), distance (meters) from the edge of a raster cell to exclude plot - usually the plot radius (dist), logfile (T/F)
# will write a logfile to the same directory as outMDDB - default is TRUE (logfile).
#
# REQUIRES the following packages: raster, maptools, sp. Other functions: distOK and zerotoNA from /mnt/a/tcormier/scripts/general/R/handy_functions_TC.R
#
# AUTHOR(s): Original script written by Wayne Walker and Alessandro Baccini. Converted to function by Tina Cormier.
#
# Untested with shapefile input as inPlot

writeMDDB_multPlotsPerPixel <- function(inImg, predNames, inPlot, xcol, ycol, outPlotMns, outMDDB, responseCol, minshots, dist, logfile=T) {
  # Required packages
  require(raster)
  require(maptools)
  require(sp)
  #require(rasterVis)
  
  # let's write a log file of "stuff" - this will overwrite an existing log with the same name (can change)
  # this later if it becomes a problem
  if (logfile == T) {
    lf <- paste0(unlist(strsplit(outMDDB, "\\."))[1], "_logfile.txt")
    file.create(lf)
    funCall <- paste0("Function Call: WriteMDDB_multPlotsPerPixel(", inImg,", (",paste(predNames,collapse=","),")",", ",inPlot,", ",outPlotMns,", ",outMDDB,", ",responseCol,", ",minshots,", ",dist,")")
    write(funCall, lf, append=T)
  }
  # Read Shapefile and input image
  # Get file extension of inPlot (must be .shp or .csv) **This assumes the only "." in the filename
  # is the one that delineates the name from the extension.
  ext <- unlist(strsplit(inPlot, "\\."))[2]
  if (ext == "csv" | ext == "CSV") {
    vec <- read.csv(inPlot)
  } else if (ext == "shp") {
      vec <- readShapePoints(inPlot)
  } # end file ext vec if/else

  coords <- as.data.frame(cbind(vec[[xcol]], vec[[ycol]]))
  names(coords) <- c("LON", "LAT")
  vec <- SpatialPointsDataFrame(coords, vec, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  vecTxt <- paste0("Number of samples in inPlot: ", nrow(vec))
  write(vecTxt, lf, append=T)
  
  #image <- raster(inImage)
  image <- raster(inImg)
  # Create blank raster images to hold mean values
  imageTemp1 <- image
  imageTemp2 <- image
  imageTemp3 <- image
  
  cat('Removing points too close to raster cell edges\n')
  write('Removing points too close to raster cell edges', lf, append=T)
  v <- distOK(coords, image, dist)
  
  # Remove all points that are too close to a raster cell edge
  flags <- which(v)
  vec <- vec[flags,]
  # I think these two lines can be accomplished in one line (above)
  #   slot(vec, "data") <- vec@data[flags,]
  #   slot(vec, "coords") <- vec@coords[flags,]
    
  # Extract values for attName variable (attribute field)
  attributes <- vec[[responseCol]]
  
  # Convert negative biomass values to 0
  attributes[attributes < 0] <- 0
  
  cat("Calculating means\n")
  dt1 <- Sys.time()
  write(paste0("Calculating means, counting points per cell, and converting raster results to points at ",dt1), lf, append=T)
  
  # Calculate mean and number of points that fall in a raster cell and save as images
  meanImage <- rasterize(vec, imageTemp1, field=attributes, fun=function(x,...){mean(na.omit(x))})
  sdImage <- rasterize(vec, imageTemp3, field=attributes, fun=function(x,...){sd(na.omit(x))})
  countImage <- rasterize(vec, imageTemp2, field=attributes,fun=function(x,...){length(na.omit(x)) >= minshots})
  countImage2 <- rasterize(vec, imageTemp2, field=attributes,fun=function(x,...)length(x))
  
  # Convert all zero pixels to NA to exclude them from the output
  countImage <- calc(countImage, zeroToNA)
  
  # Multiply mean image times count image (ones and NAs) to create the final mean image
  resultImagemn <- meanImage * countImage
  #sdImage2 <- calc(sdImage, zeroToNA)
  resultImagesd <- sdImage * countImage
  # set values where the mean image has valid numbers, but the sd is NA to 9999 (to force the rasterToPoints function
  # for sd to have the same number of points as the mean does - otherwise, can't combine (cbind) all data for trainvals).
  resultImagesd[!is.na(resultImagemn) & is.na(resultImagesd)] <- 9999
  
  cat('Converting images to points and outputing shapefile\n')
  # Convert mean image to points (each point will be located in the middle of a raster cell
  pointsmn <- rasterToPoints(resultImagemn, spatial=TRUE)
  pointssd <- rasterToPoints(resultImagesd, spatial=TRUE)
  names(pointsmn) <- responseCol
  names(pointssd) <- paste0(responseCol, "_stdev")
  
  # Output Shapefile of plot means
  writePointsShape(pointsmn, outPlotMns)
  
  dt2 <- Sys.time()
  mean.msg <- paste0("mean calculation finished ", dt2, " and took ", round(as.numeric(difftime(dt2,dt1,units='mins')),2), " minutes.")
  numPoints.msg <- paste0("wrote ", nrow(pointsmn), " training points to file.")
  write(mean.msg, lf, append=T)
  print(mean.msg)
  
  ####### Generate the training data for model development #############
  write("Assembling final MDDB", lf, append=T)
  nshots = extract(countImage2,pointsmn)
  satImage = stack(inImg)
  pixId = cellFromXY(satImage, pointsmn@coords)
  
  satData = extract(satImage, pointsmn@coords)
  trainvals = cbind(pixId, nshots, pointsmn@data, pointssd@data, satData, pointsmn@coords)
  
  #Rename variables
  colnames(trainvals) <- c("pixid","nshots","lidar_biomn","lidar_biosd", predNames, "X", "Y")
  # TOOK this out to keep the function general, but this is something you could do based on specific bands after MDDB is generated. 
  #trainvals2 = subset(trainvals, trainvals$L3 > 1 & trainvals$L4 > 1 & trainvals$L5 > 1 & trainvals$L7 > 1)
  #print(paste0("you filtered ", (nrow(trainvals)-nrow(trainvals2)), " records from trainvals, leaving you with ", nrow(trainvals2), " (", (round(nrow(trainvals2)/nrow(trainvals)*100,2)), "%) rows remaining for modeling."))
  
  #Write out the mddb file
  cat('Writing MDDB file\n')
  write("Writing MDDB file. . .", lf, append=T)
  write.csv(trainvals,file=outMDDB,row.names=FALSE)
  
  # Check that csv and shp files were created
  if (!file.exists(outPlotMns)) {
    write(paste0("ERROR: plot shapefile did write to disk: ", outPlotMns), lf, append=T)
  } else {
    write(paste0(outPlotMns, ": file created."), lf, append=T)
  }
  
  if (!file.exists(outMDDB)) {
    write(paste0("ERROR: MDDB csv did write to disk: ", outMDDB), lf, append=T)
  } else {
    write(paste0(outMDDB, ": file created."), lf, append=T)
  }
  
  write("finished", lf, append=T)
  
  return(trainvals)
} # End writeMDDB_coarse function

################## writeMDDB_multPixelsPerPlot ################## 
# PURPOSE: Write MDDB from plot/glas data and image inputs with finer resolution than plot diameter. 
# Extracts continuous, fine resolution raster data for GLAS Lidar shots or field plots with a specified 
# radius in meters (i.e. 35 m for GLAS). In this case, multiple pixels fall within each GLAS shot footprint
# or field plot.  For each plot, this code extracts the values from *all* of pixels that fall inside the 
# footprint, whether completely or partially, and then calculates the weighted mean value, with weights 
# equaling the proportion of the pixel within the footprint.

# The function returns the mddb as an object and also writes it to a file.

# INPUTS This function takes the following inputs: /path/filename of shapefile or csv containing plot data (inPlot); 
# the name of the column containing the x coordinate (xcol), the name of the column containing the y coordinate (ycol),
# the name of the column containing modeling response data (e.g., biomass column) (responseCol), the plot/shot 
# radius **in meters** (dist.m), Name of the input image or stack of predictors (inImg), vector of predictor names 
# (must match nlayers(inImg) (predNames), name of output buffered points (so we can comment out buffering section
# if we run this more than once (outBuff), do you want a logfile? accepts T or F (logfile).
#
# REQUIRES the following packages: raster, maptools, rgdal, rgeos, geosphere.
#
# AUTHOR(s): Original script written by Mary Farina. Converted to function by Tina Cormier.
#
# Untested with shapefile input as inPlot

writeMDDB_multPixelsPerPlot <- function(inPlot, xcol, ycol,responseCol, dist.m, outMDDB, inImg, predNames, outBuff, logfile=T) {
  require(raster)
  require(maptools)
  require(rgdal)
  require(rgeos)
  #library(geosphere,lib.loc="/home/mfarina/R/x86_64-redhat-linux-gnu-library/3.0")
  
  # let's write a log file of "stuff" - this will overwrite an existing log with the same name (can change)
  # this later if it becomes a problem
  if (logfile == T) {
    lf <- paste0(unlist(strsplit(outMDDB, "\\."))[1], "_logfile.txt")
    file.create(lf)
    funCall <- paste0("Function Call: WriteMDDB_multPixelsPerPlot(", inPlot,", ",xcol,", ",ycol,", ",responseCol,", ",dist.m,", ",outMDDB,", ",inImg,", (",paste(predNames,collapse=","),"), ", outBuff, ", ",logfile, ")")
    write(funCall, lf, append=T)
  } 
 
  # Read Shapefile and input image
  # Get file extension of inPlot (must be .shp or .csv) **This assumes the only "." in the filename
  # is the one that delineates the name from the extension.
  write(paste0("opening plot data: ", inPlot), lf, append=T)
  ext <- unlist(strsplit(inPlot, "\\."))[2]
  if (ext == "csv" | ext == "CSV") {
    vec <- read.csv(inPlot)
  } else if (ext == "shp") {
    vec <- readShapePoints(inPlot)
  } # end file ext vec if/else
  
  coords <- as.data.frame(cbind(vec[[xcol]], vec[[ycol]]))
  names(coords) <- c("LON", "LAT")
  vec <- SpatialPointsDataFrame(coords, vec, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  # Create circular polygons with 35m radius around each glas shot (i.e., a shapefile of shot footprints):
  write("buffering points by plot radius", lf, append=T)
  
  # gBuffer requires projected coordinates, so just for a minute:
  # Use an equal-area projection to buffer the points, then put back to WGS84
  # Adapted from: http://gis.stackexchange.com/questions/121489/1km-circles-around-lat-long-points-in-many-places-in-world
  aeqd.buffer <- function(p, r)
  {
    stopifnot(length(p) == 1)
    aeqd <- sprintf("+proj=aeqd +lat_0=%s +lon_0=%s +x_0=0 +y_0=0",
                    p@coords[[2]], p@coords[[1]])
    projected <- spTransform(p, CRS(aeqd))
    buffered <- gBuffer(projected, width=r, quadsegs=90,byid=TRUE)
    spTransform(buffered, p@proj4string)
  }
  
  # Make a list, each element of the list has one SpatialPolygons polygon (one circle)
  vec.buff <- list()
  
  t.poly <- proc.time()
  for (poly in 1:nrow(vec)) {
    vec.buff[[poly]] <- aeqd.buffer(vec[poly,], dist.m) 
  }
  t.polydiff <- proc.time()-t.poly
  write(paste0("buffering completed for ", length(vec.buff), " points in ", round(t.polydiff[3], 2), " seconds"), lf, append=T)

  # Get the ID names of each polygon...intially they are all the same, but each polygon needs a unique ID:
  IDs <- sapply(vec.buff, function(x) slot(slot(x, "polygons")[[1]], "ID"))
  # Give each polygon a unique ID  (1: number of list elements)
  Spol1 <- SpatialPolygons(lapply(1:length(vec.buff), function(x) {
    Pol <- slot(vec.buff[[x]], "polygons")[[1]]
    slot(Pol, "ID") <- as.character(x)
    Pol } ) )
  # Get the IDs, and set the row.names of the polygons as these IDs
  IDs = sapply(slot(Spol1, "polygons"), function(x) slot(x, "ID"))
  row.names(Spol1) = IDs
  #As a check, this should be true now:
  length(unique(IDs)) == length(vec.buff)
  #----------------------------
  # Build a data.frame to attach to the SpatialPolyons
  data.df = data.frame(IDs,vec);  row.names(data.df)=IDs
  spol.df = SpatialPolygonsDataFrame(Spol1,data = data.df)
  projection(spol.df) = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  #assign the final variable, "polys.wgs84.35m" as the spatial polygons dataframe:
  polys.wgs84=spol.df
  writePolyShape(polys.wgs84, fn = outBuff)
  
  # Get image/raster data
  write(paste0("opening raster data: ", inImg), lf, append=T)  
  myBands <- brick(inImg)
  n <- nlayers(myBands)
  
  # Extract the data, specifying number of layers (automatically extracts all bands for now):
  T1 <- proc.time()
  write(paste0("extracting ", n, " bands of predictor data for ", nrow(polys.wgs84), " plots."), lf, append=T)
  extract.all = extract(myBands,polys.wgs84,nl=n,weights=TRUE)
  write(paste0("finished extraction in ", round((proc.time()[3]-T1[3])/60,2), " minutes."), lf, append=T)
  
  # Compute the weighted means of the landsat bands for each glas shot
  # Here, the weights are the *proportions* of each pixel inside the glas shot. 
  # length(extract.all) should give you the total number of glas shots
  # The lapply deals with the list of dataframes. The "apply applies the weighted mean 
  # function to each column in the dataframe.  
  weighted.means <- lapply(extract.all, function(x) apply(x, 2, weighted.mean, w=x[,n+1], na.rm=F))
  npix <- unlist(lapply(extract.all, nrow))
  results <- as.data.frame(do.call("rbind", weighted.means))[,(1:n)]
  results <- cbind(npix, data.df[[responseCol]], results)
  names(results) <- c("npix", responseCol,predNames)
  
  #Write out the mddb file
  cat('Writing MDDB file\n')
  write("Writing MDDB file. . .", lf, append=T)
  write.csv(results,file=outMDDB,row.names=FALSE)
  
  if (!file.exists(outMDDB)) {
    write(paste0("ERROR: MDDB csv did write to disk: ", outMDDB), lf, append=T)
  } else {
    write(paste0(outMDDB, ": file created."), lf, append=T)
  }

  write("finished", lf, append=T)  
  return(results)
} # end writeMDDB_multPixelsPerPlot function
  
                                    

