
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
# function to produce pseudo-waveforms. The function makes several adjustments to the elevation
# values, including subtracting the DTM values to account for slope, then finding the ground peak, 
# removing anything beneath it, and adjusting/triming the other heights relative to the ground (Fabio's code).
# lasdata = las data table/df, res = vertical resolution (m), dtm = high res dtm (raster) for removing 
# slope effects (optional), max.height is the largest height to consider in metrics and is determined by the user
# based on field data or expert knowledge; heights taller than this value are flagged. min.height is
# lowest height considered in the metrics and is determined by the user based on field data or expert knowledge; 
# lower heights are flagged.

makeprof <- function(lasdata, res, dtm=NULL, max.height=NULL, min.height) { 
  #remove rows with no elevation if they exist
  naid <- which(is.na(lasdata$z))
  if (length(naid) > 0) lasdata <- lasdata[-naid,]
  
  #remove topography using DTMs if one is specified
  if (!is.null(dtm)) {
    ptground <- extract(dtm, lasdata[,1:2])
    naid2 <- which(is.na(ptground))
    
    # remove rows where z=NA after extracting, then subtract DTM 
    # (i.e. where DTM = NA)
    if (length(naid2) > 0) {
      ptground <- ptground[-naid2]
      lasdata <- lasdata[-naid2,]
    }
    lasdata$z <- lasdata$z - ptground
  } # end dtm if

  # If there are no rows left after subracting slope:
  if (nrow(lasdata) == 0) {
    profile <- data.frame(height=NA, counts=NA, intensity=NA) 
    genergy <- NA
    tenergy <- NA
    gmax0c <- NA
    log.val5 <- 0
    fgt <- 0
    fltm <- 0
    return(list(profile, gmax0c, genergy, tenergy, fgt, fltm, log.val5)) 
    }
  
  # Tile quality check/flag generation/filtering
  fgt <- length(lasdata$z[lasdata$z > max.height]) / length(lasdata$z) * 100 #82 m = 20% more than the maximum height measured in the field (i.e. 68 m)
  if (fgt > 1) {
    warning("More than 1% of normalized heights are greater than max.height") #warning("More than 1% of normalized heights > 82 m")
  } else {
    lasdata <- lasdata[lasdata$z <= max.height,] # less than 1% of points violate the max.height condition, so just remove those points.
    }
  
  fltm <- length(lasdata$z[lasdata$z < min.height]) / length(lasdata$z) * 100
  if (fltm > 1) {
    warning("More than 1% of normalized heights are less than min.height") #warning("More than 1% of normalized heights < -3 m")
  } else {
    lasdata <- lasdata[lasdata$z >= min.height,] # less than 1% of points violate the min.height condition, so just remove those points.
  }
  
  # If either of above flags are violated, this tile should not be used.
  # Break from function and return their values
  if (fgt > 1 | fltm > 1) {
    profile <- data.frame(height=NA, counts=NA, intensity=NA) 
    genergy <- NA
    tenergy <- NA
    gmax0c <- NA
    log.val5 <- 0
    return(list(profile, gmax0c, genergy, tenergy, fgt, fltm, log.val5))  
    } # end fgt/fltm if
  
  # If all is well, continue with profile
  elev <- lasdata$z
  elev0 <- elev - min(elev) #note that 0 here is the lowest elevation, not the ground peak
  int <- lasdata$i
  clas <- lasdata$c #2 - ground, 3 - low vegetation, 4 - medium vegetation, 5 - high vegetation, etc
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
  
  # profile df
  profile <- data.frame(height=z, counts=counts, intensity=int_sum)
  
  gmax0 <- max(lasdata$z[lasdata$c==2], na.rm=T) - min(lasdata$z)
  #profile$counts <- profile$counts/max(profile$counts[profile$height > gmax0]) #normalize lidar profile by maximum canopy return
  genergy <- sum(profile$counts[profile$height <= gmax0])
  tenergy <- sum(profile$counts)
  if (gmax0 < profile$height[1]) {
    log.val5 <- 1
    peakid <- 1
  } else {
    peakid <- which(profile$counts == max(profile$counts[profile$height <= gmax0])) #find ground peak
    if (length(peakid) > 1) peakid <- peakid[1]
    log.val5 <- 0
  }
  
  # Adjust heights relative to ground
  profile <- profile[peakid:nrow(profile),] #trim profile
  gmax0c <- gmax0 - min(profile$height) #correct max ground height
  profile$height <- profile$height - min(profile$height) #correct heights
  
  #return adjusted profile
  return(list(profile, gmax0c, genergy, tenergy, fgt, fltm, log.val5))
}

#

#####################################

# This function makes a plot of a lidar profile and requires ggplot2.
# waveformDF = resulting DF after executing makeprof function; nameHeightCol = name of column containing heights;
# nameCountCol = name of column containing counts; smoothFactor controls the degree of waveform line smoothing 
# (should generally be less than 1 to really show peaks in lidar data, but test different factors!).
# title = optional text for plot title; leg.txt = optional text to be printed at the bottom right of the 
# plot (e.g., Field collected height and/or biomass); ylim is the limits of the y-axis and consists of
# two numbers (e.g., ylim=c(0,75)); lineType = do you want a smoothed line ("smooth"), or the exact line ("exact")? Default
# is smooth.
#
plotWaveform <- function(waveformDF, nameHeightCol, nameCountCol, smoothFactor=0.25, plot.title=NULL, leg.txt=NULL, ylim, xlim, lineType="smooth") {
  require(ggplot2)
  wave <- waveformDF
  #calculate the smooth line
  if (lineType == "smooth") {
    smooth_vals = predict(loess(get(nameCountCol)~get(nameHeightCol), wave, span=smoothFactor, surface="direct"),wave[[nameHeightCol]])
    #smooth_vals = smooth.spline(wave[[nameHeightCol]],wave[[nameCountCol]], spar=.2)
    sm.line <- as.data.frame(cbind(smooth_vals, wave[[nameHeightCol]]))
    #sm.line <- as.data.frame(cbind(smooth_vals[[2]], smooth_vals[[1]]))
    names(sm.line) <- c("counts", "height")
  
    p <- ggplot(wave, aes(counts, height)) + ylim(ylim) + xlim(xlim) + geom_point()  + geom_path(data=sm.line, col="chartreuse4", size=0.5) + 
      theme_bw() + theme(legend.position="none") 
    
    # to include points too!
    #+ geom_point(size=1.5) 
  
    } else if (lineType == "exact") {
      p <- ggplot(wave, aes(counts, height)) + ylim(ylim) + xlim(xlim) + geom_path(data=wave, col="chartreuse4", size=0.5) + 
        theme_bw() + theme(legend.position="none") 
      
      # to include points too!
      #+ geom_point(size=1.5) 
      
  } else {
    stop(paste0("The lineType argument must be either 'smooth' or 'exact.' You entered '", lineType, "'. Please try again."))
  }
    
  # If we want to use a color gradient on the line! Would change the color in geom_path to aes(colour=wave$height)
  # + scale_colour_gradient2(low="tan4", mid="darkkhaki", high="chartreuse4",midpoint=sm.line$height[median(sm.line$counts)]) +
  
  if (!is.null(plot.title)) {
    p <- p + ggtitle(plot.title) 
  }
  
  if (!is.null(leg.txt)) {
    #     gtxt <- grobTree(textGrob(leg.txt, x=.75,  y=0.1, hjust=0,vjust=0,
    #                            gp=gpar(col="gray16")))
    #     p <- p + annotation_custom(gtxt, size=1.5)
    
    p <- p + annotate("text",x=max(xlim),y=min(wave$height),hjust=1.0,vjust=.1,label=leg.txt, size=2.75)
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
# will write a logfile to the same directory as outMDDB - default is TRUE (logfile). Write logfile 
#
# REQUIRES the following packages: raster, maptools, sp. Other functions: distOK and zerotoNA from /mnt/a/tcormier/scripts/general/R/handy_functions_TC.R
#
# AUTHOR(s): Original script written by Wayne Walker and Alessandro Baccini. Converted to function by Tina Cormier.
#
# Untested with shapefile input as inPlot

writeMDDB_multPlotsPerPixel <- function(inImg, predNames, inPlot, xcol, ycol, outPlotMns, outMDDB, responseCol, minshots, dist, pp, proj.in, logfile=T) {
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
  } else if (ext == "shp" & pp == "points") {
    vec <- readOGR(dirname(inPlot), unlist(strsplit(basename(inPlot), "\\."))[1])
    coords <- as.data.frame(cbind(vec[[xcol]], vec[[ycol]]))
    names(coords) <- c("LON", "LAT")
    vec <- SpatialPointsDataFrame(coords, vec, proj4string = CRS(proj.in))
  } else if (ext == "shp" & pp == "polygons") {
    vec <- readOGR(dirname(inPlot), unlist(strsplit(basename(inPlot), "\\."))[1])
    coords <- as.data.frame(cbind(vec[[xcol]], vec[[ycol]]))
    names(coords) <- c("LON", "LAT")
    vec <- SpatialPolygonsDataFrame(vec, data=data.frame(x=coords$LON, y=coords$LAT, row.names=row.names(vec)))
  } else {
    print("pp must equal 'polygons' or 'points'")
  }# end file ext vec if/else

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
# if we run this more than once (outBuff), pp can be "polygons" or "points" - to indicate if buffering is needed or not, 
# the projection of the input datasets (must be the same for all) in WKT (proj.in), do you want a logfile? accepts T or F (logfile).
#
# REQUIRES the following packages: raster, maptools, rgdal, rgeos
#
# AUTHOR(s): Original script written by Mary Farina. Converted to function by Tina Cormier.
#
# Untested with shapefile input as inPlot

writeMDDB_multPixelsPerPlot <- function(inPlot, xcol, ycol,responseCol, dist.m, outMDDB, inImg, predNames, outBuff, pp, proj.in, logfile=T) {
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
  } else if (ext == "shp" & pp == "points") {
    vec <- readOGR(dirname(inPlot), unlist(strsplit(basename(inPlot), "\\."))[1])
    coords <- as.data.frame(cbind(vec[[xcol]], vec[[ycol]]))
    names(coords) <- c("LON", "LAT")
    vec <- SpatialPointsDataFrame(coords, vec, proj4string = CRS(proj.in))
  } else if (ext == "shp" & pp == "polygons") {
    vec <- readOGR(dirname(inPlot), unlist(strsplit(basename(inPlot), "\\."))[1])
    coords <- as.data.frame(cbind(vec[[xcol]], vec[[ycol]]))
    names(coords) <- c("LON", "LAT")
    vec <- SpatialPolygonsDataFrame(vec, data=data.frame(LON=coords$LON, LAT=coords$LAT, b=vec[[responseCol]],row.names=row.names(vec)))
    names(vec)[3] <- responseCol
  } else {
    print("pp must equal 'polygons' or 'points'")
  }# end file ext vec if/else
  
    #vec <- SpatialPointsDataFrame(coords, vec, proj4string = CRS("+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs"))
  # Create circular polygons with 35m radius around each glas shot (i.e., a shapefile of shot footprints):
  if (pp=="points") {
    write("buffering points by plot radius", lf, append=T)
#     if (proj.in != "+proj=longlat +datum=WGS84 +no_defs") {
#       vec <- spTransform(vec, CRSobj = CRS("+proj=longlat +datum=WGS84 +no_defs"))
#     }
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
    projection(spol.df) = CRS(proj.in)
    #assign the final variable, "polys.wgs84.35m" as the spatial polygons dataframe:
    polys.crs=spol.df
    writePolyShape(polys.crs, fn = outBuff)
  } else {
      data.df <- as.data.frame(vec)
      polys.crs <- vec
  }
  
  # Get image/raster data
  write(paste0("opening raster data: ", inImg), lf, append=T)  
  myBands <- brick(inImg)
  # Some NA value housekeeping
  #myBands[myBands == -9999 | myBands == -32768 | myBands == 32767] <- NA
  n <- nlayers(myBands)
 
  # Extract the data, specifying number of layers (automatically extracts all bands for now):
  T1 <- proc.time()
  write(paste0("extracting ", n, " bands of predictor data for ", nrow(polys.crs), " plots."), lf, append=T)
  extract.all = extract(myBands,polys.crs,nl=n,weights=TRUE)
  write(paste0("finished extraction in ", round((proc.time()[3]-T1[3])/60,2), " minutes."), lf, append=T)
  
  # Compute the weighted means of the landsat bands for each glas shot
  # Here, the weights are the *proportions* of each pixel inside the glas shot. 
  # length(extract.all) should give you the total number of glas shots
  # The lapply deals with the list of dataframes. The "apply" applies the weighted mean 
  # function to each column in the dataframe.  
  
  # first, get positions of null points (no pixels intersected the points)
  extract.nullpos <- unlist(lapply(extract.all, function(x) ifelse(is.null(dim(x)), 0, 1)))
  # remove null values from extract.all and from polys/training data
  extract.valid <- extract.all[extract.nullpos==1]
  
  # Now remove -9999 values from each df in the list - they should not count toward any calcs
  extract.final <- lapply(extract.valid, function(x) ifelse(x==-9999, NA, x))
  
  weighted.means <- lapply(extract.final, function(x) apply(x, 2, weighted.mean, w=x[,n+1], na.rm=F))
  
  # Total number of pixels in each plot
  npix.tot <- unlist(lapply(extract.final, nrow))
  # Number of NA pixels in calculation - the max of all the bands, as NA values might
  # not be in the same location for each band, esp if they come from different sources.
  npix.na <- unlist(lapply(extract.final, function(x) max(colSums(is.na(x)))))
  # Difference - means the number of pixels for which we have complete data in a plot.
  npix <- npix.tot - npix.na
  
  results <- as.data.frame(do.call("rbind", weighted.means))[,(1:n)]
  results <- cbind(npix, data.df[[responseCol]][extract.nullpos==1], results, polys.crs$LON[extract.nullpos==1], polys.crs$LAT[extract.nullpos==1])
  names(results) <- c("npix_nonNA", responseCol,predNames, "X", "Y")
  
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
  
# ################## Stratified Random Sampling ################## 
# stratified = function(df, group, size) {
#   #  USE: * Specify your data frame and grouping variable (as column 
#   #         name) as the first two arguments.
#   #       * Decide on your sample size. For a sample proportional to the
#   #         population, enter "size" as a decimal. For an equal number 
#   #         of samples from each group, enter "size" as a whole number.
#   #
#   #  Example 1: Sample 10% of each group from a data frame named "z",
#   #             where the grouping variable column name is "nameGrpCol", use:
#   # 
#   #                 > stratified(z, "nameGrpCol", .1)
#   #
#   #  Example 2: Sample 5 observations from each group from a data frame
#   #             named "z"; grouping variable column name is "nameGrpCol", use:
#   #
#   #                 > stratified(z, "nameGrpCol", 5)
#   #
#   require(sampling)
#   
#   # column # for group
#   group <- which(names(df) %in% group, arr.ind = T)
#   temp = df[order(df[group]),]
#   # Calc # of samples per group
#   if (size < 1) {
#     nsamp = ceiling(table(temp[group]) * size)
#   } else if (size >= 1) {
#     nsamp = rep(size, times=length(table(temp[group])))
#   }  
#   # Select sample
#   strat = strata(temp, stratanames = names(temp[group]), 
#                  size = as.vector(nsamp), method = "srswor")
#   (dsample = getdata(temp, strat))
# }

# This function came from : https://gist.github.com/mrdwab/6424112
# The arguments to stratified are:
#   
# df: The input data.frame
# group: A character vector of the column or columns that make up the "strata".
# size: The desired sample size.
# If size is a value less than 1, a proportionate sample is taken from each stratum.
# If size is a single integer of 1 or more, that number of samples is taken from each stratum.
# If size is a vector of integers, the specified number of samples is taken for each stratum. It is recommended that you use a named vector. 
# For example, if you have two strata, "A" and "B", and you wanted 5 samples from "A" and 10 from "B", you would enter size = c(A = 5, B = 10).
# select: This allows you to subset the groups in the sampling process. This is a list. For instance, if your group variable was 
# "Group", and it contained three strata, "A", "B", and "C", but you only wanted to sample from "A" and "C", you can use select = list(Group = c("A", "C")).
# replace: For sampling with replacement.                                    
stratified <- function(df, group, size, select = NULL, 
                       replace = FALSE, bothSets = FALSE) {
  if (is.null(select)) {
    df <- df
  } else {
    if (is.null(names(select))) stop("'select' must be a named list")
    if (!all(names(select) %in% names(df)))
      stop("Please verify your 'select' argument")
    temp <- sapply(names(select),
                   function(x) df[[x]] %in% select[[x]])
    df <- df[rowSums(temp) == length(select), ]
  }
  df.interaction <- interaction(df[group], drop = TRUE)
  df.table <- table(df.interaction)
  df.split <- split(df, df.interaction)
  if (length(size) > 1) {
    if (length(size) != length(df.split))
      stop("Number of groups is ", length(df.split),
           " but number of sizes supplied is ", length(size))
    if (is.null(names(size))) {
      n <- setNames(size, names(df.split))
      message(sQuote("size"), " vector entered as:\n\nsize = structure(c(",
              paste(n, collapse = ", "), "),\n.Names = c(",
              paste(shQuote(names(n)), collapse = ", "), ")) \n\n")
    } else {
      ifelse(all(names(size) %in% names(df.split)),
             n <- size[names(df.split)],
             stop("Named vector supplied with names ",
                  paste(names(size), collapse = ", "),
                  "\n but the names for the group levels are ",
                  paste(names(df.split), collapse = ", ")))
    }
  } else if (size < 1) {
    n <- round(df.table * size, digits = 0)
  } else if (size >= 1) {
    if (all(df.table >= size) || isTRUE(replace)) {
      n <- setNames(rep(size, length.out = length(df.split)),
                    names(df.split))
    } else {
      message(
        "Some groups\n---",
        paste(names(df.table[df.table < size]), collapse = ", "),
        "---\ncontain fewer observations",
        " than desired number of samples.\n",
        "All observations have been returned from those groups.")
      n <- c(sapply(df.table[df.table >= size], function(x) x = size),
             df.table[df.table < size])
    }
  }
  temp <- lapply(
    names(df.split),
    function(x) df.split[[x]][sample(df.table[x],
                                     n[x], replace = replace), ])
  set1 <- do.call("rbind", temp)
  
  if (isTRUE(bothSets)) {
    set2 <- df[!rownames(df) %in% rownames(set1), ]
    list(SET1 = set1, SET2 = set2)
  } else {
    set1
  }
}

################## Round up to nearest "to" value ################## 
roundUp <- function(x,to=10) to*(x%/%to + as.logical(x%%to))

################### Multiple Plots Per Page ################## 
# Function directly taken from: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

################# las2tiles ################# 
#
# Write documentation!
#
# See http://www.dmap.co.uk/utmworld.htm for UTM zones as used by lastools
# Make sure las coordinates are in UTM (e.g., '50N').

# siteID can be numeric or text and should identify or name the site (for output
# las file naming). 

las2tiles <- function(lasdir, tilesize, rawFormat, utmZone, siteID, rm.tmp) {
  # make a few directories
  dir.create(paste0(lasdir, "/temp"), showWarnings = F)
  dir.create(paste0(lasdir, "/Tiles_", tilesize, "m"), showWarnings = F)
  dir.create(paste0(lasdir, "/Tiles_", tilesize, "m_geo"), showWarnings = F)
  
  # Get list of las files in lasdir
  p <- glob2rx(paste0("*", rawFormat))
  lasfiles <- list.files(lasdir, p, full.names=T)
  
  # check files (http://rapidlasso.com/2013/04/20/tutorial-quality-checking/)
  info.cmd <- paste0("/mnt/s/LAStools/bin/lasinfo -i ", lasdir, "/*.", rawFormat, " -compute_density")
  system(info.cmd)
  
  # lasvalidate and lasview currently fail on the linux side because they try to open an X 
  # window and can't. Ask Fabio if these lines are necessary. 
  val.cmd <- paste0("wine /mnt/s/LAStools/bin/lasvalidate.exe -i ", lasdir, "/*.", rawFormat, " -oxml")
  system(val.cmd)
#   view.cmd <- paste0("wine /mnt/s/LAStools/bin/lasview.exe -i ", lasdir, "/*.", rawFormat, " -gui")
#   system(view.cmd)
  
  # Merge files before using lastile. lastile could do it all, but it doesn't work sometimes
  merge.cmd <- paste0("/mnt/s/LAStools/bin/lasmerge -i ", lasdir, "/*.", rawFormat, " -odir ", lasdir, " -o merge.laz")
  system(merge.cmd)
  
  # Tile all points from all files using a tile size of tileszie
  tile.cmd <- paste0("/usr/bin/wine /mnt/s/LAStools/bin/lastile.exe -i ", lasdir, "/merge.laz -odir ", lasdir, "/temp -tile_size ", tilesize, " -olas -o ", site, " -rescale 0.01 0.01 0.01")
  system(tile.cmd)
  
  # convert las2txt
  txt.cmd <- paste0("/mnt/s/LAStools/bin/las2txt -i ", lasdir, "/temp/*.las -odir ", lasdir, "/Tiles_", tilesize, "m/ -parse xyzianrc -sep comma")
  system(txt.cmd)
  
  # Make a grid for use in R
  grid.cmd <- paste0("/usr/bin/wine /mnt/s/LAStools/bin/lasgrid.exe -i ", lasdir, "/temp/*.las -odir ", lasdir, "/Tiles_", tilesize, "m_geo -merged -o ", site, "_grid.tif -step ", tilesize, " -utm ", utmZone, " -counter_32bit")
  system(grid.cmd)
  
  # Remove temp dir and merge.laz file
  if (rm.tmp == T) {
    unlink(paste0(lasdir, "/temp/"))
    file.remove(paste0(lasdir, "/merge.laz"))
  }

} # end las2tiles
