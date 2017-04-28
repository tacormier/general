# This script calls the /mnt/a/tcormier/scripts/general/R/LAStools_extractPlots.R and submits each plot 
# from each polygon file in paramfile to the cluster as a separate job.
#
# library(rgdal)
library(raster)
library(rgeos)
library(maptools)
library(stringr)
source("/mnt/a/tcormier/scripts/general/R/handy_functions_TC.R")

# Set env variables for submitting jobs to grid engine
Sys.setenv(SGE_ROOT = "/net/share-2/export/HomeDir/sge6.2/")
Sys.setenv(SGE_CELL="Grid-Cell-01")

############# Variables #################
# csv file containing parameters for each run. Field names must be polyPath, ID_field_name, and lasindex file (shp). 
# So this table is a list of shapefiles and the accompanying las indices.
# Example file: 
# paramfile <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/las_extract/FUSION_extractPlots_params/extract_infys_20150918.csv"
# paramfile <- "/mnt/a/tcormier/Mexico_CMS/lidar/G-LiHT/field_lidar_intersect/FUSION_extractPlots_params_20151110/G-LiHT_params_20151110.csv"
# paramfile <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/las_extract/FUSION_extractPlots_params/extract_workshopArea_20160401.csv"
paramfile <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/las_extract/FUSION_extractPlots_params/extract_CMS_allPlots_20170314.csv"
# 
# Minimum area of lidar coverage within plot (as a percentage)
min.area <- 95

# outdir <- "/mnt/a/tcormier/Mexico_CMS/lidar/G-LiHT/field_lidar_intersect/" Does not need to exist.
# outdir <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170215_LAStools/"
outdir <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170314_LAStools/not_norm_unfil/"

# Do the laslists need to be reformated to run windows tool with wine? (see function documentation)? Y or N?
reform <- 'N'

# Delete tmpdir Y/N? tmpdir is created to hold all of the individual, plot-specific files. 
deltmp <- 'N'

#where do you want to store the qsub logs? Directory. Does not have to exist already.
QLOG <- "/mnt/a/tcormier/scripts/logs/CMS_lidarExtract_LAStools_20170314/"

##########################################
# Make some directories
dir.create(QLOG, showWarnings = F)
dir.create(outdir, showWarnings = F)
# Directory for incomplete plots
incomp.dir <- paste0(outdir, "/incomplete_plots/")
dir.create(incomp.dir, showWarnings = F)
# Directory for complete plots
comp.dir <- paste0(outdir, "/complete_plots/")
dir.create(comp.dir, showWarnings = F)

# open param file, do some formatting and set up loop
params <- read.csv(paramfile, stringsAsFactors = F)
params$outBase <- paste0(outdir, basename(params$lasindex))
# params$outBase <- paste0(outdir, basename(params$polyPath))
params$outBase <- gsub(".shp", ".las", params$outBase)


# We had some memory issues with submitting groups of plots to polyClipData, so let's
# create a temp directory and write out individual shapefiles for each plot, then 
# figure out which lasfiles intersect each plot. 
# First, loop over each polygon in param file

# Create a temp dir to store individual plots
tmpdir <- paste0(outdir, "tmp_indivPlots/")
dir.create(tmpdir, showWarnings=F)

# Loop over each shapefile
for (p in (1:length(params$polyPath))) {
  li <- params$lasindex[p]
  ps <- params$polyPath[p]
  colname.id <- params$ID_Field_Name[p]
  print(li)
  print(ps)
  
  # open the two shapefiles
  lasindex <- shapefile(li)
  plots.all <- shapefile(ps)
  id.col <- which(colnames(plots.all@data) %in% colname.id) 
  
  # We only want to work with plots that intersect the lasfiles we have in the index, not
  # the whole shapefile of potentially thousands of plots.
  # plotsInLas <- over(lasindex, plots.all)
  plotsInLas <- raster::intersect(lasindex, plots.all)
  
  # If there are no plots that intersect, go to the next one
  if (is.null(plotsInLas))  next()
  
  # If plots are on the edges of lastiles (i.e. the entire plot area is not fully covered by lidar data, whether by one lastile or with multiple),
  # we need to exclude them. So, first dissolve polygons with the same ID (plots in las - dissolved = pid).
  pid <- gUnaryUnion(plotsInLas, id = plotsInLas@data[[colname.id]])
  
  # Now calculate the area of each intersected/dissolved polygon and compare it with the origin plot size (did any of it get cut off? Is any of it
  # outside the lidar coverage?)
  # Extract areas from polygon objects then attach as attribute
  areas <- data.frame(area=sapply(pid@polygons, FUN=function(x) {slot(x, 'area')}))
  row.names(areas) <- sapply(pid@polygons, FUN=function(x) {slot(x, 'ID')})
  areas$pid_ID <- row.names(areas)

  # Now let's look at the original plots (not clipped/intersected) that were identified as overlapping
  # the lidar and compare the area of each plot with the area of the intersected plot. If intersected plot is
  # less, then we don't have full lidar coverage for that plot.
  plots <- plots.all[plots.all@data[[colname.id]] %in% plotsInLas[[colname.id]],]
  areaInt <- areas[match(plots@data[[colname.id]], row.names(areas)),]
  # Manual check - do IDs match up? Can code this as an automatic check later.
  areaInt <- cbind(areaInt, orig_ID=plots@data[[colname.id]])
  
  # Now get areas of original plots
  p.areas <- data.frame(area=sapply(plots@polygons, FUN=function(x) {slot(x, 'area')}))
  row.names(p.areas) <- sapply(plots@polygons, FUN=function(x) {slot(x, 'ID')})
  # Combine attributes info and areas 
  plots <- spCbind(plots, p.areas)
  areaInt <- cbind(areaInt, orig_area=plots@data$area)
  areaInt$area_diff <- round((areaInt$orig_area - areaInt$area), 2)
  
  # ID incompletely covered plots (% coverage is less than min.area)
  areaInt$perc_cov <- round((areaInt$area/areaInt$orig_area)*100,2)
  
  # Now remove plots < min.area 
  plots.complete <- plots[plots@data[[colname.id]] %in% areaInt$pid_ID[areaInt$perc_cov >= min.area],]
 
  # ****Also write shapefile of excluded plots - for manual checking that everything is going as expected.
  inc.pos <- !(plots@data[[colname.id]] %in% areaInt$pid_ID[areaInt$perc_cov >= min.area])
  if (TRUE %in% inc.pos) {
    plots.incomplete <- plots[!(plots@data[[colname.id]] %in% areaInt$pid_ID[areaInt$perc_cov >= min.area]),]
    shapefile(plots.incomplete, paste0(incomp.dir, "incomplete_plots_", basename(li)), overwrite = T)
  }
  
  if (nrow(plots.complete) == 0) next()
    
    shapefile(plots.complete, paste0(comp.dir, "complete_plots_", basename(li)) , overwrite = T)
  
  # loop over each plot
  for (pl in (1:nrow(plots.complete))) {
    p.indiv <- plots.complete[pl,]
    #id <- as.character(p.indiv@data[[colname.id]])
    id <- as.character(p.indiv@data[,params$ID_Field_Name[p]])
    newfile <- paste0(tmpdir, "TCID_", id, "_", stripExtBase(ps), "_", stripExtBase(li),".shp")
    newoutbase <- paste0(outdir, "TCID_", id, "_", stripExtBase(ps), "_", stripExtBase(li), ".las")
    
    # now intersect plot with las index to see which lasfiles we need
    intras <- raster::intersect(lasindex, p.indiv)
    
    # If the plot does not intersect the las file, skip to the next plot (this is only necessary if you haven't intersected the
    # plots with the lasindex)
    if (is.null(intras)) next()
    
    p.indiv.out <- paste0(tmpdir, "TCID_", id, "_", stripExtBase(ps), "_", stripExtBase(li), ".shp")
    shapefile(p.indiv, p.indiv.out, overwrite=T)
    # Now write list of intersection las files to new laslist
    laslist <- intras@data$location
    # HACK - fix double //
    laslist <- gsub("//", "/", laslist)
    outlist <- paste0(tmpdir, "TCID_", id, "_", stripExtBase(ps), "_", stripExtBase(li), "_lasIntersect.txt")
    write.table(laslist, outlist, row.names=F, col.names=F, quote=F)
    
    # Finally, write new param file into tmpdir for just this indiv plot
    param.indiv <- data.frame(polyPath=newfile, ID_field_num=id.col, lasindex=outlist, outBase=newoutbase, stringsAsFactors = F)
    pi.name <- paste0(tmpdir, "TCID_", id, "_", stripExtBase(paramfile), "_", stripExtBase(li), "_params.csv")
    write.table(param.indiv, pi.name, sep=",", quote=F, row.names=F)
    
    # Reformat table for windows tool?
    if (reform == "Y") {
      ll <- scan(param.indiv$lasindex, what="character")
      outname <- paste0(tmpdir, stripExtBase(param.indiv$lasindex), "_wine.txt")
      # now replace original name with name of reformatted file
      param.indiv$lasindex <- outname
      
      # perform the reformatting and write out new file
      ll.ref <- gsub("/mnt", "Z:/mnt", ll)
      ll.ref <- gsub("/", "\\\\\\\\", ll.ref)
      
      write.table(ll.ref, outname, quote = F, row.names = F, col.names = F)
    }# end reform if
    
    # Submit job to cluster
    jobname <- stripExtBase(newoutbase)
    sys.call <- paste("/net/share-2/export/HomeDir/sge6.2/bin/lx24-amd64/qsub -b yes -q nondev.q -V -N", jobname, "-l rdisk=2G", "-o",QLOG, "-e", QLOG, 
                      "/mnt/s/bin/jqsub 'R --vanilla --slave < /mnt/a/tcormier/scripts/general/R/LAStools_extractPlots.R' --args", 
                      param.indiv$polyPath[1], param.indiv$lasindex[1], param.indiv$outBase[1])
    system(sys.call)
    
    # If you'd rather just run it from here, here are the lines:
    # now make some tweaks to the file paths in param.indiv to allow this windows tool to run on linux
    # NOTE: When doing this same line on "params" df, I get the right format out the other side. BUT, with a df with
    # only one row, my results are transposed?? To fix, transpose it back - but use this with CAUTION!
#     param.indiv2 <- data.frame(t(apply(param.indiv, 2, function(x) gsub("/", "\\\\\\\\", x))), stringsAsFactors = F)
#     FUSION_polyclipdata(polyPath=param.indiv2$polyPath[1], fieldNum=param.indiv2$ID_field_num[1], outBase=param.indiv2$outBase[1], lasList=param.indiv2$lasindex[1])
    
    }# end plot loop
} # end poly loop

if (deltmp == "Y") {
  unlink(tmpdir)
  }

