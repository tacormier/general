# This script calls the /mnt/a/tcormier/scripts/general/R/FUSION_extractPlots.R and submits each plot 
# from each polygon file in paramfile to the cluster as a separate job.
#
library(rgdal)
library(raster)
library(rgeos)
library(stringr)
source("/mnt/a/tcormier/scripts/general/R/handy_functions_TC.R")

# Set env variables for submitting jobs to grid engine
Sys.setenv(SGE_ROOT = "/net/share-2/export/HomeDir/sge6.2/")
Sys.setenv(SGE_CELL="Grid-Cell-01")

############# Variables #################
# csv file containing parameters for each run. Field names must be polyPath, ID_field_num, and lasindex file (shp). 
# So this table is a list of shapefiles and the accompanying las indices.

# Example file: /mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/las_extract/FUSION_extractPlots_params/extract_infys_20150918.csv
# paramfile <- "/mnt/a/tcormier/Mexico_CMS/lidar/G-LiHT/field_lidar_intersect/FUSION_extractPlots_params_20151110/G-LiHT_params_20151110.csv"
paramfile <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/las_extract/FUSION_extractPlots_params/extract_infys_20160127_forSG.csv"
# 
# outdir <- "/mnt/a/tcormier/Mexico_CMS/lidar/G-LiHT/field_lidar_intersect/" Does not need to exist.
outdir <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/las_extract/extract_20160128_forSG/"

# Do the laslists need to be reformated to run windows tool with wine? (see function documentation)? Y or N?
reform <- 'Y'

# Delete tmpdir Y/N? tmpdir is created to hold all of the individual, plot-specific files. 
deltmp <- 'N'

#where do you want to store the qsub logs? Directory. Does not have to exist already.
QLOG <- "/mnt/a/tcormier/scripts/logs/CMS_lidarExtract/"

##########################################
dir.create(QLOG, showWarnings = F)

# open param file, do some formatting and set up loop
params <- read.csv(paramfile, stringsAsFactors = F)
params$outBase <- paste0(outdir, basename(params$lasindex))
# params$outBase <- paste0(outdir, basename(params$polyPath))
params$outBase <- gsub(".shp", ".las", params$outBase)
dir.create(outdir)

# We had some memory issues with submitting groups of plots to polyClipData, so let's
# create a temp directory and write out individual shapefiles for each plot, then 
# figure out which lasfiles intersect each plot. 
# First, loop over each polygon in param file

# Create a temp dir to store individual plots
tmpdir <- paste0(outdir, "tmp_indivPlots/")
dir.create(tmpdir)

# Loop over each shapefile
for (p in (1:length(params$polyPath))) {
  # strip ".shp" from paths
  li <- unlist(str_split(params$lasindex[p], "\\."))[1]
  ps <- unlist(str_split(params$polyPath[p], "\\."))[1]
  id.col <- unlist(str_split(params$ID_field_num[p], "\\."))[1]
  # open the two shapefiles
  lasindex <- readOGR(dirname(li), basename(li))
  plots.all <- readOGR(dirname(ps), basename(ps))
  
  # We only want to work with plots that intersect the lasfiles we have in the index, not
  # the whole shapefile of potentially thousands of plots.
  plotsInLas <- over(lasindex, plots.all)
  # plotsInLas results in a simple dataframe, not a spatial object. Need to
  # just pull the identified plots from the shapefile
  plots <- plots.all[plots.all@data$FOLIO %in% plotsInLas$FOLIO,]
  
  # loop over each plot
  for (pl in (1:nrow(plots))) {
    p.indiv <- plots[pl,]
    #id <- as.character(p.indiv@data$FOLIO)
    id <- as.character(p.indiv@data[,params$ID_field_num[1]])
    newfile <- paste0(tmpdir,basename(ps), "_", id, ".shp")
    newoutbase <- paste0(outdir, basename(li), ".las")
    
    # now intersect plot with las index to see which lasfiles we need
    intras <- raster::intersect(lasindex, p.indiv)
    
    # If the plot does not intersect the las file, skip to the next plot (this is only necessary if you haven't intersected the
    # plots with the lasindex)
    if (is.null(intras)) next()
    
    writeOGR(p.indiv, tmpdir, paste0(basename(ps), "_", id), driver="ESRI Shapefile", check_exists = T, overwrite_layer = T)
    
    # Now write list of intersection las files to new laslist
    laslist <- intras@data$location
    outlist <- paste0(tmpdir, basename(ps), "_", id, "_lasIntersect.txt")
    write.table(laslist, outlist, row.names=F, col.names=F, quote=F)
    
    # Finally, write new param file into tmpdir for just this indiv plot
    param.indiv <- data.frame(polyPath=newfile, ID_field_num=params$ID_field_num[p], lasindex=outlist, outBase=newoutbase, stringsAsFactors = F)
    pi.name <- paste0(tmpdir, unlist(str_split(basename(paramfile), "\\."))[1], "_", id, ".csv")
    write.table(param.indiv, pi.name, sep=",", quote=F, row.names=F)
    
    # Reformat table for windows tool?
    if (reform == "Y") {
      ll <- scan(param.indiv$lasindex,what="character")
      outname <- paste0(unlist(strsplit(param.indiv$lasindex, "\\."))[1], "_wine.txt")
      # now replace original name with name of reformatted file
      param.indiv$lasindex <- outname
      
      # perform the reformatting and write out new file
      ll.ref <- gsub("/mnt", "Z:/mnt", ll)
      ll.ref <- gsub("/", "\\\\\\\\", ll.ref)
      
      write.table(ll.ref, outname, quote = F, row.names = F, col.names = F)
    }# end reform if
    
    # Submit job to cluster
    jobname <- paste0(unlist(strsplit(basename(newoutbase), "\\."))[1], "_", id)
    sys.call <- paste("/net/share-2/export/HomeDir/sge6.2/bin/lx24-amd64/qsub -b yes -q nondev.q -V -N", jobname, "-l rdisk=2G", "-o",QLOG, "-e", QLOG, 
                      "/mnt/s/bin/jqsub 'R --vanilla --slave < /mnt/a/tcormier/scripts/general/R/FUSION_extractPlots.R' --args", 
                      param.indiv$polyPath[1], param.indiv$ID_field_num[1], param.indiv$lasindex[1], param.indiv$outBase[1])
    system(sys.call)
    
    # If you'd rather just run it from here, here are the lines:
    # now make some tweaks to the file paths in param.indiv to allow this windows tool to run on linux
    # NOTE: When doing this same line on "params" df, I get the right format out the other side. BUT, with a df with
    # only one row, my results are transposed?? To fix, transpose it back - but use this with CAUTION!
#     param.indiv2 <- data.frame(t(apply(param.indiv, 2, function(x) gsub("/", "\\\\\\\\", x))), stringsAsFactors = F)
#     FUSION_polyclipdata(polyPath=param.indiv2$polyPath[1], fieldNum=param.indiv2$ID_field_num[1], outBase=param.indiv2$outBase[1], lasList=param.indiv2$lasindex[1])

  } # end plot loop
} # end poly loop

if (deltmp == "Y") {
  unlink(tmpdir)
  }

