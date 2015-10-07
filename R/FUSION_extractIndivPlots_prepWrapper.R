# This script calls the FUSION_polyclipdata function from 
# ***When you have time...REORGANIZE this a bit such that it submits each line of the param file as
# a job to the cluster.
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
paramfile <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/las_extract/FUSION_extractPlots_params/extract_infys_20150918.csv"
# 
outdir <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/las_extract/extract_20150918/"
# Do the laslists need to be reformated to run windows tool with wine? (see function documentation)? Y or N?
reform <- 'Y'

# Delete tmpdir Y/N? tmpdir is created to hold all of the individual, plot-specific files. 
deltmp <- 'N'

#where do you want to store the qsub logs? Directory.
QLOG <- "/mnt/a/tcormier/scripts/logs/CMS_lidarExtract/"

##########################################
dir.create(QLOG, showWarnings = F)

# open param file, do some formatting and set up loop
params <- read.csv(paramfile, stringsAsFactors = F)
params$outBase <- paste0(outdir, basename(params$polyPath))
params$outBase <- gsub(".shp", ".las", params$outBase)
dir.create(outdir)

# We had some memory issues with submitting groups of plots to polyClipData, so let's
# create a temp directory and write out individual shapefiles for each plot, then 
# figure out which lasfiles intersect each plot. 
# First, loop over each polygon in param file

# Create a temp dir to store individual plots
tmpdir <- paste0(outdir, "tmp_indivPlots/")
dir.create(tmpdir)

for (p in (1:length(params$polyPath))) {
  # strip ".shp" from paths
  li <- unlist(str_split(params$lasindex[p], "\\."))[1]
  ps <- unlist(str_split(params$polyPath[p], "\\."))[1]
  # open the two shapefiles
  lasindex <- readOGR(dirname(li), basename(li))
  plots <- readOGR(dirname(ps), basename(ps))
  
  #test <- raster::intersect(lasindex, plots)
  
  # loop over each plot
  for (pl in (1:nrow(plots))) {
    p.indiv <- plots[pl,]
    id <- as.character(p.indiv@data$FOLIO)
    newfile <- paste0(tmpdir,basename(ps), "_", id, ".shp")
    newoutbase <- paste0(outdir, basename(ps), ".las")
    
    writeOGR(p.indiv, tmpdir, paste0(basename(ps), "_", id), driver="ESRI Shapefile", check_exists = T, overwrite_layer = T)
    # now intersect plot with las index to see which lasfiles we need
    intras <- raster::intersect(lasindex, p.indiv)
    
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
    jobname <- unlist(strsplit(basename(param.indiv$polyPath), "\\."))[1]
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

