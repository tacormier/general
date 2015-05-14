# Author: Tina Cormier
# Date: February, 2015
# Purpose: Simple script to stack scaling rasters into single files (also mult by 100 and round of decimal 
# So we can write out as 16 bit images instead of floats). 
#
# Runs via wrapper script called /mnt/a/tcormier/scripts/general/R/loop_group_stack.sh

# Packages:
library(raster)

#Set tmpdir
rasterOptions(tmpdir="/home/tcormier/RasTmpDir/")

# Read in arguments
args <- commandArgs(trailingOnly=TRUE)
infile <- args[1]
outdir <- args[2]

#######################################
# Variables for running manually:
#indir <- '/mnt/a/mfarina/biomass_scale/scale_inputs/res_180m/'

# Output directory - should have resolution directories beneath it. 
#outdir <- "/mnt/a/tcormier/Mexico_CMS/scale/"
#######################################
#setwd(indir)

#list files to stack
dir <- dirname(infile)
loc <- strsplit(strsplit(infile, "/")[[1]][8], "_")[[1]][2]
p <- glob2rx(paste0("*_",loc,"_*.tif$"))
files <- list.files(dir, pattern=p, full.names=T)

#split <- function(s) strsplit(strsplit(s, "/")[[1]][8], "_")[[1]][2]
#grps <- unique(sapply(files, split))

#outfile naming
res.a <- regexpr(pattern = '_[0-9]+m', files[1])
res <- regmatches(files[1], res.a)
outfile <- paste0(outdir, "res", res, "/", unlist(strsplit(basename(files[1]), "\\."))[1], "_stack.tif")
#don't want to mult DEM by 100 - will force us to write 32 bit raster, and we don't need that precision for elev.
print(paste0("Stacking files for the following state: ", loc, ". Inputs are: ", files))
stk <- round((stack(files) * c(100,100,100,100,1,100,100)),digits = 0)

# stk1 <- round((stack(files[1]) * c(100,100,100,100,1)),digits = 0)
# stk2 <- round((stack(files[2]) * 100),digits = 0)

#little quality check:
if (nlayers(stk) == 7) {
  writeRaster(stk, filename = outfile, overwrite=T, datatype='INT2S')
  #writeRaster(stk, filename = outfile, overwrite=T)
  
} else {
  stop(paste0("ERROR in: ", outfile, " - Stack does not have 7 layers. It has ", nlayers(stk)))
}

  

