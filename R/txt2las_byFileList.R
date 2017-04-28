# source("/mnt/a/tcormier/scripts/general/R/handy_functions_TC.R")
#######################################################################
# rdata file for testing
vars <- "/mnt/r/Mex_Lidar/G_LiHT/AMIGACarb_AM_Guan_Chihuahua_GLAS_May2013/lidar/las//Tiles_30m//temp/81.RDATA"
# Get command line arguments 
Args <- commandArgs(trailingOnly=TRUE)
print(Args)

vars <- Args[1]
load(vars)
#######################################################################

txt2las.cmd <- paste0("/mnt/a/tcormier/LAStools/bin/txt2las.exe -lof ", lof, " -parse xyzianrc ", "-odir ", odir)
system(txt2las.cmd)
