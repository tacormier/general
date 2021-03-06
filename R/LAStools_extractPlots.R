# This script calls the FUSION_polyclipdata function from handy_functions.
library(rgdal)

source("/mnt/a/tcormier/scripts/general/R/handy_functions_TC.R")

# Get command line arguments 
Args <- commandArgs(trailingOnly=TRUE)

print(Args)
polyPath <- Args[1]
# fieldNum <- Args[2]
lasList <- Args[2]
outBase <- Args[3]

####################################
# now make some tweaks to the file paths in param.indiv to allow this windows tool to run on linux
# Because this was passed from another R script, need to fix our slashes before running the function
# print(paste0("Before slash change: ", outBase))
# polyPath <- gsub("/", "\\\\\\\\", polyPath)
# lasList <- gsub("/", "\\\\\\\\", lasList)
# outBase <- gsub("/", "\\\\\\\\", outBase)
# print(paste0("After slash change: ", outBase))

# Run function
lasclip.cmd <- paste0("/mnt/a/tcormier/LAStools/bin/lasclip.exe -lof ", lasList, " -poly ", polyPath, " -o ", outBase, " -oparse xyzianrc -merged -v")
print(" ")
print(lasclip.cmd)
print(" ")
system(lasclip.cmd)
