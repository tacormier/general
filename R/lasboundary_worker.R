# Script gets variables from lasboundary_wrapper.R.
# Expecting txt, tbl, outshp.

Args <- commandArgs(trailingOnly=TRUE)
print(Args)

vars <- Args[1]
# load variables
load(vars)

# lasboundary call
las.bnd <- paste0("/mnt/a/tcormier/LAStools/bin/lasboundary.exe -lof ", txt, " -thin_with_grid 5 -disjoint -merged -utm ", tbl$Zone_Num, "N -o ", outshp)
system(las.bnd)
