# Expects to receive rdata file with input raster (inras) and output name (outfile).

Args <- commandArgs(trailingOnly=TRUE)
print(Args)

vars <- Args[1]
load(vars)

##########################

# Hillshade call
hs.cmd <- paste("/mnt/s/gdal-1.10.1/bin/gdaldem hillShade", inras, outfile)
system(hs.cmd)
