# Script to tile las files using las2tiles from handy_functions_TC.R
source("/mnt/a/tcormier/scripts/general/R/handy_functions_TC.R")

# directory containing all las files to tile - currently, this script assumes
# there is one directory. Can rewrite to use paramter file or list of directories.
lasdir <- "/mnt/a/tcormier/SE_Asia/Ellis_Paper/lidar/Amindo/"
# numeric value representing the desired tile size in meters
tilesize <- "20"
# lidar file format, usually "las"
rawFormat <- "las"
# UTM zone and hemisphere (e.g., '50N')
utmZone <- '50N'
# identifier for the site (i.e. "Amindo", "Site1")
siteID <- "Amindo"
# do you want to keep the tiled las files (which would be duplicates of the txt files 
# created); requires capital T or F.
rm.tmp <- T

###############################

# Run function - will write tiles to disk.
las2tiles(lasdir, tilesize, rawFormat='las', utmZone, siteID, rm.tmp=T)