#!/bin/bash
# Written by jesse bishop - borrowed/modified by Tina Cormier :)
outdir=/mnt/a/tcormier/MREDD/for_Ale/tiles/madmex/
tileID="${raster/lhv_/}"
tile=madmex_lcc_landsat_2008_${tileID}

tr=$(gdalinfo $raster | grep 'Pixel Size' | awk -F ' = ' '{print $2}' | tr -d '()-' | sed 's/,/ /g')
ll=$(gdalinfo $raster | grep 'Lower Left' | grep  -o "\( \?-\?[0-9]\{1,3\}\.[0-9]*,[ ]*-\?[0-9]\{1,2\}\.[0-9]*\)" | tr -d '() ' | sed 's/,/ /g')
ur=$(gdalinfo $raster | grep 'Upper Right' | grep  -o "\( \?-\?[0-9]\{1,3\}\.[0-9]*,[ ]*-\?[0-9]\{1,2\}\.[0-9]*\)" | tr -d '() ' | sed 's/,/ /g')

# Warp the data to the extent and resolution of the segment raster
# Slope & elev
gdalwarp -te $ll $ur -tr $tr -co "COMPRESS=LZW" -of GTiff /mnt/p/mexico/madmex/Mosaics_4.3/madmex_lcc_landsat_2008_v4.3.tif $outdir/${tile}


#loop over them and set each one as $raster