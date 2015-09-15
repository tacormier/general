#!/bin/bash
# Written by jesse bishop - borrowed/modified by Tina Cormier :)

outdir=/mnt/a/tcormier/MREDD/for_Ale/tiles/municipalities/
tiledir=/mnt/p/mredd/tiles/
#loop over them and set each one as $raster
for raster in $(ls $tiledir/lhv*[0-9].tif)
do


	tileID=$(basename "${raster/lhv_/}")
	tilename=mex_municipalities_${tileID}

	tr=$(gdalinfo $raster | grep 'Pixel Size' | awk -F ' = ' '{print $2}' | tr -d '()-' | sed 's/,/ /g')
	ll=$(gdalinfo $raster | grep 'Lower Left' | grep  -o "\( \?-\?[0-9]\{1,3\}\.[0-9]*,[ ]*-\?[0-9]\{1,2\}\.[0-9]*\)" | tr -d '() ' | sed 's/,/ /g')
	ur=$(gdalinfo $raster | grep 'Upper Right' | grep  -o "\( \?-\?[0-9]\{1,3\}\.[0-9]*,[ ]*-\?[0-9]\{1,2\}\.[0-9]*\)" | tr -d '() ' | sed 's/,/ /g')

	# Warp the data to the extent and resolution of the segment raster
	# Slope & elev
	gdalwarp -te $ll $ur -tr $tr -co "COMPRESS=LZW" -of GTiff /mnt/a/tcormier/MREDD/for_Ale/tiles/municipalities/mgm2013v6_2_WGS84.tif $outdir/${tilename}

done