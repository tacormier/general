#!/bin/bash
#txtheader=/mnt/a/tcormier/Mexico_CMS/lidar/parameter_files/CMS_metrics_header.csv
#filelist=/mnt/a/tcormier/testing/catcsv.txt 
filelist=$1

# directory containing text files to merge
# dir=$1
outfile=$2
# File that already contains the header row.
txtheader=$3

/bin/cp ${txtheader} $(dirname ${outfile})/$(basename ${outfile})

# If $1 is a directory of files. Need to write logic to handle
# figuring out what $1 is, but for now, just comment out.
# cd $dir
# find . -type f -name '*.csv' -exec tail -n +2 {} \; >> $outfile


# Alternative - if $1 is a txt list of files
for i in $(cat ${filelist})
do
	tail -n +2 ${i} >> $outfile
done

# Faster alternative?
# cat $filelist | xargs -n 10000 -P8 tail -n +2 > $outfile
# cat $filelist | xargs -n 10000 -P8 sed -n '2p'  > $outfile
# /mnt/s/python-2.7.6/bin/csvstack -H $(cat $filelist | tr '\n' ' ') > $outfile
# sed -i "1c $(cat $txtheader)" $outfile