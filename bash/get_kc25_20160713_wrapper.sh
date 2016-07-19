#!/bin/bash

# Runs get_kc25_20160713.sh
filelist="/mnt/a/tcormier/general/ALOS_global/lists/global_MOS_5deg.csv"
outdir="/mnt/a/tcormier/general/ALOS_global/MOS/2007_25m/"
QLOG="/mnt/a/tcormier/scripts/logs/kc25/MOS/"



# i="ftp:\\ftp.eorc.jaxa.jp/pub/ALOS/ext1/PALSAR_MSC/25m_MSC/2007/N00E000_07_MOS.tar.gz"
for i in $(cat $filelist); 
do 
	jobname=$(basename ${i} .tar.gz) 
	# -net_bw helps me restrict to 2 downloads per node because one uses 50% of the available bandwidth. Using dev.q bc
	# I don't want to download too many at once and get fired! haha.
	/net/share-2/export/HomeDir/sge6.2/bin/lx24-amd64/qsub -b yes -q nondev.q -V -N $jobname -o $QLOG -e $QLOG -l net_bw=0.5 /mnt/a/tcormier/scripts/general/bash/get_kc25_20160713.sh $i $outdir
done
