#!/bin/bash

tile=$1
year=$2
outroot=$3

lon=$(echo $tile | awk -F 'X' '{print $2}')
lat=$(echo $tile | awk -F 'X' '{print $3}')
year2=${year:2:2}

if [ $lon -lt 0 ]; then lodir='W'; lon=$(echo $lon | tr -d '-'); else lodir='E'; fi
if [ $lat -lt 0 ]; then ladir='S'; lat=$(echo $lat | tr -d '-'); else ladir='N'; fi

cd $outroot/${year}
wget -c "ftp://ftp.eorc.jaxa.jp/pub/ALOS/ext1/PALSAR_MSC/100m_MSC/${year}/${ladir}$(printf "%02d" $lat)${lodir}$(printf "%03d" $lon)_${year2}_FNF100.tar.gz"

#cd $outroot/mos/${year}
#wget -c "ftp://ftp.eorc.jaxa.jp/pub/ALOS-2/PALSAR_MSC/50m_MSC/${year}/${ladir}$(printf "%02d" $lat)${lodir}$(printf "%03d" $lon)_${year2}_MOS.tar.gz"


# Use this (i.e., copy and paste) at the command line to run this script:
#for x in $(seq -180 20 160); do for y in $(seq 90 -20 -70); do /mnt/s/bin/whrc_generic_qsub_wrapper.sh 1M /mnt/a/tcormier/scripts/general/bash/get_kc50.sh tileX${x}X${y} 2007 /mnt/z/r/JAXA/FNF/100m; done; done