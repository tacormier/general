#!/bin/bash

tile=$1
outdir=$2

cd $outdir
donefile=$outdir/$(basename ${tile}).done
errorfile=$outdir/$(basename ${tile}).error

if [ ! -e ${donefile} ]; then
	wget -c $tile
fi

# This checks if the exit status of the script was successful 
if [ "$?" == "0" ]; then touch ${donefile}; else touch ${errorfile}; fi

