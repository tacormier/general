#!/bin/bash
# Called by /mnt/a/tcormier/scripts/general/R/unzip_GLiHT_wrapper.R OR
# can be run from the command line.


gzfile=$1
outdir=$2


cd $outdir
donefile=$outdir/$(basename ${gzfile}).done
errorfile=$outdir/$(basename ${gzfile}).error

# Sometimes the file might be tarred as well - need to check the file name for
# .tar.gz vs. .gz
if [ ! -e ${donefile} ]; then
	test=$(echo ${gzfile} | grep \.tar\.gz)
	# checks if test is empty - would mean it's NOT a tar file - just gzip
	if [[  -z "$test"  ]]; then	
		gunzip ${gzfile}
	else
		tar -zxvf ${gzfile}
	fi
fi

# This checks if the exit status of the script was successful 
if [ "$?" == "0" ]; then touch ${donefile}; else touch ${errorfile}; fi

if [[ -e ${donefile} && -e ${gzfile} ]]; then 
	rm -f ${gzfile}
fi

