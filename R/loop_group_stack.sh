#!/bin/bash

# Tina Cormier
# February 2015
#
# This wrapper script works with /mnt/a/tcormier/scripts/general/R/loop_group_stack.R
# inputs are an image file (for this script, images are in pairs to be stacked together, so only
# include the first image of the pair).
#
# outdir is the directory that contains the resolution subdirectories.
#
# The script is meant to be used within a loop that submits jobs to the cluster - i.e.:

# for i in $(cat $txtfile); do /mnt/s/bin/whrc_generic_qsub_wrapper.sh 4G /mnt/a/tcormier/scripts/general/R/loop_group_stack.sh $i $outdir;done
################################
if [ "${#}" -ne "2" ]; then
    echo
    echo "usage: /mnt/a/tcormier/scripts/general/R/loop_group_stack.sh <infile> <outdir>"
    echo "example: /mnt/a/tcormier/scripts/general/R/loop_group_stack.sh /mnt/a/mfarina/biomass_scale/scale_inputs/res_180m/ /mnt/a/tcormier/Mexico_CMS/scale/"
    echo
    echo "outdir should be the root directory containing the resolution subdirs"
    exit
fi


infile=$1
outdir=$2

cd $indir
R --vanilla --slave < /mnt/a/tcormier/scripts/general/R/loop_group_stack.R --args ${infile} ${outdir}
done


