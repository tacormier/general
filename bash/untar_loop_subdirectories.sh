#!/bin/bash
# goes one level deep into master dir and untars everything

masterdir=$1

cd $masterdir
for d in $(ls);
do
	cd ${d}
	pwd
	for tgz in $(ls *.tar.gz);
	do
		tar -zxvf ${tgz}
	done
	cd $masterdir
done
