# List .tar.gz files in directory and untar

d=${1}

for tgz in $(ls ${d}/*.tar.gz);
do
	tar --skip-old-files -zxvf ${tgz}
done