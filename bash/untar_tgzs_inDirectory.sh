# List .tar.gz files in directory and untar

d=${1}
cd $d
for tgz in $(ls *.tar.gz);
do
	#tar --skip-old-files -zxvf ${tgz}
	tar -zxvf ${tgz}
done