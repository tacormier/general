#!/bin/python
import os
import csv
from ftplib import FTP


url="ftp.eorc.jaxa.jp"
ftp_dir="/pub/ALOS/ext1/PALSAR_MSC/25m_MSC/2007/"
csvfile = "/mnt/a/tcormier/general/ALOS_global/lists/global_FNF_5deg.csv"
 
ftp = FTP(url)
ftp.login()
dir_source = ftp.nlst(ftp_dir)
files = [f for f in dir_source if 'FNF.tar.gz' in f]
fullfiles = ['ftp://' + url + s for s in files]
ftp.close()

#Assuming res is a flat list
with open(csvfile, "w") as output:
    writer = csv.writer(output, lineterminator='\n')
    for val in fullfiles:
        writer.writerow([val])    