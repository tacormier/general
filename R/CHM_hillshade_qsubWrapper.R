dirfile <- "/mnt/a/tcormier/Mexico_CMS/lidar/las_all/LAS_directories_UTMzones.csv"
QLOG <- "/mnt/a/tcormier/scripts/logs/G-LiHT_Hillshade/"
dir.create(QLOG)

dirs.all <- read.csv(dirfile, stringsAsFactors = F)
dirs.g <- dirs.all[dirs.all$source == 'G-LiHT',]

for (i in dirs.g$path) {
  d <- paste0(dirname(i), "/geotiff/")
  # print(d)
  chm.files <- list.files(d, "*_CHM.tif$", full.names=T)
  
  for (chm in chm.files) {
    # print(chm)
    outfile <- paste0(unlist(strsplit(chm, "\\."))[1], "_HS.tif")
    rdata <- paste0(unlist(strsplit(chm, "\\."))[1], "_HS.Rdata")
    inras <- chm
    save(inras, outfile, file=rdata)
    jobname <- paste0("Hillshade_", unlist(strsplit(basename(chm), "\\."))[1])
    
    sys.call <- paste("/net/share-2/export/HomeDir/sge6.2/bin/lx24-amd64/qsub -b yes -q nondev.q -V -N", jobname, "-l rdisk=2G", "-o",QLOG, "-e", QLOG, 
                      "/mnt/s/bin/jqsub 'R --vanilla --slave < /mnt/a/tcormier/scripts/general/R/raster_hillshade.R' --args", rdata)
    system(sys.call)
    
  }
}


