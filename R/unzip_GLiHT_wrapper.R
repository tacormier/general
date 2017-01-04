# Wrapper script to unzip las and dtm files. Calls /mnt/s/bin/jqsub /mnt/a/tcormier/scripts/general/bash/unzip.sh
QLOG <- "/mnt/a/tcormier/scripts/logs/unzip_GLiHT/"

main.dir <- "/mnt/r/Mex_Lidar/G_LiHT/"
subdirs <- list.dirs(main.dir, full.names=T, recursive = F)

# We want to unzip just the las files and the DTMs
i=subdirs[1]
# for (i in subdirs) {
  lasfiles <- list.files(paste0(i, "/lidar/las/"), "*.gz$", full.names=T)
  for (l in lasfiles) {
    print(l)
    l.jobname <- unlist(strsplit(basename(l), "\\."))[1]
    l.outdir <- paste0(dirname(l), "/")
    l.cmd <- paste("/net/share-2/export/HomeDir/sge6.2/bin/lx24-amd64/qsub -b yes -l rdisk=2G -q nondev.q -V -N", l.jobname, "-o",QLOG, "-e", QLOG, 
                                      "/mnt/s/bin/jqsub /mnt/a/tcormier/scripts/general/bash/unzip.sh", l, l.outdir)
    system(l.cmd)
  } # end lasfile loop
  
  # Now DTM/CHM files
  p <- glob2rx("*_CHM*.gz$|*_DTM*.gz$")
  terrfiles <- list.files(paste0(i, "/lidar/geotiff/"), p, full.names=T)
  for (tf in terrfiles) {
    print(tf)
    tf.jobname <- unlist(strsplit(basename(tf), "\\."))[1]
    tf.outdir <- paste0(dirname(tf), "/")
    tf.cmd <- paste("/net/share-2/export/HomeDir/sge6.2/bin/lx24-amd64/qsub -b yes -l rdisk=2G -q nondev.q -V -N", tf.jobname, "-o",QLOG, "-e", QLOG, 
                   "/mnt/s/bin/jqsub /mnt/a/tcormier/scripts/general/bash/unzip.sh", tf, tf.outdir)
    system(tf.cmd)
  } # end terrain file loop
}# end subdir loop
