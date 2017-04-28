dirlist <- "/mnt/a/tcormier/Mexico_CMS/lidar/parameter_files/dirlists/G-LiHT_Tiles_30m.txt"
dirlist <- "/mnt/a/tcormier/Mexico_CMS/lidar/parameter_files/dirlists/Cutzamala_Tiles30m.txt"
QLOG <- "/mnt/a/tcormier/scripts/logs/normalize_g-liht/"
##########################
dir.create(QLOG, showWarnings = F)

dirs <- scan(dirlist, what="character")

total.tiles <- vector()

for (d in dirs) {
  # make a temp dir to hold rdata files and file lists
  tmpdir <- paste0(d, "/temp/")
  dir.create(tmpdir)
  # List text files
  txtfiles <- list.files(d, "*.txt$", full.names=T)
  total.tiles <- c(total.tiles, length(txt.files))
  # Cannot submit too many jobs at a time. These should be pretty quick though!
  # Hopefully do not need to split when running on cloud machines.
  # Break list into parts:
  # max to submit at once:
  maxsub <- 1000
  txt.split <- split(txtfiles, ceiling(seq_along(txtfiles)/maxsub))
  # i=80
  # lcomp <- c("nondev.q@lcomp-5.whrc.ad",)
  for (i in 1:length(txt.split)) {
    print(names(txt.split[i]))
    outtxt <- paste0(tmpdir, names(txt.split[i]), ".txt")
    txt.sub <- txt.split[[i]]
    write.table(txt.sub, outtxt, row.names=F, quote=F, col.names=F)
    
    lof <- outtxt
    odir <- d
    
    out.rdata <- paste0(tmpdir, names(txt.split[i]), ".RDATA")
    save(lof, odir, file=out.rdata)
    
    # This is hardcoded for mex lidar dir structure
    jobname <- paste0(unlist(strsplit(d, "/"))[6], "_list", names(txt.split[i]), "_normalize")
    
    sys.call <- paste("/net/share-2/export/HomeDir/sge6.2/bin/lx24-amd64/qsub -b yes -l rdisk=2G -q dev.q -V -N", jobname, "-o",QLOG, "-e", QLOG, 
                      "/mnt/s/bin/jqsub 'R --vanilla --slave < /mnt/a/tcormier/scripts/general/R/txt2las_byFileList.R' --args", out.rdata)
    system(sys.call)
    
  }

}