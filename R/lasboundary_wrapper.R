# This script is just a wrapper for calling lasboundary. 
# Could have written it in bash or python or whatever.

QLOG <- "/mnt/a/tcormier/scripts/logs/lasboundary/"

# output directory where boundary shapefile will be saved.
outdir <- "/mnt/r/Mex_Lidar/ground_coverage_lastools_noholes/"

# Input directory containing regional subdirectories
# master.dir <- "/mnt/r/Mex_Lidar/Cartodata/"
# master.dir <- "/mnt/r/Mex_Lidar/G_LiHT/"

# List of regional subdirectories:
# regions <- c("Campeche_Yucatan", "Chiapas", "Filipe_Carillo_Puerto",  "Oaxaca1", "Chihuahua",
             # "Oaxaca2", "Oaxaca3")
 
# regions <- c("AMIGACarb_AM_Guan_Chihuahua_GLAS_May2013", "AMIGACarb_Oax_local_1_NFI_Apr2013",
#              "AMIGACarb_Chiap_Campeche_NFI_Apr2013", "AMIGACarb_Out_of_the_Yuc_GLAS_May2013",
#              "AMIGACarb_Chiaps_1_GLAS_Apr2013", "AMIGACarb_Out_of_the_Yuc_NFI_May2013",
#              "AMIGACarb_Chiaps_1_NFI_Apr2013",  "AMIGACarb_PM1_Herm_Guan_GLAS_Apr2013",
#              "AMIGACarb_Chihuahua_norte_GLAS_May2013", "AMIGACarb_PM_Guan_Chihuahua_GLAS_May2013",
#              "AMIGACarb_Chihuahua_norte_NFI_May2013",   "AMIGACarb_PozaRica_Hildalgo_NFI_May2013",
#              "AMIGACarb_Chihua_Sud_GLAS_May2013", "AMIGACarb_Yuc_Centro_GLAS_Apr2013",
#              "AMIGACarb_Chihua_Sud_NFI_May2013",  "AMIGACarb_Yuc_Centro_NFI_Apr2013",
#              "AMIGACarb_Cuern_Oax_PM_NFI_Apr2013", "AMIGACarb_Yuc_Norte_GLAS_Apr2013",
#              "AMIGACarb_G5_Oax_GLAS_Apr2013", "AMIGACarb_Yuc_Norte_NFI_Apr2013",
#              "AMIGACarb_Guan_Oax_1_NFI_Apr2013", "AMIGACarb_Yuc_North_2_GLAS_Apr2013",
#              "AMIGACarb_Guan_Potosi_1_GLAS_Apr2013", "AMIGACarb_Yuc_North_2_NFI_Apr2013",
#              "AMIGACarb_Guan_Potosi_1_NFI_Apr2013", "AMIGACarb_Yuc_South_GLAS_Apr2013",
#              "AMIGACarb_Herm_AM_GLAS_Apr2013", "AMIGACarb_Yuc_South_NFI_Apr2013",
#              "AMIGACarb_Herm_AM_NFI_Apr2013", "Hildalgo_May2013",
#              "AMIGACarb_Herm_Guan_GLAS_Apr2013", "Hildalgo_May2013_bak",
#              "AMIGACarb_Herm_Guan_NFI_Apr2013", "Kuiic1_Apr2013"
#              "AMIGACarb_HermMx_PM_GLAS_Jun2013", "Kuiic2_Apr2013",
#              "AMIGACarb_Oax_Chiap_1_GLAS_Apr2013", "Kuiic3_Apr2013",
#              "AMIGACarb_Oax_Chiap_1_NFI_Apr2013", "AMIGACarb_Oax_local_1_GLAS_Apr2013")

# Subdir structure - what happens after the region name? Assuming it's uniform in every 
# directory. In Cartodata, for example, there is a LAS folder, which contains other subdirs
# for transects and quadrants. So just go down to the deepest common directory, and we'll use
# a recursive find to get all the las files.
# subdir.st <- "/LAS/"

# Because we are doing a recursive search, we don't want all the 30m tiles etc. So
# give some guidance by providing the depth of the recursive search - how many directories
# deep?
# depth <- 2

# Table of dirs with lasfiles and their UTM zones (specific to mex project)
lasutm.tbl <- "/mnt/a/tcormier/Mexico_CMS/lidar/las_all/LAS_directories_UTMzones.csv"
########################################
listdir <- paste0(outdir, "fileLists/")
rdata.dir <- paste0(outdir, "/rdata_files/")
dir.create(outdir)
dir.create(listdir)
dir.create(rdata.dir)

lasutm <- read.csv(lasutm.tbl, stringsAsFactors = F)
# One-time thing here
# subsec <- strsplit(lasutm$path[lasutm$source == "Cartodata"], "/")
# lasutm$subsection <- NA
# lasutm$subsection[lasutm$source == "Cartodata"] <- unlist(lapply(subsec, function(l) l[length(l)]))
# write.csv(lasutm, lasutm.tbl, row.names=F, quote=F)

# quick hack - only for Mexico data and just so happens to work for
# Cartodata and G-LiHT
# lasutm$subdir <- NA
# for (i in (1:nrow(lasutm))) {
#   x <- unlist(strsplit(lasutm$path[i], "/"))
#   lasutm$subdir[i] <- x[length(x)-2]
# }
# write.csv(lasutm, lasutm.tbl, row.names=F, quote=F)


for (lasdir in lasutm$path) {
  tbl <- lasutm[lasutm$path == lasdir,]
  srch.dir <- lasdir
  # srch.dir <- paste0(master.dir, reg, subdir.st)
  # This is epically slow.
  # las.files <- list.files(srch.dir, "*.las", recursive = T, full.names=T)
  # las.cmd <- paste0("find ", srch.dir, " -maxdepth ", depth, " -type f -name '*.las'")
  # lasfiles <- system(las.cmd, intern=T)
  las.files <- list.files(srch.dir, "*.las$", full.names=T)
  
  # write lasfiles to txt file
  if(!is.na(tbl$subsection)) {
    txt <- paste0(listdir, tbl$source, "_", tbl$subdir, "_", tbl$subsection, "_UTM", tbl$Zone_Num, "N_fileList.txt")
    outshp <- paste0(outdir, tbl$source, "_", tbl$subdir, "_", tbl$subsection, "_UTM", tbl$Zone_Num, "N_groundCoverage.shp")
    out.rdata <- paste0(rdata.dir, tbl$source, "_", tbl$subdir, "_", tbl$subsection, "_UTM", tbl$Zone_Num, "N_groundCoverage.rdata")
  } else {
    txt <- paste0(listdir, tbl$source, "_", tbl$subdir, "_UTM", tbl$Zone_Num, "N_fileList.txt")
    outshp <- paste0(outdir, tbl$source, "_", tbl$subdir, "_UTM", tbl$Zone_Num, "N_groundCoverage.shp")
    out.rdata <- paste0(rdata.dir, tbl$source, "_", tbl$subdir, "_UTM", tbl$Zone_Num, "N_groundCoverage.rdata")
  }
  
  fileConn<-file(txt)
  writeLines(las.files, fileConn)
  close(fileConn)
  

  # Test file that runs quickly for debugging only
#   txt <- paste0(unlist(strsplit(txt, "\\."))[1], "_TEST.txt")
#   lasfiles2 <- lasfiles[c(1:10, 331:337)]
#   fileConn<-file(txt)
#   writeLines(lasfiles2, fileConn)
#   close(fileConn)
  
  # Write rdata file to pass to worker script
  save(txt, tbl, outshp, file=out.rdata)
  
  # Grid engine call
  jobname <- basename(unlist(strsplit(outshp, "\\."))[1])
  sys.call <- paste("/net/share-2/export/HomeDir/sge6.2/bin/lx24-amd64/qsub -b yes -l rdisk=4G -q nondev.q -V -N", jobname, "-o",QLOG, "-e", QLOG, 
                    "/mnt/s/bin/jqsub 'R --vanilla --slave < /mnt/a/tcormier/scripts/general/R/lasboundary_worker.R' --args", out.rdata)
  system(sys.call)
  
}# end reg loop



# A litle check for after it's all done to see if they all produced a ground 
# shapefile
# i=1
# for (lasdir in lasutm$path) {
#   tbl <- lasutm[lasutm$path == lasdir,]
#   srch.dir <- lasdir
#   
#   # write lasfiles to txt file
#   if(!is.na(tbl$subsection)) {
#     outshp <- paste0(outdir, tbl$source, "_", tbl$region, "_", tbl$subsection, "_UTM", tbl$Zone_Num, "_groundCoverage.shp")
#   } else {
#     outshp <- paste0(outdir, tbl$source, "_", tbl$region, "_UTM", tbl$Zone_Num, "_groundCoverage.shp")
#   }
#   print(i)
#   print(outshp)
#   print(file.exists(outshp))
#   print(" ")
#   print(" ")
#   i=i+1
# }
# 
 # length(list.files(outdir, "*.shp$")) ==  length(lasutm$path)


