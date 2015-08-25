library(maptools)

#plotindir <- "a:\\tcormier\\SE_Asia\\Ellis_Paper\\field_plots\\indiv_plots\\"
lasindir <- "a:\\tcormier\\SE_Asia\\Ellis_Paper\\lidar\\"
#lasindir <- "r:\\Mex_Lidar\\Cartodata\\"
outdir <- "a:\\tcormier\\SE_Asia\\Ellis_Paper\\model_training\\lidar_in_plots\\"
#outdir <- "a:\\tcormier\\Mexico_CMS\\lidar\\"

#lastxtfile <- "a:\\tcormier\\Mexico_CMS\\lidar\\las_all\\lasfiles_CampecheYucatan.txt"
lastxtfile <- "a:\\tcormier\\SE_Asia\\Ellis_Paper\\lidar\\las_all\\plotlasfiles.txt"

#plotsize (diameter)
#ps <- 20
#plotpolyfile <- "a:\\tcormier\\SE_Asia\\Ellis_Paper\\field_plots\\BplotsForLidarCalibBaccini_poly_10m_rad.shp"
plotpolyfile <- "a:\\tcormier\\SE_Asia\\Ellis_Paper\\field_plots\\SE_Asia_biomass0_10m_rad.shp"
#plotpolyfile <- "a:\\tcormier\\Mexico_CMS\\field\\INFyS_Cartodata_intersect_UTM16N.shp"
#Study areas
sa <- c("Amindo","Meraang","Sumalindo_bio")
#sa <- c("Campeche_Yucatan",  "Chiapas",  "Chihuahua",  "Jalisco",  "Oaxaca1",  "Oaxaca2")
####################################
#lastxtfile <- paste0("a:\\tcormier\\Mexico_CMS\\lidar\\las_all\\lasfiles.txt")
#wrote this little bash command line code to create lasfiles.txt for the CMS project.
#for i in $(ls $PWD/); do echo $i; for j in $(ls $PWD/${i}/LAS/); do echo $j; $(echo $PWD/${i}/LAS/${j}/*.las >> /mnt/a/tcormier/Mexico_CMS/lidar/las_all/lasfiles.txt); done; done
#if (file.exists(lastxtfile)) file.remove(lastxtfile)

# 
# for (i in sa) {
#   i.indir <- paste0(lasindir,i, "\\LAS\\")
#   #list subdirs
#   i.subdirs <- list.dirs(path = i.indir, full.names = T,recursive = F)
#   #sub out fwd slashes
#   i.subdirs <- sub("/", "", i.subdirs)
#   
#   for (sub in i.subdirs) {
#     lasfiles <- list.files(sub, "*.las$", full.names=T)
#     #sub out fwd slashes again
#     lasfiles <- sub("/", "\\\\", lasfiles)
#     write.table(lasfiles, file = lastxtfile, eol = "\n", quote = F, row.names = F,col.names = F, append = T)
#   }
# }


#pt <- readShapePoints(plotpolyfile)
pt <- readShapePoly(plotpolyfile)
# #Get bounding box coordinates (need radius here, thus ps/2)
# bb <- c(pt$X_PROJ-(ps/2), pt$Y_PROJ-(ps/2),pt$X_PROJ+(ps/2), pt$Y_PROJ+(ps/2))
#outfile <- paste0(outdir,"las\\",unlist(strsplit(basename(plotpolyfile), "\\."))[1], ".las")
outfile <- paste0(outdir,"field_intersect\\",unlist(strsplit(basename(plotpolyfile), "\\."))[1], ".las")
  
#cmd <- paste0("C:\\FUSION\\clipdata /shape:1 ", lastxt, " ", outfile, " ", paste(bb, collapse = " "), " \n")
#cmd <- paste0("C:\\FUSION\\polyclipdata /multifile /shape:10,* ", plotpolyfile, " ", outfile, " ", lastxtfile, " \n")
cmd <- paste0("C:\\FUSION\\polyclipdata /multifile /shape:1,* ", plotpolyfile, " ", outfile, " ", lastxtfile, " \n")
#batch <- append(batch, cmd)

outbatch <- paste0(outdir, "batchfiles\\", unlist(strsplit(basename(plotpolyfile), "\\."))[1], ".bat")
write.table(cmd, file = outbatch, eol = "\n", quote = F, row.names = F,col.names = F)

