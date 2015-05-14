library(maptools)

# plotindir <- "a:\\tcormier\\SE_Asia\\Ellis_Paper\\field_plots\\indiv_plots\\"
lasindir <- "a:\\tcormier\\SE_Asia\\Ellis_Paper\\lidar\\"
outdir <- "a:\\tcormier\\SE_Asia\\Ellis_Paper\\model_training\\lidar_in_plots\\"
#plotsize
ps <- 20
plotpolyfile <- "a:\\tcormier\\SE_Asia\\Ellis_Paper\\field_plots\\BplotsForLidarCalibBaccini_poly_20m.shp"

#Study areas
sa <- c("Amindo", "Meraang", "Sumalindo_bio")
####################################
lastxtfile <- paste0(lasindir, "las_all\\plotlasfiles.txt")
if (file.exists(lastxtfile)) file.remove(lastxtfile)

for (i in sa) {
  i.indir <- paste0(lasindir,i, "\\")
  lasfiles <- list.files(i.indir, "*.las$", full.names=T)
  write.table(lasfiles, file = lastxtfile, eol = "\n", quote = F, row.names = F,col.names = F, append = T)
}



pt <- readShapePoints(plotpolyfile)
# #Get bounding box coordinates (need radius here, thus ps/2)
# bb <- c(pt$X_PROJ-(ps/2), pt$Y_PROJ-(ps/2),pt$X_PROJ+(ps/2), pt$Y_PROJ+(ps/2))
outfile <- paste0(outdir,"las\\",unlist(strsplit(basename(plotpolyfile), "\\."))[1], ".las")
  
#cmd <- paste0("C:\\FUSION\\clipdata /shape:1 ", lastxt, " ", outfile, " ", paste(bb, collapse = " "), " \n")
cmd <- paste0("C:\\FUSION\\polyclipdata /multifile /shape:10,* ", plotpolyfile, " ", outfile, " ", lastxtfile, " \n")
#batch <- append(batch, cmd)

outbatch <- paste0(outdir, "batchfiles\\", unlist(strsplit(basename(plotpolyfile), "\\."))[1], ".bat")
write.table(cmd, file = outbatch, eol = "\n", quote = F, row.names = F,col.names = F)
