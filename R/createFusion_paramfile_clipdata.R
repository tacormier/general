# SampleFile - Name for subsample file (extension will be added) or a text file containing 
# sample information for 1 or more samples. Each line in the text file should have the subsample 
# filename and the MinX MinY MaxX MaxY values for the sample area separated by spaces or commas. 
# The output filename cannot contain spaces.
# MinX MinY - Lower left corner of the sample area bounding box.
# MaxX MaxY - Upper right corner of the sample area bounding box.

# sample file name - windows format
sf <- "/mnt/a/tcormier/SE_Asia/Ellis_Paper/model_training/lidar_in_plots/las/clipPlots_grndNormalized_params.txt"

# outdir where normalized las files will be stored - windows format:
outdir <- "A:\\tcormier\\SE_Asia\\Ellis_Paper\\model_training\\lidar_in_plots\\las\\grnd_normalized\\"

# txt file of input las files - linux format
lastxt <- "/mnt/a/tcormier/SE_Asia/Ellis_Paper/model_training/lidar_in_plots/las/allfiles_linuxPaths.txt"
####################################
# loop over files, get bounding coordinates, set output names, and write to sf (specified above)
lasfiles <- as.vector(read.table(lastxt, sep=" ")[,1])
#outdf <- as.data.frame(matrix(data=NA, ncol = 5, nrow=0))
outvec <- vector()

for (i in 1:length(lasfiles)) {
  outfile <- paste0(outdir, unlist(strsplit(basename(lasfiles[i]), "\\."))[1], "_gn")
  lasinfocmd <- paste0("/mnt/s/LAStools/bin/lasinfo ", lasfiles[i], " 2>&1")
  lasinfo <- try(system(lasinfocmd, intern=T))
  
  # This assumes header info is ALWAYS uniform - check with new data!
  minX <- unlist(strsplit(lasinfo[20], " "))[24]
  minY <- unlist(strsplit(lasinfo[20], " "))[25]
  maxX <- unlist(strsplit(lasinfo[21], " "))[24]
  maxY <- unlist(strsplit(lasinfo[21], " "))[25]
  
  #outvec[i] <- paste(outfile, paste0("[", paste(minX, minY, maxX, maxY,sep=" "), "]"), sep=",")
  outvec[i] <- paste(outfile, minX, minY, maxX, maxY)
  
}

write.table(outvec,file=sf, quote = F, row.names = F, col.names=F)
