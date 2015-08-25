tifdir <- "/mnt/t/testing/mexico/biomass/alos/biomass_modeling/workshop_data/alos_links/"
dbdir <- "/home/tcormier/Dropbox (WHRC)/bis-whrc/"
segdir <- "/mnt/t/testing/mexico/biomass/alos/biomass_modeling/workshop_data/segments/"

# running <- list.files(dbdir, "*.doing$", full.names=T)
# allfiles <- list.files(dbdir, "*hhhvzratio.tif$", full.names=T)

# notrunning <- allfiles %in% sub(pattern = ".doing", "", running)
# remfiles <- allfiles[notrunning == F]
# 
# file.remove(remfiles)

# Which files have not been segmented yet
allfiles <- list.files(tifdir, "*hhhvzratio.tif$", full.names=T)
segfiles <- list.files(segdir, glob2rx("*hhhvzratio.tif_7_01_09.tif"), full.names=T)

# To remove
torem <- segfiles[sub("_7_01_09.tif", "", basename(segfiles)) %in% (basename(allfiles)) == F]

# the difference (the ones we still need to segment)
todo <- allfiles[(basename(allfiles) %in% sub("_7_01_09.tif", "", basename(segfiles))) == F]
# Make a list of all images to be segmented from the cluster
#allfiles.cluster <- list.files(tifdir, "*hhhvzratio.tif$", full.names=T)
# Copy the first 10 over to dropbox
file.copy(todo, dbdir)

# keep running list of files remaining to be copied
#files.remaining <- allfiles.cluster[-(1:10)]
files.remaining <- todo
while (length(files.remaining) > 0) {
  
  # Check for "done" files and move them
  donefiles <- list.files(dbdir, "*.done$", full.names=T)
  
  if (length(donefiles) == 0) {
    print(paste0("there are no done files right now: ", Sys.time()))
    Sys.sleep(60)
  } else {
    
    print(paste0("New loop at ", Sys.time()))
    print(paste0("there are ", length(files.remaining), " files remaining to segment."))
    print("the following files are still processing:")
    print(paste0(list.files(dbdir, "*.doing$", full.names=T)))
    
    print("the following files are done: ")
    print(paste0(donefiles))
    Sys.sleep(60)
#     print("the following files are still processing:")
#     print(paste0(list.files(dbdir, "*.doing$", full.names=T)))
  
    for (f in donefiles) {
        pattern <- glob2rx(paste0(unlist(strsplit(basename(f), "\\."))[1], ".*"))
        copy <- list.files(dbdir, pattern=pattern, full.names=T)
        waitTF <- any(copy == grep("*.doing", value=T,  copy))
        
        # If the .doing file is still there, skip it until it's gone.
        if (waitTF == T) {
          print(paste0("skipping ", f, " - doing file still exists."))
          next
        }
        
        # I tried file.rename, but got errors bc copying across platforms??
        print(paste0("copying. . . ", f))
        file.copy(copy, segdir)
        file.remove(copy)
        # now move that file off of the files remaining list
        files.remaining <- files.remaining[(basename(files.remaining) %in% (paste0(unlist(strsplit(basename(f), "\\."))[1], ".tif")) == F)]
        
#         if (length(list.files(dbdir, pattern="*hhhvzratio.tif$")) < 10) {
#           print(paste0("moving ", files.remaining[1], " to dropbox. . ."))
#           file.copy(files.remaining[1], dbdir)
#           files.remaining <- files.remaining[-1]
#         
#         } else {
#           next
#         }
  
    }# end donefiles loop
  }# end donefiles if
  
  #Check for dead files and remove them except for tif and cmd file (so they will start again)
  deadfiles <- list.files(dbdir, "*.dead$", full.names=T)
  for (dead in deadfiles) {
    pattern <- glob2rx(paste0(unlist(strsplit(basename(dead), "\\."))[1], ".*"))
    deadgrp <- list.files(dbdir, pattern=pattern, full.names=T)
    deadgrp <- deadgrp[grep("*hhhvzratio.tif$", invert=T, deadgrp)]
    deadgrp <- deadgrp[grep("*hhhvzratio.tif.cmd$", invert=T, deadgrp)]
    print("removing the following dead files:")
    print(paste0(deadgrp))
    file.remove(deadgrp)
  } #end deadfiles loop
  
 
  
} # end while

