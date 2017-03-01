# (1) Make sure all complete plots were extracted by FUSION_extractIndivPlots_prepWrapper.R
# (2) Address duplicate plots (those plots covered by >1 lidar acquisition)
# Need tmpdir files, so hopefully you didn't delete them :)

# tmpdir
# tmpdir <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170215/tmp_indivPlots/"
tmpdir <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170215/norm/"

# directory with las files
lasdir <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170215/norm_fil/"

# where to store list of IDs with errors:
err.dir <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170215/norm_fil/error_files/"


# (1)
# find wine and las files - those files submitted to the FUSION command
# w.files <- list.files(tmpdir, "*_wine.txt$", full.names=T)
w.files <- list.files(tmpdir, "*.las", full.names=T)
l.files <- list.files(lasdir, "*.las$", full.names=T)

# First, see which files are missing by comparing the names
# w.base <- unlist(lapply(w.files, function(x) gsub("_lasIntersect_wine", "", stripExtBase(x))))
w.base <- unlist(lapply(w.files, function(x) stripExtBase(x)))
l.base <- unlist(lapply(l.files, function(x) stripExtBase(x)))

missing <- w.base[!(w.base %in% l.base)]
# missing <- l.base[!(l.base %in% w.base)]
# get plot IDs
ids <- sapply(strsplit(missing, "_"), function(x) x[[2]])
write.table(ids, file=paste0(err.dir, "missingIDs.txt"), quote=F, row.names=F, col.names = F)


redo <- vector()

for (i in 1:length(missing)) {
  redo[i] <- paste0("/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170215/norm/", missing[i], ".las")
  
}
lasfiles <- redo


# (2) Addressing duplicates
lasdir <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170215/norm_fil/"
las.fil <- list.files(lasdir, "*.las$", full.names=T)
id.a <- strsplit(basename(las.fil), "_")
id <- sapply(id.a, "[", 2)

# All rows with a non-unique ID = T
dups <- duplicated(id) | duplicated(id, fromLast = TRUE)

# Just the duplicate ID rows from las.fil
dup.id <- id[dups]
las.dup.id <- las.fil[dups]

dup.unique <- unique(dup.id)

dup.keep <- vector()
for (uid in (1:length(dup.unique))) {
  # for testing
  # uid=1
  # get records from las.fil
  las.uid <- las.fil[id == dup.unique[uid]]
  
  # If cartodata appears once or more, just take the first one (this should not happen)
  if ((length(grep('Cartodata', las.uid))) > 0) {
    dup.keep[uid] <- las.uid[grepl('Cartodata', las.uid) == T][1]
    
    # OR, if G-LiHT ONLY, keep first one
  } else if ((length(grep('G-LiHT', las.uid)) > 0) & (length(grep('Cartodata', las.uid)) == 0)) {
    dup.keep[uid] <- las.uid[1]
    
  }
   
}

# Now we have a vector of files to keep - need to know which ones to move to a "duplicate coverage" folder
dup.dir <- paste0(lasdir, "duplicate_coverage/")
dir.create(dup.dir)

las.move <- las.dup.id[!(las.dup.id %in% dup.keep)]
new <- paste0(dup.dir, basename(las.move))
file.rename(las.move, new)

