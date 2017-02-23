# (1) Make sure all complete plots were extracted by FUSION_extractIndivPlots_prepWrapper.R
# (2) Address duplicate plots (those plots covered by >1 lidar acquisition)
# Need tmpdir files, so hopefully you didn't delete them :)

# tmpdir
# tmpdir <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170215/tmp_indivPlots/"
tmpdir <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170215/norm/"

# directory with las files
lasdir <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170215/"

# where to store list of IDs with errors:
err.dir <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170215/norm/error_files/"


# find wine and las files - those files submitted to the FUSION command
# w.files <- list.files(tmpdir, "*_wine.txt$", full.names=T)
w.files <- list.files(tmpdir, "*.las", full.names=T)
l.files <- list.files(lasdir, "*.las$", full.names=T)

# First, see which files are missing by comparing the names
# w.base <- unlist(lapply(w.files, function(x) gsub("_lasIntersect_wine", "", stripExtBase(x))))
w.base <- unlist(lapply(w.files, function(x) stripExtBase(x)))
l.base <- unlist(lapply(l.files, function(x) stripExtBase(x)))

# missing <- w.base[!(w.base %in% l.base)]
missing <- l.base[!(l.base %in% w.base)]
# get plot IDs
ids <- sapply(strsplit(missing, "_"), function(x) x[[2]])
write.table(ids, file=paste0(err.dir, "missingIDs.txt"), quote=F, row.names=F, col.names = F)

