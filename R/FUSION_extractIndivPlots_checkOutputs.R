# (1) Make sure all complete plots were extracted by FUSION_extractIndivPlots_prepWrapper.R
# (2) Address duplicate plots (those plots covered by >1 lidar acquisition)
# Need tmpdir files, so hopefully you didn't delete them :)

# tmpdir
tmpdir <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170215/tmp_indivPlots/"

# directory with las files
lasdir <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170215/"

# find wine and las files - those files submitted to the FUSION command
w.files <- list.files(tmpdir, "*_wine.txt$", full.names=T)
l.files <- list.files(lasdir, "*.las$", full.names=T)

# First, see which files are missing by comparing the names
w.base <- unlist(lapply(w.files, function(x) gsub("_lasIntersect_wine", "", stripExtBase(x))))
l.base <- unlist(lapply(l.files, function(x) stripExtBase(x)))

missing <- w.base[!which()]
missing <- w.base[!(w.base %in% l.base)]

