# (1) Make sure all complete plots were extracted by FUSION_extractIndivPlots_prepWrapper.R
# (2) Address duplicate plots (those plots covered by >1 lidar acquisition)
# Need tmpdir files, so hopefully you didn't delete them :)

# tmpdir
tmpdir <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170215/tmp_indivPlots/"

# directory with las files
lasdir <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170215/"

# find wine files - those files submitted to the FUSION command
w.files <- list.files(tmpdir, "*_wine.txt$")
