# After removing duplicates, made new laslist. 
# will call it out.txt for now for ease and consistency - update later.
pt1 <- proc.time()
out.txt <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170314/norm_fil/filtered_laslist.txt"
reform <- 'Y'
out.metrics <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170314/norm_fil/plot_metrics_FUSION.csv"

# Reformat table for windows tool?
if (reform == "Y") {
  lasfil.list <- scan(out.txt, what="character")
  # tbl <- list.files(outlasnorm.dir, "*.las$", full.names=T)
  lasfiles_reform <- paste0(stripExt(out.txt), "_wine.txt")
  
  # perform the reformatting and write out new file
  tbl.ref <- gsub("/mnt", "Z:/mnt", lasfil.list)
  tbl.ref <- gsub("/", "\\\\\\\\", tbl.ref)
  
  write.table(tbl.ref, lasfiles_reform, quote = F, row.names = F, col.names = F)
}# end reform if


lasfiles_reform_txt <- gsub("/mnt", "Z:/mnt", lasfiles_reform)
lasfiles_reform_txt <- gsub("/", "\\\\\\\\", lasfiles_reform_txt)
out.metrics.reform <- gsub("/mnt", "Z:/mnt", out.metrics)
out.metrics.reform <- gsub("/", "\\\\\\\\", out.metrics.reform)

# Submit to fusion to calc metrics
cloudmet.cmd <- paste0("/usr/bin/wine \\\\mnt\\\\a\\\\tcormier\\\\FUSION2\\\\cloudmetrics.exe /minht:1.5 /above:3 /new ", lasfiles_reform_txt, " ", out.metrics.reform)
system(cloudmet.cmd)
pt2 <- proc.time()
print(paste0("metrics took ", round((pt2[3] - pt1[3])/60,2), " minutes"))


