# (1) Make sure all complete plots were extracted, normalized, and filtered (a - c) were extracted by FUSION_extractIndivPlots_prepWrapper.R
# (2) Address duplicate plots (those plots covered by >1 lidar acquisition)
# Need tmpdir files, so hopefully you didn't delete them :)

# (a) Plot extraction step. 
library(raster)
library(rgeos)
plots.shp <- "/mnt/a/tcormier/Mexico_CMS/field/points_wgs84_updatedFields_points/CMS_FieldPoints_wgs84_updatedFields_20170615_wZeroBio_ecoRegions.shp"
complete.plots <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170615_zeroBiomassAdded/complete_plots/"
# This was made by hand, by me using qgis to check each point in the 15 shapefiles in the incomplete plot dir.
incomp.plotList <- "/mnt/a/tcormier/Mexico_CMS/field/IncAndMissingPlots/Incomplete_Plot_list.csv"
extract.dir <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170615_zeroBiomassAdded/"

p.shp <- shapefile(plots.shp)
pts <- p.shp@data
inc <- read.csv(incomp.plotList, stringsAsFactors = F)

all.id <- pts$ID_TC
# lasfiles in extract.dir
exlas.files <- list.files(extract.dir, "*.las$", full.names=T)
exlas.base <- unlist(lapply(exlas.files, function(x) stripExtBase(x)))
exlas.ids <- sapply(strsplit(exlas.base, "_"), function(x) x[[2]])

# Which IDs are in original points, but were not extracted
missing.id <- all.id[!(all.id %in% exlas.ids)]

# Now remove the ones that were incomplete plots
inc <- inc[inc$Covered_by_Another_Acquisition == 'N',]
missing.complete <- missing.id[!(missing.id %in% inc$ID_TC)]

pts.mc <- pts[pts$ID_TC %in% missing.complete,]
p.shp <- p.shp[p.shp@data$ID_TC %in% pts.mc$ID_TC,]

# Write out and bring over to mac so I can see it in qgis
# Went through every one and ID'd missing plots that SHOULD be extracted - 
# There were some in that shapefile that had partial covereage (not sure why
# they weren't in the "incomplete" plots, but I added them to the incomplete plot list table. New
# total of incompletely covered plots is: 33.)
# There were also plots in there that just did not intersect with the lidar (18)
shapefile(p.shp, "/mnt/a/tcormier/Mexico_CMS/field/IncAndMissingPlots/missing_IDs.shp", overwrite=T)

# list of IDs manually gathered from QGIS that SHOULD have been processed/extracted, but had UTM
# mismatches with data they should overlay (due to lidar acquisitions crossing mult UTM zones, but only
# assigning one zone to the whole acquisition)
id.should <- c(181,217,238,275,295,332,342,347,350,357,359,365,378,389,392,401,405,429,432,433,438,441,445,446,453,477,497,506,527,531,533,543,554,563,595,606)
pts.fix <- p.shp[p.shp@data$ID_TC %in% id.should,]
# Add a field holding the new utm zone for each plot
pts.fix@data$to_UTM <- "NA"
pts.fix@data$lasindex <- "NA"

# Bring in UTM boundaries - just for viewing
utm <- shapefile("/mnt/a/tcormier/general/boundaries/UTM_Zone_Boundaries/UTM_Zone_Boundaries.shp")

# This issue occurs in 6 acquisitions: G-LiHT_AMIGACarb_Chihuahua_norte_NFI_May2013_UTM12N_groundCoverage_WGS84.shp, G-LiHT_AMIGACarb_Herm_Guan_NFI_Apr2013_UTM12N_groundCoverage_WGS84.shp
# G-LiHT_AMIGACarb_Oax_Chiap_1_NFI_Apr2013_UTM14N_groundCoverage_WGS84.shp, G-LiHT_AMIGACarb_Yuc_South_GLAS_Apr2013_UTM15N_groundCoverage_WGS84.shp
# G-LiHT_AMIGACarb_Yuc_South_NFI_Apr2013_UTM15N_groundCoverage_WGS84.shp, and G-LiHT_AMIGACarb_Herm_AM_NFI_Apr2013_UTM12N_groundIndex.shp

# Run these manually in batches to fill in pts.fix, then run the loop. Ugh. Mess. 

# chi.file <- "/mnt/r/Mex_Lidar/ground_coverage_lastools_noholes/wgs84/G-LiHT_AMIGACarb_Chihuahua_norte_NFI_May2013_UTM12N_groundCoverage_WGS84.shp"
# lasindex <- "/mnt/r/Mex_Lidar/ground_index_noholes/G-LiHT_AMIGACarb_Chihuahua_norte_NFI_May2013_UTM12N_groundIndex.shp"

# chi.file <- "/mnt/r/Mex_Lidar/ground_coverage_lastools_noholes/wgs84/G-LiHT_AMIGACarb_Herm_Guan_NFI_Apr2013_UTM12N_groundCoverage_WGS84.shp"
# lasindex <- "/mnt/r/Mex_Lidar/ground_index_noholes/G-LiHT_AMIGACarb_Herm_Guan_NFI_Apr2013_UTM12N_groundIndex.shp"
# # 
# chi.file <- "/mnt/r/Mex_Lidar/ground_coverage_lastools_noholes/wgs84/G-LiHT_AMIGACarb_Oax_Chiap_1_NFI_Apr2013_UTM14N_groundCoverage_WGS84.shp"
# lasindex <- "/mnt/r/Mex_Lidar/ground_index_noholes/G-LiHT_AMIGACarb_Oax_Chiap_1_NFI_Apr2013_UTM14N_groundIndex.shp"
# 
# chi.file <- "/mnt/r/Mex_Lidar/ground_coverage_lastools_noholes/wgs84/G-LiHT_AMIGACarb_Yuc_South_GLAS_Apr2013_UTM15N_groundCoverage_WGS84.shp"
# lasindex <- "/mnt/r/Mex_Lidar/ground_index_noholes/G-LiHT_AMIGACarb_Yuc_South_GLAS_Apr2013_UTM15N_groundIndex.shp"
# 
# chi.file <- "/mnt/r/Mex_Lidar/ground_coverage_lastools_noholes/wgs84/G-LiHT_AMIGACarb_Yuc_South_NFI_Apr2013_UTM15N_groundCoverage_WGS84.shp"
# lasindex <- "/mnt/r/Mex_Lidar/ground_index_noholes/G-LiHT_AMIGACarb_Yuc_South_NFI_Apr2013_UTM15N_groundIndex.shp"
# # 
chi.file <- "/mnt/r/Mex_Lidar/ground_coverage_lastools_noholes/wgs84/G-LiHT_AMIGACarb_Herm_AM_NFI_Apr2013_UTM12N_groundCoverage_WGS84.shp"
lasindex <- "/mnt/r/Mex_Lidar/ground_index_noholes/G-LiHT_AMIGACarb_Herm_AM_NFI_Apr2013_UTM12N_groundIndex.shp"

chi <- shapefile(chi.file)
# get utm
u.to <- sub("N_groundCoverage.*", "", sub(".*UTM", "", chi.file))

# Some plotting to be sure
plot(chi)
plot(utm, add=T)
plot(pts.fix, add=T, col="red")

pts.crop <- crop(pts.fix, chi)
pts.crop
pts.fix@data$to_UTM[pts.fix@data$ID_TC %in% pts.crop@data$ID_TC] <- u.to
pts.fix@data$lasindex[pts.fix@data$ID_TC %in% pts.crop@data$ID_TC] <- lasindex

# Set up new param file for this batch of weirdness
params <- as.data.frame(matrix(nrow=length(id.should), ncol=3, data=NA))
names(params) <- c("polyPath", "ID_Field_Name",	"lasindex")

for (i in (1:length(id.should))) {
  i.poly <- pts.fix[i,]
  i.utm <- i.poly@data$UTM_Zone
  i.id <- i.poly@data$ID_TC
  
  # Now grab the polygon file containing this ID from this dir
  poly.dir <- "/mnt/a/tcormier/Mexico_CMS/field/polygons_projected/"
  pat <- glob2rx(paste0("*UTM", i.utm,".shp"))
  poly.orig <- list.files(poly.dir, "*.shp$", full.names=T)[grep(pat, list.files(poly.dir, "*.shp$", full.names=T))]
  po <- shapefile(poly.orig)
  # pull specific id, write to file, and fill in params
  po.i <- po[po@data$ID_TC == i.id,]
  new.proj <- paste0("+proj=utm +zone=",i.poly@data$to_UTM," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  po.reproj <- spTransform(po.i, CRSobj = CRS(new.proj))
  
  outname <- paste0("/mnt/a/tcormier/Mexico_CMS/field/IncAndMissingPlots/UTM_issues/missing_ID_poly_UTM", i.utm, "_to_UTM",i.poly@data$to_UTM,"_", i.id, ".shp")
  shapefile(po.reproj, outname, overwrite=T)
  
  params$polyPath[i] <- outname
  params$ID_Field_Name[i] <- "ID_TC"
  params$lasindex[i] <- i.poly@data$lasindex
  
}
out.params <- paste0("/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/las_extract/FUSION_extractPlots_params/extract_CMS_MISSING_Plots_from_20170615.csv")
write.csv(params, out.params, row.names=F, quote=F)




##############################
library(lidR)
# (b) check normalization step:
# Ok, so I already checked that 36 lasfiles did not normalize - the same number that had the projection
# zone issue described and (I thought) rectified above.
# Let's check some things:
dtm <- raster("/mnt/r/Mex_Lidar/G_LiHT/AMIGACarb_Chihuahua_norte_NFI_May2013/lidar/geotiff/DTM_mosaic.vrt")
las <- readLAS("/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170314/not_norm_unfil/TCID_342_missing_ID_poly_UTM13_to_UTM12_342_G-LiHT_AMIGACarb_Chihuahua_norte_NFI_May2013_UTM12N_groundIndex.las")

plot(extent(dtm), col="red")
plot(extent(las), add=T)
# Figured this one out on my own (needed to rename files). See: CMS_rename_missing_lasPlots.R All files now accounted for

###############################
# (c) Checking after filtering:
# A quick manual check in terminal yields 12 plots that failed QA, meaning we have 690 from here. 
# Now to remove dups.

###############################

# (2) Addressing duplicates
lasdir <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170314/norm_fil/"
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

#########################
# One more overall check because I feel like one plot is missing! YUP ID 101 was missing for no good reason!!
# Original shapefile of plots:
plots.shp <- "/mnt/a/tcormier/Mexico_CMS/field/points_wgs84_updatedFields_points/CMS_FieldPoints_wgs84_updatedFields_20170228_ecoRegions.shp"
incomp.plotList <- "/mnt/a/tcormier/Mexico_CMS/field/Incomplete_Plot_list.csv"
extract.dir <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170314/norm_fil/"
dup.dir <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170314/norm_fil/duplicate_coverage/"



p.shp <- shapefile(plots.shp)
pts <- p.shp@data
inc <- read.csv(incomp.plotList, stringsAsFactors = F)
inc <- inc[inc$Covered_by_Another_Acquisition == 'N',]

all.id <- pts$ID_TC

exlas.files <- list.files(extract.dir, "*.las$", full.names=T)
exlas.base <- unlist(lapply(exlas.files, function(x) stripExtBase(x)))
exlas.ids <- sapply(strsplit(exlas.base, "_"), function(x) x[[2]])

duplas.files <- list.files(dup.dir, "*.las$", full.names=T)
duplas.base <- unlist(lapply(duplas.files, function(x) stripExtBase(x)))
duplas.ids <- sapply(strsplit(duplas.base, "_"), function(x) x[[2]])

comb.ids <- c(exlas.ids, duplas.ids)

# which are missing?
missing <- all.id[!(all.id %in% comb.ids)]

# Which are missing after we take out plots known to not overlap lidar?
missing2 <- missing[!(missing %in% inc$ID_TC)]
#


# Which IDs are in original points, but were not extracted
missing.id <- all.id[!(all.id %in% exlas.ids)]






#########################
# Old code 
# tmpdir
# tmpdir <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170314/tmp_indivPlots/"
# tmpdir <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170314/norm/"
# tmpdir <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170215/norm_fil/"
# 
# # directory with las files
# lasdir <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170314/norm_fil/"
# 
# # where to store list of IDs with errors:
# err.dir <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170314/error_files/"
# 
# 
# # (1)
# # find wine and las files - those files submitted to the FUSION command
# # w.files <- list.files(tmpdir, "*_wine.txt$", full.names=T)
# w.files <- list.files(tmpdir, "*.las$", full.names=T)
# l.files <- list.files(lasdir, "*.las$", full.names=T)
# 
# # First, see which files are missing by comparing the names
# # w.base <- unlist(lapply(w.files, function(x) gsub("_lasIntersect_wine", "", stripExtBase(x))))
# w.base <- unlist(lapply(w.files, function(x) stripExtBase(x)))
# l.base <- unlist(lapply(l.files, function(x) stripExtBase(x)))
# # Tiny hack for a specific check between two datasets
# w.base <- sub("20160727", "20170228", w.base)
# 
# # missing <- w.base[!(w.base %in% l.base)]
# missing <- l.base[!(l.base %in% w.base)]
# # get plot IDs
# ids <- sapply(strsplit(missing, "_"), function(x) x[[2]])
# dir.create(err.dir)
# write.table(ids, file=paste0(err.dir, "missingIDs.txt"), quote=F, row.names=F, col.names = F)
# 
# 
# redo <- vector()
# 
# for (i in 1:length(missing)) {
#   redo[i] <- paste0(lasdir, missing[i], ".las")
#   
# }
# lasfiles <- redo

