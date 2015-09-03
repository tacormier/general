library(ggplot2)
library(mgcv)
library(gridExtra)
library(raster)
library(pbapply)

source("/mnt/a/tcormier/scripts/general/R/handy_functions_TC.R")
rasterOptions(tmpdir="/home/tcormier/RasTmpDir/")

# do you want to create the profile or does it already exist ("y" or "n")
createprof <- "y"
# if profile already exists, filepath
prof.file <- ""
# line type = "smooth" or "exact"
lineType <- "exact"
smoothVal <- 0.15
#ylim.waves <- c(0,80)
#xlim.waves <- c(0,200)
# If profile must be created:
indir <- "/mnt/a/tcormier/testing/lidar_processing/indiv_tile_testing/windows/"
#indir <- "/mnt/r/Mex_Lidar/Cartodata/Campeche_Yucatan/LAS/Q1/Tiles_50m/"
#indir <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/txt/CampecheYucatan/"
#plotfile <- "/mnt/a/tcormier/SE_Asia/Ellis_Paper/field_plots/BplotsForLidarCalibBaccini_poly_10m_rad_addNoBiomassPlots_removeColumns.csv"
plotfile <- NULL
figdir <- "/mnt/a/tcormier/testing/lidar_processing/indiv_tile_testing/waveforms/"
binsize <- 0.5 #vertical resolution of profile (m)

# Values for flagging bad points or tiles - based on field data or expert knowledge
max.height <- 82
min.height <- -3

dtm.file <- "/mnt/a/fgoncalves/Mexico/dtm_mosaic.tif"
#dtm.file <- "/mnt/r/Mex_Lidar/Cartodata/Campeche_Yucatan/Deliverables/Mosaics/Q1/Q1_DTM.tif"
#dtm.file <- "/mnt/r/Mex_Lidar/Cartodata/Oaxaca2/Deliverables/Mosaics/DTM.tif"
#############################################
if (createprof == "y") {
  dtm <- raster(dtm.file)
  plotlas.files <- list.files(indir, ("*.txt$"), full.names=T)
  plotlas <- pblapply(plotlas.files, read.table, sep=",",col.names = c("x","y","z","i","a","n","r","c"))
  plots <- read.csv(plotfile)
  
  # Calc waveforms here and plop them into a list, so we don't have to keep re-running it to try different plots.
  print(paste0("Starting profile calcs at ", Sys.time()))
  makeprofile <- pblapply(plotlas, makeprof, res=binsize, dtm=dtm, max.height=max.height, min.height=min.height)
  print(paste0("Finished profile calcs at ", Sys.time()))
  #pull out just the profiles from the function returns
  profile <- pblapply(makeprofile, "[[", 1)
  
} else if (createprof == "n") {
  profile <- load(prof.file)

} else {
  stop(paste0("the variable 'createprof' must be 'y' or 'n' (lowercase). You entered ", createprof))
}

# Use site name as title
sitenames <- function(filenames) {
  paste(unlist(strsplit(unlist(strsplit(filenames, "_"))[10:11], "\\."))[1:2], collapse="_")
}

if (!is.null(plotfile)) {
  sites <- pblapply(plotlas.files, sitenames)
  names(profile) <- sites
} else {
  sites <- lapply(plotlas.files, basename)
  n <- lapply(plotlas, nrow)
  names(profile) <- sites
  #names(profile) <- paste0(sites, "   n = ", n)
  # names(profile) <- sites
}

# Find xlim and ylim
# saveAll <- profile
# profile <- profile[29:30]

xmax <- roundUp(pbsapply(do.call("rbind",profile),max)[2],to = 5)
ymax <- roundUp(pbsapply(do.call("rbind",profile),max)[1],to = 5)
xmax <- 150
ymax <- 70
xlim.waves <- c(0,xmax)
ylim.waves <- c(0,ymax)

# If no waveform=specific text to be added to plots, create plots from this one line:
#plotlist <- lapply(profile, plotWaveform, nameHeightCol="height", nameCountCol="counts", ylim=ylim.waves, smoothFactor=0.075, lineType=lineType, plot.title=names(plotlas))
plotwf <- function(x) {
  plotWaveform(waveformDF=profile[[x]], nameHeightCol="height", nameCountCol="counts", ylim=ylim.waves, xlim=xlim.waves, smoothFactor=smoothVal, lineType=lineType, plot.title=x)  
}
plotlist <- pblapply(names(profile), plotwf)
#plotlist[[2]]

#Otherwise, loop through.
# maxhts <- data.frame(site=NA, field_maxht=NA, lidar_maxht=NA)
# #field_AGC=NA, lidar_AGC=NA)
# 
# plotlist <- list()
# for (i in 1:length(profile)) {
#   #hack
#   prof <- profile[[i]]
#   site <- paste(unlist(strsplit(unlist(strsplit(plotlas.files[i], "_"))[10:11], "\\."))[1:2], collapse="_")
#   #used sprintf to keep trailing zeros
#   biomass <- sprintf("%.2f", round(plots$AGLB__Mg_C[plots$PlotID == site], digits=2))
#   tree.ht <- sprintf("%.2f", round(plots$Tree_Hei_2[plots$PlotID == site], digits=2))
#   
#   maxhts[i,] <- c(site, tree.ht, max(prof$height))
#   #biomass, NA)
#   
#   # Add text to plot (still working on making this prettier - a little bit of a hack w/ adding spaces to 
#   # make things sort of line up better.)
#   annotation <- paste0("Field Data:              \nTallest Tree = ", tree.ht, " m \nAGB = ", biomass, " MgC/ha")
#   
#   plotlist[[i]] <- plotWaveform(prof, nameHeightCol="height",nameCountCol="counts", plot.title=site, leg.txt = annotation, 
#                                 ylim=ylim.waves, xlim=xlim.waves, lineType=lineType, smoothFactor = smoothVal)
#   plotlist[[i]]  
# } #end profile loop

# # PRINT 2x2 per page of a pdf!
# args.list <- c(plotlist, 2,2)
# names(args.list) <- c(1:length(plotlist), "nrow", "ncol")
# ggsave(paste0(figdir, "waveform_plots_SEAsia_", lineType, "Lines_10.pdf"), do.call("marrangeGrob", args.list), height=8, width=8)
#ggsave(paste0(figdir, "waveform_plots_SEAsia_", lineType, "Lines_10.pdf"), plotlist, height=8, width=8)

pdf(file=paste0(figdir, "waveform_plots_SEAsia_test_windows_diff_n", lineType,"_", sub("\\.", "", smoothVal), ".pdf"), onefile=T, width=8, height=8)
pgsetup <- seq(from=1, to=length(plotlist), by=4)
for (pl in pgsetup) {
  if (pl != pgsetup[length(pgsetup)]) {
  plot.pg <- plotlist[pl:(pl+3)]
  } else {
    plot.pg <- plotlist[pl:(length(plotlist))]
  }
  multiplot(plotlist=plot.pg, layout=matrix(c(1,2,3,4), byrow=T, nrow=2))
}
dev.off()
#ggsave(paste0(figdir, "waveform_plots_SEAsia_", lineType, ".pdf"), multiplot(plotlist=plotlist, cols=2, nrow=2),height=8, width=8) 


# Hard coded for Mexico report
# ggsave(paste0(figdir, "waveform_plots_Mex1_bio_CampecheYucatan_3mgha.pdf"), plotlist[[2]], height=8, width=8)

