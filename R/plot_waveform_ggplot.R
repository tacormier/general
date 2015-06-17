library(ggplot2)
library(mgcv)
library(gridExtra)
library(raster)

source("/mnt/a/tcormier/scripts/general/R/handy_functions_TC.R")
rasterOptions(tmpdir="/home/tcormier/RasTmpDir/")

# do you want to create the profile or does it already exist ("y" or "n")
createprof <- "y"
# if profile already exists, filepath
prof.file <- ""
lineType <- "smooth"
ylim.waves <- c(0,80)
# If profile must be created:
indir <- "/mnt/a/tcormier/SE_Asia/Ellis_Paper/model_training/lidar_in_plots/txt/"
plotfile <- "/mnt/a/tcormier/SE_Asia/Ellis_Paper/field_plots/BplotsForLidarCalibBaccini_removeColumns.csv"
figdir <- "/mnt/a/tcormier/SE_Asia/Ellis_Paper/figures/"


binsize <- 0.5 #vertical resolution of profile (m)
dtm.file <- "/mnt/a/fgoncalves/Mexico/dtm_mosaic.tif"


#############################################
if (createprof == "y") {
  dtm <- raster(dtm.file)
  plotlas.files <- list.files(indir, ("*.txt$"), full.names=T)
  plotlas <- lapply(plotlas.files, read.table, sep=",",col.names = c("x","y","z","i","a","n","r","c"))
  plots <- read.csv(plotfile)
  
  # Calc waveforms here and plop them into a list, so we don't have to keep re-running it to try different plots.
  makeprofile <- lapply(plotlas, makeprof, res=binsize, dtm=dtm)
  #pull out just the profiles from the function returns
  profile <- lapply(makeprofile, "[[", 1)
  
} else if (createprof == "n") {
  profile <- load(prof.file)

} else {
  stop(paste0("the variable 'createprof' must be 'y' or 'n' (lowercase). You entered ", createprof))
}

# Use site name as title
sitenames <- function(filenames) {
  paste(unlist(strsplit(unlist(strsplit(filenames, "_"))[9:10], "\\."))[1:2], collapse="_")
}
sites <- lapply(plotlas.files, sitenames)
names(profile) <- sites
# If no waveform=specific text to be added to plots, create plots from this one line:
#plotlist <- lapply(profile, plotWaveform, nameHeightCol="height", nameCountCol="counts", ylim=ylim.waves, smoothFactor=0.075, lineType=lineType, plot.title=names(plotlas))
plotwf <- function(x) {
  plotWaveform(waveformDF=profile[[x]], nameHeightCol="height", nameCountCol="counts", ylim=ylim.waves, smoothFactor=0.1, lineType=lineType, plot.title=x)  
}
plotlist <- lapply(names(profile), plotwf)
plotlist[[25]]
#Otherwise, loop through.
# maxhts <- data.frame(site=NA, field_maxht=NA, lidar_maxht=NA)
# #field_AGC=NA, lidar_AGC=NA)
# 
# plotlist <- list()
# for (i in 1:length(profile)) {
#   #hack
#   prof <- profile[[i]]
#   site <- paste(unlist(strsplit(unlist(strsplit(plotlas.files[i], "_"))[9:10], "\\."))[1:2], collapse="_")
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
#   plotlist[[i]] <- plotWaveform(prof, nameHeightCol="height",nameCountCol="counts", plot.title=site, leg.txt = annotation, ylim=ylim.waves, lineType=lineType)
#   plotlist[[i]]  
# } #end profile loop

# PRINT 2x2 per page of a pdf!
args.list <- c(plotlist, 2,2, "")
names(args.list) <- c(1:length(plotlist), "nrow", "ncol", "top")
ggsave(paste0(figdir, "waveform_plots_SEAsia_", lineType, "Lines_10.pdf"), do.call(marrangeGrob, args.list), height=8, width=8)


