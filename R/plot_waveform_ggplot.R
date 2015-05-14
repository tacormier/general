library(ggplot2)
library(mgcv)
library(gridExtra)

source("/mnt/a/tcormier/scripts/general/R/handy_functions_TC.R")

indir <- "/mnt/a/tcormier/SE_Asia/Ellis_Paper/model_training/lidar_in_plots/txt/"
binsize <- 0.5 #vertical resolution of profile (m)
plotfile <- "/mnt/a/tcormier/SE_Asia/Ellis_Paper/field_plots/BplotsForLidarCalibBaccini_removeColumns.csv"
figdir <- "/mnt/a/tcormier/SE_Asia/Ellis_Paper/figures/"

#############################################
plotlas.files <- list.files(indir, ("*.txt$"), full.names=T)
plotlas <- lapply(plotlas.files, read.table, sep=",",col.names = c("x","y","z","i","a","n","r","c"))
plots <- read.csv(plotfile)

# Calc waveforms here and plop them into a list, so we don't have to keep re-running it to try different plots.
profile <- lapply(plotlas, makeprof, res=binsize)

# Make ground peak 0m
profdata <- array(data=NA, dim=c(length(seq(0,100,binsize)), length(plotlas.files)))
plotids <- array(NA, length(plotlas.files))
gmax0 <- lapply(plotlas, function(lasdata) max(lasdata$z[lasdata$c==2], na.rm=T) - min(lasdata$z))

for (g in 1:length(gmax0)){
  if (gmax0[[g]] < profile[[g]]$height[1]) {
    peakid <- 1
  } else {
    peakid <- which(profile[[g]]$counts == max(profile[[g]]$counts[profile[[g]]$height <= gmax0[[g]]])) #find ground peak
    if (length(peakid) > 1) peakid <- peakid[1]
  }
  
  profile[[g]] <- profile[[g]][peakid:nrow(profile[[g]]),] #trim profile
  profile[[g]]$height <- profile[[g]]$height - min(profile[[g]]$height) #correct heights
  
  # This puts all profiles into one dataframe. Could be super useful later. Right now, I have them all in a list,
  # and the plotting code is expecting a list.  
  #profdata[1:nrow(profile[[g]]),g] <- profile[[g]]$counts
  
  dummy <- gsub(".txt", "", plotlas.files[g])
  dummy <- strsplit(dummy, split="m_")
  plotids[g] <- dummy[[1]][2]
}

# If no waveform=specific text to be added to plots:
#plotlist <- lapply(profile, plotWaveform, nameHeightCol="height", nameCountCol="counts")

# Otherwise, loop through.
maxhts <- data.frame(site=NA, field_maxht=NA, lidar_maxht=NA)
#field_AGC=NA, lidar_AGC=NA)

plotlist <- list()
for (i in 1:length(profile)) {
  #hack
  prof <- profile[[i]]
  site <- paste(unlist(strsplit(unlist(strsplit(plotlas.files[i], "_"))[9:10], "\\."))[1:2], collapse="_")
  #used sprintf to keep trailing zeros
  biomass <- sprintf("%.2f", round(plots$AGLB__Mg_C[plots$PlotID == site], digits=2))
  tree.ht <- sprintf("%.2f", round(plots$Tree_Hei_2[plots$PlotID == site], digits=2))
  
  maxhts[i,] <- c(site, tree.ht, max(prof$height))
  #biomass, NA)
  
  # Add text to plot (still working on making this prettier - a little bit of a hack w/ adding spaces to 
  # make things sort of line up better.)
  annotation <- paste0("Field Data:              \nTallest Tree = ", tree.ht, " m \nAGB = ", biomass, " MgC/ha")
  
  plotlist[[i]] <- plotWaveform(prof, nameHeightCol="height",nameCountCol="counts", title=site, leg.txt = annotation)
  plotlist[[i]]  
} #end profile loop

# PRINT 2x2 per page of a pdf!
args.list <- c(plotlist, 2,2, "")
names(args.list) <- c(1:length(plotlist), "nrow", "ncol", "top")
ggsave(paste0(figdir, "waveform_plots_SEAsia.pdf"), do.call(marrangeGrob, args.list), height=8, width=8)


