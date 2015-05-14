library(ggplot2)

indir <- "/mnt/a/tcormier/SE_Asia/Ellis_Paper/model_training/lidar_in_plots/txt/"
binsize <- 0.5 #vertical resolution of profile (m)
plotfile <- "/mnt/a/tcormier/SE_Asia/Ellis_Paper/field_plots/BplotsForLidarCalibBaccini_removeColumns.csv"
###########################################
# From Fabio's calc metrics code:
#function to produce pseudo-waveforms
makeprof <- function(mydata, res) { #mydata = las data, res = vertical resolution (m)
  elev <- mydata$z
  elev0 <- elev - min(elev) #note that 0 here is the lowest elevation, not the ground peak
  int <- mydata$i
  clas <- mydata$c #2 - ground, 3 - low vegetation, 4 - medium vegetation, 5 - high vegetation, etc
  breaks <- seq(min(elev0), max(elev0)+res, by=res)
  z <- breaks[2:length(breaks)]
  gmax <- max(elev0[clas==2])
  #if (gmax < z[1]) warning("ground height < bin size")
  
  #get number of points per height interval
  hist_elev0 <- hist(elev0, breaks=seq(min(elev0), max(elev0)+res, by=res), include.lowest=T, right=F, plot=F) 
  counts <- hist_elev0$counts
  
  #get total intensity per height interval
  int_sum <- rep(0, times=length(z))
  int_n <- rep(0, times=length(z))
  for (i in 1:length(z)) { #inefficient... change it later
    for (j in 1:length(elev0)) if (elev0[j] >= breaks[i] & elev0[j] < breaks[i+1]) { 
      int_sum[i] <- int_sum[i] + int[j]
      int_n[i] <- int_n[i] + 1
    }
  }
  
  #check: int_n should be equal to counts
  if (sum(int_n - counts) != 0) stop("Check the code... we have a problem") 
  
  #return
  data.frame(height=z, counts=counts, intensity=int_sum)
}
#############################################

plotlas <- list.files(indir, ("*.txt$"), full.names=T)
plots <- read.csv(plotfile)

#load fabio's profiles for comparison
load("/mnt/a/tcormier/SE_Asia/Ellis_Paper/model_training/lidar_in_plots//txt//profiles_fg.RData")
head(profdata)

pdf(file = paste0(indir, "waveform_plots_profilesTC.pdf"), height=8, width = 8)
par(mfrow=c(2,2))
for (pl in plotlas) {
  las <- read.table(pl, sep=",",col.names = c("x","y","z","i","a","n","r","c"))
  #hack
  site <- paste(unlist(strsplit(unlist(strsplit(pl, "_"))[9:10], "\\."))[1:2], collapse="_")
  biomass <- round(plots$AGLB__Mg_C[plots$PlotID == site], digits=2)
  tree.ht <- round(plots$Tree_Hei_2[plots$PlotID == site],digits=2)
  
  waveform <- makeprof(las, binsize)
  wf.sort <- waveform[order(waveform$height),]
  
  smooth_vals = predict(loess(counts~height,waveform),waveform$height)
  sm.line <- as.data.frame(cbind(smooth_vals, waveform$height))
  names(sm.line) <- c("counts", "height")
  
  
#   wf <- ggplot(waveform, aes(counts, height)) + geom_point()
#   wf  + geom_path(alpha=0.5) 
#   geom_line(data=sm.line, aes(counts, height)) 
  
  plot(waveform$counts, waveform$height, pch=19, main=site, ylab="height", xlab="counts")
  lines(smooth_vals, waveform$height, col= "blue",lwd = 1.5)
  
  
  leg.text <- c(paste0("tree height: ", tree.ht), paste0("AGLB (MgC): ", biomass))
  legend("bottomright", legend=leg.text, bty="n", cex=0.85)
  
}

dev.off()

#to run Fabio's, which is in one table:
pdf(file = paste0(indir, "waveform_plots_profilesFG.pdf"), height=8, width = 8)
par(mfrow=c(2,2))

for (pl in c(2:length(profdata))) {
  
  #hack
  site <- names(profdata)[pl]
  biomass <- round(plots$AGLB__Mg_C[plots$PlotID == site], digits=2)
  tree.ht <- round(plots$Tree_Hei_2[plots$PlotID == site],digits=2)
  
  waveform <- profdata[,c(1,pl)]
  names(waveform) <- c("height", "counts")
  
  
  smooth_vals = predict(loess(counts~height,waveform),waveform$height)
  sm.line <- as.data.frame(cbind(smooth_vals, waveform$height))
  names(sm.line) <- c("counts", "height")
  
  plot(waveform$counts, waveform$height, pch=19, main=site, ylab="height", xlab="counts")
  lines(smooth_vals, waveform$height, col= "blue",lwd = 1.5)
  leg.text <- c(paste0("tree height: ", tree.ht), paste0("AGLB (MgC): ", biomass))
  legend("bottomright", legend=leg.text, bty="n", cex=0.85)
  
}

dev.off()


