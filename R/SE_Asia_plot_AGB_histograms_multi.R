library(raster)

l1 <- "/mnt/a/fgoncalves/SE_Asia/Mapping/Amindo/Tiles_20m_geo/agb_map.tif"

l2 <- "/mnt/a/fgoncalves/SE_Asia/Mapping/Karya_Lestari/Tiles_20m_geo/agb_map.tif"

l3 <- "/mnt/a/fgoncalves/SE_Asia/Mapping/Meraang/Tiles_20m_geo/agb_map.tif"

l4 <- "/mnt/a/fgoncalves/SE_Asia/Mapping/Narkata_north/Tiles_20m_geo/agb_map.tif"
l5 <- "/mnt/a/fgoncalves/SE_Asia/Mapping/Narkata_south/Tiles_20m_geo/agb_map.tif"

l6 <- "/mnt/a/fgoncalves/SE_Asia/Mapping/Sumalindo_bio/Tiles_20m_geo/agb_map.tif"
l7 <- "/mnt/a/fgoncalves/SE_Asia/Mapping/Sumalindo_east/Tiles_20m_geo/agb_map.tif"
l8 <- "/mnt/a/fgoncalves/SE_Asia/Mapping/Sumalindo_west/Tiles_20m_geo/agb_map.tif"

#######################################
pd1.vec <- getValues(raster(l1))
pd2.vec <- getValues(raster(l2))
pd3.vec <- getValues(raster(l3))
pd4.vec <- getValues(raster(l4))
pd5.vec <- getValues(raster(l5))
pd6.vec <- getValues(raster(l6))
pd7.vec <- getValues(raster(l7))
pd8.vec <- getValues(raster(l8))

# combining a couple of sites:
l9 <- c(pd4.vec, pd5.vec)
l10 <- c(pd6.vec, pd7.vec, pd8.vec)

# Put all sites into one df for quick hist plotting with same y axis.
all <- data.frame(dataset=c(rep('Amindo', length(pd1.vec)), rep('Karya_Lestari', length(pd2.vec)), rep('Meraang', length(pd3.vec)), rep("Narkata", length(l9)), rep("Sumalindo", length(l10))), value=c(pd1.vec, pd2.vec, pd3.vec, l9, l10))
# Suggestions from Wayne:
# 1. cut off biomass at 700 t/ha
all.sub <- all[all$value >= 1 & all$value <= 700,]
all.sub <- all.sub[!is.na(all.sub$dataset),]

plotfile <- "/mnt/a/tcormier/SE_Asia/Ellis_Paper/field_plots/BplotsForLidarCalibBaccini_removeColumns.csv"
plots <- read.csv(plotfile)
mean(plots$AGLB__Mg_C)
sd(plots$AGLB__Mg_C)

p <- data.frame(dataset="Field Plots", value=plots$AGLB__Mg_C)
all.sub <- rbind(all.sub, p)

# some simple stats
names <- names(tapply(all.sub$value, INDEX = all.sub$dataset, FUN = mean))
means <- sprintf("%.2f", round(tapply(all.sub$value, INDEX = all.sub$dataset, FUN = mean),digits=2))
sd <- sprintf("%.2f", round(tapply(all.sub$value, INDEX = all.sub$dataset, FUN = sd), digits=2))

# label placement df
labs=paste0("mean AGB = ", means, "\n   sd = ", sd)
lab.df <- data.frame(x=rep(500, length(sd)), y=rep(0.18, length(sd)), dataset=names,labels=labs)
#histogram
# to get same y axis, used the first answer from this link: http://stackoverflow.com/questions/22181132/normalizing-y-axis-in-histograms-in-r-ggplot-to-proportion-by-group
# COLOR
d.plot <- ggplot(all.sub,aes(x=value,fill=dataset))+ xlim(0,700)+ ylab("Density")+xlab("AGB (MgC/ha)")+
            geom_histogram(aes(y= 25*..density..),binwidth=25)+
            scale_fill_discrete(name="Site") +
            facet_wrap(~dataset,nrow=2)
# B&W
d.plot <- ggplot(all.sub,aes(x=value))+ xlim(0,700)+ ylab("Density")+xlab("AGB (MgC/ha)")+
  geom_histogram(aes(y= 25*..density..),binwidth=25)+
  scale_fill_discrete(name="Site") +
  facet_wrap(~dataset,nrow=2)
d.plot
#to add color legend back in where field plots are listed: 
#theme(legend.justification=c(.9,.2), legend.position=c(.9,.2))+
d.plot <- d.plot + geom_text(aes(x, y, label=labels, group=NULL),data=lab.df, size=3, hjust=0.5)
d.plot

ggsave("/mnt/a/tcormier/SE_Asia/Ellis_Paper/figures/agb_histograms_subset_BW.pdf", plot = d.plot, width = 8, height=8)


# ggplot(data=plots,aes(x=AGLB__Mg_C))+ ylab("density")+xlab("AGB (MgC/ha)")+
# geom_histogram(aes(y= 25*..density..),binwidth=25)
