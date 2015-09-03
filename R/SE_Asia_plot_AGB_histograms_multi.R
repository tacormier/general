# We wanted to know if the distribution of the plots matched the distribution of biomass in the 
# overall sites.
library(ggplot2)
library(gridExtra)
library(raster)

# l1 <- "/mnt/a/fgoncalves/SE_Asia/Mapping/Amindo/Tiles_20m_geo/agb_map.tif"
# 
# l2 <- "/mnt/a/fgoncalves/SE_Asia/Mapping/Karya_Lestari/Tiles_20m_geo/agb_map.tif"
# 
# l3 <- "/mnt/a/fgoncalves/SE_Asia/Mapping/Meraang/Tiles_20m_geo/agb_map.tif"
# 
# l4 <- "/mnt/a/fgoncalves/SE_Asia/Mapping/Narkata_north/Tiles_20m_geo/agb_map.tif"
# l5 <- "/mnt/a/fgoncalves/SE_Asia/Mapping/Narkata_south/Tiles_20m_geo/agb_map.tif"
# 
# l6 <- "/mnt/a/fgoncalves/SE_Asia/Mapping/Sumalindo_bio/Tiles_20m_geo/agb_map.tif"
# l7 <- "/mnt/a/fgoncalves/SE_Asia/Mapping/Sumalindo_east/Tiles_20m_geo/agb_map.tif"
# l8 <- "/mnt/a/fgoncalves/SE_Asia/Mapping/Sumalindo_west/Tiles_20m_geo/agb_map.tif"

l1 <- "/mnt/a/tcormier/SE_Asia/Ellis_Paper/results/maps/Amindo.tif"

l2 <- "/mnt/a/tcormier/SE_Asia/Ellis_Paper/results/maps/Karya_Lestari.tif"

l3 <- "/mnt/a/tcormier/SE_Asia/Ellis_Paper/results/maps/Meraang.tif"

l4 <- "/mnt/a/tcormier/SE_Asia/Ellis_Paper/results/maps/Narkata_north.tif"
l5 <- "/mnt/a/tcormier/SE_Asia/Ellis_Paper/results/maps/Narkata_south.tif"

l6 <- "/mnt/a/tcormier/SE_Asia/Ellis_Paper/results/maps/Sumalindo_bio.tif"
l7 <- "/mnt/a/tcormier/SE_Asia/Ellis_Paper/results/maps/Sumalindo_east.tif"
l8 <- "/mnt/a/tcormier/SE_Asia/Ellis_Paper/results/maps/Sumalindo_west.tif"

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
#9 <- c(pd4.vec, pd5.vec)
l10 <- c(pd6.vec, pd7.vec, pd8.vec)

# Put all sites into one df for quick hist plotting with same y axis.
# all <- data.frame(dataset=c(rep('Amindo', length(pd1.vec)), rep('Karya_Lestari', length(pd2.vec)), rep('Meraang', length(pd3.vec)), rep("Narkata", length(l9)), rep("Sumalindo", length(l10))), value=c(pd1.vec, pd2.vec, pd3.vec, l9, l10))
#  all <- data.frame(dataset=c(rep('D', length(pd1.vec)), rep('H', length(pd2.vec)), rep('G', length(pd3.vec)), rep("B 2010", length(pd5.vec)), 
#                              rep("B 2011", length(pd4.vec)), rep("C", length(l10))), 
#                   value=c(pd1.vec, pd2.vec, pd3.vec, pd5.vec, pd4.vec, l10))

all <- data.frame(dataset=c(rep("B 2010", length(pd5.vec)), rep("B 2011", length(pd4.vec)), rep("C", length(l10)), rep("CNI", 1), rep('D', length(pd1.vec)), rep('G', length(pd3.vec)), rep('H', length(pd2.vec))), 
                  value=c(pd5.vec, pd4.vec, l10, 1, pd1.vec, pd3.vec,pd2.vec))

#rep("DNI", 1), rep("Field Plot Data", 1)
# Suggestions from Wayne:
# 1. cut off biomass at 700 t/ha
all.sub <- all[all$value >= 1 & all$value <= 700,]
all.sub <- all.sub[!is.na(all.sub$dataset),]
# This is a dummy row for plotting - want it to be 0 so it doesn't mess up the y axis limits.
all.sub <- all.sub[!(all.sub$dataset == "CNI"),]
# all.sub[(all.sub$dataset == "CNI"),] <- NA
plotfile <- "/mnt/a/tcormier/SE_Asia/Ellis_Paper/field_plots/BplotsForLidarCalibBaccini_poly_10m_rad_addNoBiomassPlots.csv"
plots <- read.csv(plotfile)
# remove the second zero biomass plot (we only used one in the model)
plots <- plots[-30,]
mean(plots$AGLB__Mg_C)
sd(plots$AGLB__Mg_C)

p <- data.frame(dataset="Field Plot Data", value=plots$AGLB__Mg_C)
all.sub <- rbind(all.sub, p)
#all.sub$dataset <- factor(all.sub$dataset, levels=c("B 2010", "B 2011", "C", "DNI", "D", "G", "H", "Field Plot Data"))
#all.sub$dataset <- factor(all.sub$dataset, levels)

# some simple stats
names <- names(tapply(all.sub$value, INDEX = all.sub$dataset, FUN = mean))
means <- sprintf("%.2f", round(tapply(all.sub$value, INDEX = all.sub$dataset, FUN = mean),digits=2))
sd <- sprintf("%.2f", round(tapply(all.sub$value, INDEX = all.sub$dataset, FUN = sd), digits=2))
means[means=="NA"] <- "0.00"
#Move the zero value to position 4 (this has to do with facet positioning) - also change names position
# means <- append(means, means[[length(means)]], after=3)
sd[sd=="NA"] <- "0.00"
# sd <- append(sd, sd[[length(sd)]], after=3)
# names <- append(names, names[[length(names)]], after=3)

#Now remove the last value - what a pain
# means <- means[-(length(means))]
# sd <- sd[-(length(sd))]
# names <- names[-(length(names))]

# label placement df
#labs=paste0("Mean = ", means, "\n sd = ", sd)
#lab.df <- data.frame(x=rep(500, length(sd)), y=rep(0.22, length(sd)), dataset=names,labels=labs)

# lab.mean <- paste0("bar(x)", " = ", means)
m.eqn <- function(df) {
  eq <- substitute("Mean" == m, list(m = df$means))
  return(as.character(as.expression(eq)))
}

sd.eqn <- function(df) {
  eq <- substitute("Std dev" == m, list(m = df$sd))
  return(as.character(as.expression(eq)))
}  
  
df.means <- as.data.frame(cbind(names, means),stringsAsFactors=F) 
#df.means$names <- factor(df.means$names, levels=c("B 2010", "B 2011", "C","DNI", "D","G", "H", "Field Plot Data"))
#df.means$means <- as.numeric(df.means$means)
# This function returns the values in a different order (alphabetical), thus the hack below to reorder them. Didn't
# want to spend any more time on this!
eq1 <- ddply(df.means,.(names),m.eqn)
lab.mean.df <- data.frame(x=rep(275, length(means)), y=rep(0.205, length(means)), dataset=eq1$names,labels=eq1$V1, stringsAsFactors=F)

df.sd <- as.data.frame(cbind(names, sd), stringsAsFactors=F)  
#df.sd$names <- factor(df.sd$names, levels=c("B 2010", "B 2011", "C","DNI", "D","G", "H", "Field Plot Data"))
#df.sd$sd <- as.numeric(df.sd$sd)
# This function returns the values in a different order, thus the hack below to reorder them. Didn't
# want to spend any more time on this!
eq2 <- ddply(df.sd,.(names),sd.eqn)
lab.sd.df <- data.frame(x=rep(275, length(sd)), y=rep(0.190, length(sd)), dataset=eq2$names,labels=eq2$V1)
#split <- 1:length(sd)

# Total hack because I can't figure out how to get these back in the order of the factor.
# Ain't nobody got time for this!
lab.mean.df <- lab.mean.df[c(1:5,7,8,6),]
lab.sd.df <- lab.sd.df[c(1:5,7,8,6),]

# histogram
# to get same y axis, used the first answer from this link: http://stackoverflow.com/questions/22181132/normalizing-y-axis-in-histograms-in-r-ggplot-to-proportion-by-group
# COLOR
# d.plot <- ggplot(all.sub,aes(x=value,fill=dataset))+ xlim(0,700)+ ylab("Density")+xlab("AGB (MgC/ha)")+
#             geom_histogram(aes(y= 25*..density..),binwidth=25)+
#             scale_fill_discrete(name="Site") +
#             facet_wrap(~dataset,nrow=2)
# B&W ## FIGURE OUT WHY TEXT DISPLAYS PROPERLY WTIHOUT ADDING EMPTY PLOT, BUT NOT WITH IT
d.plot <- ggplot(all.sub,aes(x=value))+ xlim(0,700)+ ylab("Density")+xlab("AGB (MgC/ha)")+
  geom_histogram(aes(y= 25*..density..),binwidth=25) + theme_bw() +
  scale_fill_discrete(name="dataset")  + theme(axis.title.y=element_text(vjust=1)) + theme(axis.title.x=element_text(vjust=0.1)) +
  facet_wrap(~dataset,nrow=2, drop=FALSE) + theme(strip.text.x=element_text(size=11))
d.plot

# d.plot <- ggplot(all.sub,aes(x=value))+ xlim(0,700)+ ylab("Density")+xlab("AGB (MgC/ha)")+
#   geom_histogram(aes(y= 25*..density..),binwidth=25)+theme_bw() +
#   scale_fill_discrete(name="dataset2") +
#   facet_wrap(~dataset2, nrow=2, drop=FALSE) + theme(strip.text.x=element_text(size=11)) + geom_text(data=lab.mean.df, aes(x, y, label=labels, group=NULL), parse=T, size=3, hjust=0) +
#   geom_text(data=lab.sd.df, aes(x, y, label=labels, group=NULL), parse=T, size=3, hjust=0)
# d.plot

d.plot2 <- d.plot + geom_text(data=lab.mean.df, aes(x, y, label=labels, group=NULL), parse=T, size=3, hjust=0) +
  geom_text(data=lab.sd.df, aes(x, y, label=labels, group=NULL), parse=T, size=3, hjust=0)
d.plot2

g <- ggplotGrob(d.plot2)
## remove empty panels
g$grobs[names(g$grobs) %in% c("panel4", "strip_t4")] <- NULL
## remove them from the layout
g$layout <- g$layout[!(g$layout$name %in% c("panel-4", "strip_t-4")),]
## move axis closer to panel
g$layout[g$layout$name == "axis_b-9", c("t", "b")] = c(9,9)

grid.newpage()
rem.plot <- grid.draw(g)

#d.plot
#to add color legend back in where field plots are listed: 
#theme(legend.justification=c(.9,.2), legend.position=c(.9,.2))+
# #d.plot <- d.plot + geom_text(aes(x, y, label=labels, group=NULL),data=lab.df, size=3, hjust=0.5)
# d.plot <- d.plot + geom_text(aes(x, y, label=labels, group=NULL),data=lab.mean.df, parse=T, size=3, hjust=0) +
#   geom_text(aes(x, y, label=labels, group=NULL),data=lab.sd.df, parse=T, size=3, hjust=0)
# d.plot

#ggsave does not work with grob object - what a pain! Use pdf. 
#ggsave("/mnt/a/tcormier/SE_Asia/Ellis_Paper/figures/agb_histograms_subset_BW_GroundAdded_rearrange.pdf", plot = grid.draw(g), width = 7.5, height=5.5)
pdf(file = "/mnt/a/tcormier/SE_Asia/Ellis_Paper/figures/agb_histograms_subset_BW_GroundAdded_rearrange.pdf", height = 5.5, width=7.5)
grid.draw(g)
dev.off()

# ggplot(data=plots,aes(x=AGLB__Mg_C))+ ylab("density")+xlab("AGB (MgC/ha)")+
# geom_histogram(aes(y= 25*..density..),binwidth=25)
