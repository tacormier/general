library(raster)
library(maptools)
library(ggplot2)
library(reshape)

# compare SE_Asia Biomass maps with plot data
plots <- "/mnt/a/tcormier/SE_Asia/Ellis_Paper/field_plots/BplotsForLidarCalibBaccini.shp"

# first, the maps:
a <- "/mnt/a/fgoncalves/SE_Asia/Mapping/Amindo/Tiles_20m_geo/agb_map.tif"
k <- "/mnt/a/fgoncalves/SE_Asia/Mapping/Karya_Lestari/Tiles_20m_geo/agb_map.tif"
m <- "/mnt/a/fgoncalves/SE_Asia/Mapping/Meraang/Tiles_20m_geo/agb_map.tif"
nn <- "/mnt/a/fgoncalves/SE_Asia/Mapping/Narkata_north/Tiles_20m_geo/agb_map.tif"
ns <- "/mnt/a/fgoncalves/SE_Asia/Mapping/Narkata_south/Tiles_20m_geo/agb_map.tif"
sb <- "/mnt/a/fgoncalves/SE_Asia/Mapping/Sumalindo_bio/Tiles_20m_geo/agb_map.tif"
se <- "/mnt/a/fgoncalves/SE_Asia/Mapping/Sumalindo_east/Tiles_20m_geo/agb_map.tif"
sw <- "/mnt/a/fgoncalves/SE_Asia/Mapping/Sumalindo_west/Tiles_20m_geo/agb_map.tif"

# Fabio's modeling results:
mod.res <- read.csv("/mnt/a/tcormier/SE_Asia/Ellis_Paper/model_training/resomeresidualsplots/obs_vs_pred_mod5var.csv")

# Plot observed vs. modeled for paper:
mod.plot <- ggplot(mod.res, aes(x=agb_obs, y=agb_pred)) + geom_point() + xlab("Observed AGB (MgC/ha)") + ylab("Predicted AGB (MgC/ha)") +
  xlim(75, 385) + ylim(75, 385) + geom_abline(intercept=0, slope=1)
ggsave(paste0(figdir, "modelingResults_oneToone.pdf"), mod.plot, height=6, width=6)



# Now merge them (finally - for easier analysis)
file.list <- list(a,k,m,nn,ns,sb,se,se)
ras.list <- lapply(file.list, raster)
# because we have many rasters (not just two), use do.call
ras.merge <- do.call(merge, ras.list)

#resample to 1m for weighted mean calcs
ras.merge.res <- raster::disaggregate(ras.merge, fact=c(10,10))

# write merged level to disk - save us aggravation!
writeRaster(ras.merge, filename = "/mnt/a/fgoncalves/SE_Asia/Mapping/agb_map_merge.tif")

# now the points:
p <- readShapePoints(plots)

# Extract raster values to underlying points
p.extract <- extract(ras.merge, p)
p.extract.mean20 <- extract(ras.merge, p, buffer=20, fun=mean)
p.extract.max20 <- extract(ras.merge, p, buffer=20, fun=max)

# Try going back to original plot radius and extract weighted mean
p.extract.wmn10 <- extract(ras.merge.res, p, buffer=10, weights=T, fun=mean)
p.extract.wmx10 <- extract(ras.merge.res, p, buffer=10, weights=T, fun=max)

# Combine
agb <- as.data.frame(cbind(p$AGLB__Mg_C, p.extract, p.extract.mean20, p.extract.max20))
names(agb) <- c("field_AGB", "pixel_AGB", "mean_AGB_20mRadius", "max_AGB_20mRadius")

# weighted mean
agb.wmn <- as.data.frame(cbind(p$AGLB__Mg_C, p.extract.wmn10, p.extract.wmx10))
names(agb.wmn) <- c("field_AGB", "weightedMean_AGB", "weightedMax_AGB")

# structure data for plotting in ggplot
agb.melt <- melt(agb, id.vars="field_AGB")
agbw.melt <- melt(agb.wmn, id.vars="field_AGB")

# plot mapped AGB vs. plot value
agb.plot <- ggplot(data=agb, aes(field_AGB, modeled_AGB)) + xlim(c(0,375)) + ylim(c(0,375)) + geom_point() + geom_abline(intercept=0, slope=1) + xlab("field AGB") + ylab("Model Raster AGB")
ggsave(filename="/mnt/a/tcormier/SE_Asia/Ellis_Paper/figures/FieldAGB_vs_RasAGB.pdf", plot = agb.plot, width=7, height=7, units="in")

# plot field value vs. pixel, mean, and max values
agb.plot.fac <- ggplot(data=agb.melt, (aes(field_AGB, value, colour=variable))) + geom_point() + geom_abline(intercept=0, slope=1) + 
  xlim(c(0,375)) + ylim(c(0,375)) + facet_wrap(~variable,nrow=2) + theme(legend.justification=c(.8,.2), legend.position=c(.8,.2))
agb.plot.fac
ggsave(filename="/mnt/a/tcormier/SE_Asia/Ellis_Paper/figures/FieldAGB_vs_RasAGB_pix_mean_max.pdf", plot = agb.plot.fac, width=7, height=7, units="in")

# plot weighted mean of pixels covering the plot vs. plot value
agbwmn.plot <- ggplot(data=agb.wmn, aes(field_AGB, weightedMean_AGB)) + xlim(c(0,375)) + ylim(c(0,375)) + geom_point() + geom_abline(intercept=0, slope=1) + 
  xlab("field AGB") + ylab("Weighted Mean of Pixels Covering Plot")
agbwmn.plot

agbwmx.plot <- ggplot(data=agb.wmx, aes(field_AGB, weightedMax_AGB)) + xlim(c(0,375)) + ylim(c(0,375)) + geom_point() + geom_abline(intercept=0, slope=1) + 
  xlab("field AGB") + ylab("Weighted Max of Pixels Covering Plot")
agbwmx.plot

# Put weighted mean/max plots together w/ faceting
agbw.plot.fac <- ggplot(data=agbw.melt, (aes(field_AGB, value, colour=variable))) + geom_point() + geom_abline(intercept=0, slope=1) + 
  xlim(c(0,375)) + ylim(c(0,375)) + facet_wrap(~variable,nrow=2) + theme(legend.justification=c(.97,.03), legend.position=c(.97,.03))
agbw.plot.fac
ggsave(filename="/mnt/a/tcormier/SE_Asia/Ellis_Paper/figures/FieldAGB_vs_RasAGB_weighted_mean_max.pdf", plot = agbw.plot.fac, width=7, height=7, units="in")



##############
# Look at radius vs biomass, then look at residuals
p.df <- as.data.frame(p)
ggplot(data=p.df, aes(X20_BAF_Pri, AGLB__Mg_C)) + geom_point() + geom_smooth(method="lm")

# residuals vs. plot size
p.resid <- as.data.frame(cbind(p.df$X20_BAF_Pri, (p.df$AGLB__Mg_C - agb.wmn$weightedMean_AGB)))
names(p.resid) <- c("plot_size", "residuals_Field_Map")

ggplot(data=p.resid, aes(x=plot_size, y=residuals_Field_Map)) + geom_point() + geom_hline(yintercept=0) + geom_smooth()
#plot(p.resid$field_agb, p.resid$residuals_Field_Map)

# Look at observed vs. residuals and plot size vs residuals from fabio's model (not raster-based extraction -
# model prediction)
mod.df <- merge(mod.res, p.df, by.x = "plots", by.y="PlotID")
m.resid <- as.data.frame(cbind(mod.df$agb_obs, mod.df$agb_pred, mod.df$X20_BAF_Pri))
names(m.resid) <- c("observed", "predicted", "plot_size")
m.resid$residuals <- m.resid$observed - m.resid$predicted
#resid vs obs
ggplot(data=m.resid, aes(x=observed, y=residuals)) + geom_point() + geom_hline(yintercept=0) + geom_smooth()
#resid vs plot size
ggplot(data=m.resid, aes(x=plot_size, y=residuals)) + geom_point() + geom_hline(yintercept=0) + geom_smooth()
