library(ggplot2)
library(mgcv)
library(gridExtra)
library(raster)
library(pbapply)
library(rgdal)
library(stringr)

source("/mnt/a/tcormier/scripts/general/R/handy_functions_TC.R")
rasterOptions(tmpdir="/home/tcormier/RasTmpDir/")

# do you want to create the profile or does it already exist ("y" or "n")
createprof <- "n"
# if profiles already exist, in what directory?
prof.file <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170215/norm_fil/filtered_las_profiles/"
# line type = "smooth" or "exact"
lineType <- "smooth"
smoothVal <- 0.15
#ylim.waves <- c(0,80)
#xlim.waves <- c(0,200)
# If profile must be created:
indir <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/las_extract/extract_20150918/"
#indir <- "/mnt/r/Mex_Lidar/Cartodata/Campeche_Yucatan/LAS/Q1/Tiles_50m/"
#indir <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/txt/CampecheYucatan/"
# csv or shapefile - gets identifying info from shapefile to title the plot
plotfile <- "/mnt/a/tcormier/Mexico_CMS/field/points_wgs84_updatedFields_points/CMS_FieldPoints_wgs84_updatedFields_20170228.shp"
figdir <- "/mnt/a/tcormier/Mexico_CMS/lidar/field_intersect/all_20170215/norm_fil/filtered_las_waveform_plots/"
binsize <- 0.5 #vertical resolution of profile (m)

# Values for flagging bad points or tiles - based on field data or expert knowledge
# max.height <- 150
# min.height <- -6

# dtm.file <- "/mnt/a/fgoncalves/Mexico/dtm_mosaic.tif"
#dtm.file <- "/mnt/r/Mex_Lidar/Cartodata/Campeche_Yucatan/Deliverables/Mosaics/Q1/Q1_DTM.tif"
#dtm.file <- "/mnt/r/Mex_Lidar/Cartodata/Oaxaca2/Deliverables/Mosaics/DTM.tif"
#############################################
if (createprof == "y") {
  dtm <- raster(dtm.file)
  plotlas.files <- list.files(indir, ("*.txt$"), full.names=T)
  plotlas <- pblapply(plotlas.files, read.table, sep=",",col.names = c("x","y","z","i","a","n","r","c"))
  
  # Calc waveforms here and plop them into a list, so we don't have to keep re-running it to try different plots.
  print(paste0("Starting profile calcs at ", Sys.time()))
  makeprofile <- pblapply(plotlas, makeprof, res=binsize, dtm=dtm, max.height=max.height, min.height=min.height)
  print(paste0("Finished profile calcs at ", Sys.time()))
  #pull out just the profiles from the function returns
  profile <- pblapply(makeprofile, "[[", 1)
  
} else if (createprof == "n") {
  profs <- plotlas.files <- list.files(prof.file, "*.csv", full.names=T)
  profile <- pblapply(profs, read.csv)
  
} else {
  stop(paste0("the variable 'createprof' must be 'y' or 'n' (lowercase). You entered ", createprof))
}

# Use site name as title -  for SE Asia
# sitenames <- function(filenames) {
#   paste(unlist(strsplit(unlist(strsplit(filenames, "_"))[10:11], "\\."))[1:2], collapse="_")
# }

# For Mexico - the sitename = folio from shapefile
# sitenames <- function(filename) {
#   name <- sub("_infysPlots_UTM[0-9][0-9]N","",basename(filename))
#   name <- sub("_profile.csv", "", name)
#   return(name)
# }

if (!is.null(plotfile)) {
  plot.ext <- unlist(strsplit(plotfile, "\\."))[2]
  if (plot.ext == "csv") {
    plots <- read.csv(plotfile)
  } else if (plot.ext == "shp") {
    plots <- shapefile(plotfile)
  }
}

  # Build a name for the profile - specific to mexico
  prof.id1 <- basename(plotlas.files)
  prof.id <- sapply(strsplit(prof.id1, "_"), function(x) x[[2]])
  # Use a regex to get site name
  prof.site1 <- sub(".*CMS_FieldPolygons_updatedFields_20160727_UTM\\d{2}_", "", basename(plotlas.files))
  prof.site <- sub("_UTM\\d{2}N_groundIndex_profile\\.csv", "", prof.site1)
  sites <- paste0("ID_TC_", prof.id, "_", prof.site)
  # get site names from profile names
  # sites <- pblapply(plotlas.files, sitenames)
  names(profile) <- sites
# } else {
#   sites <- lapply(plotlas.files, basename)
#   # n <- lapply(plotlas, nrow)
#   names(profile) <- sites
#   #names(profile) <- paste0(sites, "   n = ", n)
#   # names(profile) <- sites
# }

# Find xlim and ylim
# saveAll <- profile
# profile <- profile[29:30]
# profile.orig <- profile
profile <- profile.orig
names(profile) <- sites
profile <- mapply(cbind, profile, "ID"=as.numeric(prof.id), SIMPLIFY=F)
profile <- pblapply(profile, function(x) merge(x, plots@data, by.x="ID", by.y="ID_TC", all.x=T))
counts_size <- pblapply(profile, function(x) x$counts/x$Plot_m2)
profile <- mapply(cbind, profile, "counts_size"=(counts_size), SIMPLIFY=F)
# remove plot 223 bc it's whack.
profile <- profile[-223]

# xmax <- roundUp(pbsapply(do.call("rbind",profile),max)[2],to = 5)
# ymax <- roundUp(as.numeric(pbsapply(do.call("rbind",profile),max))[2],to = 5)
# xmax <- roundUp(pbsapply(do.call("rbind",profile),max)[3],to = 5)
# normalize counts - since plots are all different sizes, this helps with the x-axis!
# FIX this so I'm normalizing based on the entire data set, not separately normalizing each profile 
all.prof <- do.call("rbind",profile)
min.count <- min(all.prof$counts) 
max.height <- max(all.prof$height)
# max.count <- max(all.prof$counts) 
# max.5perc <- quantile(all.prof$counts, 0.95)
# max.1perc <- quantile(all.prof$counts, 0.99)
# max.count <- max.1perc

# now add on plot size so we can remove the effect of that
# all.prof$id <- as.numeric(sapply(strsplit(row.names(all.prof), "_"), function(x) x[[3]]))
# all.prof.join <- merge(all.prof, plots, by.x="id", by.y="ID_TC", all.x=T)
# all.prof$counts_size <- all.prof$counts/all.prof.join$Plot_m2
max.5perc <- quantile(all.prof$counts_size, 0.95)
max.1perc <- quantile(all.prof$counts_size, 0.99)
max.count <- max.1perc

rescale.profile <- function(vec, oldmin, oldmax, newmin, newmax) {
  
  # rasterOptions(tmpdir="/home/tcormier/RasTmpDir/")
  newrange <- c(newmin, newmax)
  normalized = (vec - oldmin)/(oldmax - oldmin) * (newmax - newmin) + newmin
  
  return(normalized)
} # end rescaleRange function


norm.count <- pblapply(profile, function(x) rescale.profile(x$counts_size, min.count, max.count, 0, 1))
# norm.count <- rescale.profile(all.prof$counts_size, min.count, max.count, 0, 1)

# norm.add2Prof <- pblapply(profile, function(x) cbind.data.frame(x, norm.count))
norm.add2Prof <- mapply(cbind, profile, "norm_counts"=norm.count, SIMPLIFY=F)
# profile.orig <- profile
profile <- norm.add2Prof

xmax <- 1
ymax <- 50
xlim.waves <- c(0,xmax)
ylim.waves <- c(0,ymax)

# If no waveform=specific text to be added to plots, create plots from this one line:
#plotlist <- lapply(profile, plotWaveform, nameHeightCol="height", nameCountCol="counts", ylim=ylim.waves, smoothFactor=0.075, lineType=lineType, plot.title=names(plotlas))
# plotwf <- function(x) {
#   plotWaveform(waveformDF=profile[[x]], nameHeightCol="height", nameCountCol="counts", ylim=ylim.waves, xlim=xlim.waves, smoothFactor=smoothVal, lineType=lineType, plot.title=x)  
# }
# plotlist <- pblapply(names(profile), plotwf)
#plotlist[[2]]

#Otherwise, loop through.
#maxhts <- data.frame(site=NA, field_maxht=NA, lidar_maxht=NA)
#field_AGC=NA, lidar_AGC=NA)

plotlist <- list()
for (i in 1:length(profile)) {
  #hack
  prof <- profile[[i]]
  site <- names(profile[i])
  #ANOTHER WICKED HACK to make the x axis a bit smaller
  # prof$counts[prof$counts > xmax] <- xmax
  prof$norm_counts[prof$norm_counts > xmax] <- xmax
  #used sprintf to keep trailing zeros
  # id <- sub("_", "", str_extract(site, "_[0-9]+"))
  id <- prof.id[i]
  title <- paste0(gsub("_", " ", prof.site[i]), "\nID ", gsub("_", " ", id))
  
  biomass <- sprintf("Field AGB = %.2f Mg/ha", round(plots@data$Bio_MgHa[plots@data$ID_TC == id], digits=2))
  #tree.ht <- sprintf("%.2f", round(plots$Tree_Hei_2[plots$PlotID == site], digits=2))
  # A little check:
  # ch1 <- paste0("site = ", site, ". Field layer biomass = ", plots@data$Bio_MgHa[plots@data$Bio_MgHa == id], ".")
  # print(ch1)
  #maxhts[i,] <- c(site, tree.ht, max(prof$height))
  #biomass, NA)
  
  # Add text to plot (still working on making this prettier - a little bit of a hack w/ adding spaces to 
  # make things sort of line up better.)
  # annotation <- paste0("Field Data:              \nTallest Tree = ", tree.ht, " m \nAGB = ", biomass, " MgC/ha")
  annotation <- biomass
  plotlist[[i]] <- plotWaveform(prof, nameHeightCol="height",nameCountCol="norm_counts", plot.title=title, leg.txt = annotation, 
                                ylim=ylim.waves, xlim=xlim.waves, lineType=lineType, smoothFactor = smoothVal)
  # print(plotlist[[i]])
  # Sys.sleep(3)
  
} #end profile loop

# # PRINT 2x2 per page of a pdf!
# args.list <- c(plotlist, 2,2)
# names(args.list) <- c(1:length(plotlist), "nrow", "ncol")
# ggsave(paste0(figdir, "waveform_plots_SEAsia_", lineType, "Lines_10.pdf"), do.call("marrangeGrob", args.list), height=8, width=8)
#ggsave(paste0(figdir, "waveform_plots_SEAsia_", lineType, "Lines_10.pdf"), plotlist, height=8, width=8)
# filename <- paste0(figdir, "/CMS_FieldPlot_waveforms_", lineType,"_", sub("\\.", "", smoothVal), ".pdf")
filename <- paste0(figdir, "/CMS_FieldPlot_waveforms_", lineType,"_divPlotSize_norm0-1", ".pdf")

pdf(file=filename, onefile=T, width=8, height=8)
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

