library(ggplot2)
library(plyr)
library(gridExtra)
#library(plotly)
source("/mnt/a/tcormier/scripts/general/R/handy_functions_TC.R")

# Sys.setenv("plotly_username"="tcormier")
# Sys.setenv("plotly_api_key"="v5vxzo2poa")

linfile <- "/mnt/a/tcormier/testing/lidar_processing/Tiles_20m/metrics.csv"
winfile <- "/mnt/a/tcormier/testing/lidar_processing/windows/Tiles_20m/metrics.csv"

lm_eqn <- function(df){
  m <- lm(df[[w.name]] ~ df[[l.name]]);
  eq <- substitute(italic(r)^2~"="~r2, 
                   list(r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

lin <- read.csv(linfile, as.is = T)
win <- read.csv(winfile, as.is = T)

l.list <- w.list <- data.frame(lasfile=NA, n=NA)

for (i in (1:nrow(lin))) {
  x <- read.table(lin[i, 1], sep=",")
  l.list[i, 1] <- lin[i, 1]
  l.list[i, 2] <- nrow(x)
  
  y <- read.table(win[i, 1], sep=",")
  w.list[i, 1] <- win[i, 1]
  w.list[i, 2] <- nrow(y)
}

# let's first look at some numbers = # points per tile
# l.list <- as.vector(read.csv(linlist, as.is=T)[,1])
# w.list <- as.vector(read.csv(winlist, as.is=T)[,1])

if (identical(basename(l.list$lasfile), basename(w.list$lasfile))) {
  list.diff <- w.list$n - l.list$n
  diff.files <- rep(0, length(list.diff))
  for (diff in (1:length(list.diff))) {
    if (list.diff[diff] != 0) {
      diff.files[diff] <- basename(l.list[diff,1])
    }
  }
}


# Remove 0's (those files are the same)
diff.files <- diff.files[diff.files != "0"]

# # Copy these files into their respective windows and linux directories
# lincopy <- paste0("/mnt/a/tcormier/testing/lidar_processing/Tiles_20m/", diff.files)
# wincopy <- paste0("/mnt/a/tcormier/testing/lidar_processing/windows/Tiles_20m/", diff.files)
# file.copy(lincopy, "/mnt/a/tcormier/testing/lidar_processing/indiv_tile_testing/linux/",overwrite=T)
# file.copy(wincopy, "/mnt/a/tcormier/testing/lidar_processing/indiv_tile_testing/windows/",overwrite=T)

# Now run over to the waveform ggplot code and plot these up and see if they are different!

plot(w.list$n, l.list$n)


lin.comp <- complete.cases(lin)
win.comp <- complete.cases(win)
lin <- lin[lin.comp,]
win <- win[win.comp,]

m.names <- names(lin)

plotlist <- list()

# iterator for plotlist bc not every column will be numeric, which will throw off
# adding to the plotlist.
it <- 1
for (i in 1:length(m.names)) {
  if (is.numeric(lin[,i])) {
    l.name <- paste0("lin_", m.names[i])
    w.name <- paste0("win_", m.names[i])
    df <-cbind.data.frame(basename(lin[,1]), lin[,i], win[,i])
    names(df) <- c("tile", l.name, w.name)
    df$abs_diff <- abs(df$lin_npeaks - df$win_npeaks)
    
    
    lm.met <- lm(win[,i] ~ lin[,i])
    p <- ggplot(data=df, aes_string(x=l.name, y=w.name))
    p1 <- p + geom_point() + stat_smooth(method="lm", se=F, color="tomato2") +
      annotate("text", y= max(df[[w.name]]), x =min(df[[l.name]]),label=lm_eqn(df),parse=T, hjust=.1)
    p1
    
    
    plotlist[[it]] <- p1
    it=it+1
  }
  
}

plotlist <- list()
it <- 1
for (i in 1:length(m.names)) {
  if (is.numeric(lin[,i])) {
    l.name <- paste0("lin_", m.names[i])
    w.name <- paste0("win_", m.names[i])
    df <- as.data.frame(cbind(lin[,i], win[,i]))
    df <- cbind(df, basename(win$lasfiles))
    names(df) <- c(l.name, w.name, "files")
    df$files <- as.character(df$files)
    
    
    
    df$diff <- round((df[[w.name]] - df[[l.name]]), digits=2)
    
    lm.met <- lm(win[,i] ~ lin[,i])
    p <- ggplot(data=df, aes_string(x=l.name, y=w.name, label="files"))
    p <- p + geom_text()
    
    p1 <- p + geom_point() + stat_smooth(method="lm", se=F, color="tomato2") +
      annotate("text", y= max(df[[w.name]]), x =min(df[[l.name]]),label=lm_eqn(df),parse=T, hjust=.1)
    p1
    
    
    plotlist[[it]] <- p
    it=it+1
  }
  
}
#ggplotly(plotlist[[10]])

# # PRINT 2x2 per page of a pdf!

pdf(file="/mnt/a/tcormier/testing/lidar_processing/win_lin_compare.pdf", onefile=T, width=8, height=8)
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


# From this plotting exercise, I have identified a few tiles that seem to have
# different points in them (more than just by the n, but the waveforms are different).
# So looking at the differences in npeaks, for example, I find 3 tiles that are 6 or 7 
# (the max difference) peaks different from each other. Let's ID points in one of those 
# pairs where the xyz are different and plot them:

# Tiles to choose from: Amindo_447860_201960.txt, Amindo_447960_202280.txt, Amindo_447980_201840.txt
# We'll start with Amindo_447960_202280.txt

lt <- read.table("/mnt/a/tcormier/testing/lidar_processing/Tiles_20m/Amindo_447960_202280.txt", sep=",",col.names = c("x","y","z","i","a","n","r","c"))
wt <- read.table("/mnt/a/tcormier/testing/lidar_processing/windows/Tiles_20m/Amindo_447960_202280.txt", sep=",",col.names = c("x","y","z","i","a","n","r","c"))

# lt$x <- round(lt$x, digits=1)
# lt$y <- round(lt$y, digits=1)
# lt$z <- round(lt$z, digits=1)
# 
# wt$x <- round(wt$x, digits=1)
# wt$y <- round(wt$y, digits=1)
# wt$z <- round(wt$z, digits=1)

lt$xyz <- paste(lt$x,lt$y,lt$z, sep="_")
wt$xyz <- paste(wt$x,wt$y,wt$z, sep="_")
names(lt) <- paste0("lt_", names(lt))
names(wt) <- paste0("wt_", names(wt))

# remove the last row from wt to make them even
wt <- wt[-(nrow(wt)),]

xdiff <- data.frame(lt$lt_x, wt$wt_x, diff=abs(lt$lt_x-wt$wt_x))
ydiff <- data.frame(lt$lt_y, wt$wt_y, diff=abs(lt$lt_y-wt$wt_y))
zdiff <- data.frame(lt$lt_z, wt$wt_z, diff=abs(lt$lt_z-wt$wt_z))

hist(xdiff$diff[1:(which(xdiff$diff > 0.010001)[1]-1)], breaks=100, xlab="xdiff", col="blue", main="hist")
hist(ydiff$diff[1:(which(ydiff$diff > 0.010001)[1]-1)], breaks=100, xlab="ydiff", col="blue", main="hist")
hist(zdiff$diff[1:(which(zdiff$diff > 0.010001)[1]-1)], breaks=100, xlab="zdiff", col="blue", main="hist")

xoff <- xdiff[1:(which(xdiff$diff > 0.010001)[1]-1),]

# plot first 1282 points (before the extra point comes in)
plot(xdiff$lt.lt_x[1:(which(xdiff$diff > 0.010001)[1]-1)], ydiff$lt.lt_y[1:(which(xdiff$diff > 0.010001)[1]-1)],
     xlab = "x", ylab="y")
points(xdiff$wt.wt_x[1283], ydiff$wt.wt_y[1283], col="red", pch=19)

# Some comparison
# Which xyz are in lt that are not in wt - this is how I discovered that points were
# .01m off.
lt.unique <- lt[!(lt$xyz %in% wt$xyz),]
wt.unique <- wt[!(wt$xyz %in% lt$xyz),]

plot(lt$x, lt$y)
points(lt.unique$x, lt.unique$y, col="red", pch=20)

