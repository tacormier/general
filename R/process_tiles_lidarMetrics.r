require(signal)

#control
#indir <- "R:\\Mex_Lidar\\Cartodata\\Chiapas\\LAS\\T1"
#indir <- "/mnt/r/Mex_Lidar/Cartodata/Chiapas/LAS/T1/"
indir <- "/mnt/a/tcormier/SE_Asia/Ellis_Paper/lidar/Sumalindo_bio/Tiles_20m/"
tilesize <- 20 #in meters
binsize <- 0.5 #vertical resolution of profile (m)
min_ptden <- 8 #min CartoData = 8 ppm2; min SL Brazil = 4 ppm2
vegflag <- "F" #(T/F): vegetation returns classified?


#get filenames and coordinates (lower-left corner of the tiles)
#setwd(paste0(indir,"//Tiles_50m"))
setwd(indir)
print(indir)
lasfiles <- list.files(path=indir, pattern="[.]txt", full.names=T)
dummy <- sub(".txt", "", lasfiles)
dummy <- lapply(strsplit(dummy, "_"), rev)
xcor <- as.numeric(sapply(dummy, "[", 2))
ycor <- as.numeric(sapply(dummy, "[", 1))
plot(xcor,ycor)


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


#process each tile
  
  #define metrics, etc
  dummyn <- paste0("ptden_lt", min_ptden)
  log <- array(0, c(length(lasfiles), 7))
    colnames(log) <- c("no_ground", "no_veg", "wlen_gt60", "wlen", "ground_lt_binsize", dummyn, "ptden")
  ptden <- array(NA, length(lasfiles))
  freq <- seq(0.01, 0.3, by=0.001)
  nf <- length(freq)
  amp <- array(NA, c(length(lasfiles), nf))
    colnames(amp) <- gsub("p0.", "p.", paste0("amp", format(freq, digits=2)))
  pha <- array(NA, c(length(lasfiles), nf))
    colnames(pha) <- gsub("a0.", "a.", paste0("pha", format(freq, digits=2)))
  trad <- array(NA, c(length(lasfiles), 10))
    colnames(trad) <- c("meanh", "stdevh", "h25", "h50", "h75", "mch", "npeaks", "ent", "fslope", "g2t")

  #loop
  for (p in 1:length(lasfiles)) {
    print(paste0("calculating metrics for file ", p, " of ", length(lasfiles)))
    #print(paste0("calculating metrics for ", indir, lasfiles[p]))
    lasdata <- read.csv(lasfiles[p], header=F, col.names=c("x","y","z","i","a","n","r","c"))
    #x,y,z - coordinates; i - intensity; a - scan angle; n - number of returns for given pulse; r - number of this return; c - classification number
    
    # For the SE_Asia stuff, the x an y are not in the file name, so here I define xcor and ycor as the upper left corner of the tile (min x and max y)
#     xcor <- min(lasdata$x)
#     ycor <- max(lasdata$y)
    
    #if (length() > ) lasdata <- lasdata[-which(is.na(lasdata$z)),]
    #tile quality check
    if (!(2 %in% lasdata$c)) {log[p,1] <- 1; next()} #warning("No ground return")
    dummy <- c(3,4,5) %in% lasdata$c
    if (vegflag == "T") {
      if (sum(dummy+0) == 0) {log[p,2] <- 1; next()} #warning("No vegetation return")
    }
    wlen <- max(lasdata$z-min(lasdata$z))
    if (wlen > 60) {log[p,3] <- 1; log[p,4] <- wlen} #warning(paste0("Waveform length = ", wlen, " m"))
    
    #get point density
    ptden[p] <- nrow(lasdata)/(tilesize^2)
    if (ptden[p] < min_ptden) {log[p,6] <- 1; log[p,7] <- ptden[p]}
    
    #get profile and make ground peak = 0 m
    profile <- makeprof(lasdata, binsize)
    gmax0 <- max(lasdata$z[lasdata$c==2], na.rm=T) - min(lasdata$z)
    #profile$counts <- profile$counts/max(profile$counts[profile$height > gmax0]) #normalize lidar profile by maximum canopy return
    genergy <- sum(profile$counts[profile$height <= gmax0])
    tenergy <- sum(profile$counts)
    if (gmax0 < profile$height[1]) {
      log[p,5] <- 1
      peakid <- 1
    } else {
      peakid <- which(profile$counts == max(profile$counts[profile$height <= gmax0])) #find ground peak
      if (length(peakid) > 1) peakid <- peakid[1]
    }
    profile <- profile[peakid:nrow(profile),] #trim profile
    gmax0c <- gmax0 - min(profile$height) #correct max ground height
    profile$height <- profile$height - min(profile$height) #correct heights
        
    #extract metrics
    prof_a <- profile$counts
    prof_h <- profile$height
    
      #Fourier
        nh <- length(prof_h)
        integ <- array(NA, nh)
        for (j in 1:nf) { #loop over frequencies
          for (i in 1:nh) integ[i] <- exp(2 * pi * 1i * freq[j] * prof_h[i]) * prof_a[i] #loop over height bins to calc fourier integrand
          fourier <- sum(integ) / sum(prof_a)
          amp[p,j] <- Mod(fourier) #amplitude
          pha[p,j] <- Arg(fourier) #wrapped phase
        }
      
      #traditional
        trad[p,1] <- sum(prof_h*prof_a)/sum(prof_a) #meanh
        trad[p,2] <-  sqrt(sum(prof_h^2*prof_a)/sum(prof_a) - trad[p,1]^2) #stdevh
        #
        cumprof <- cumsum(prof_a)/sum(prof_a)
        trad[p,3] <- prof_h[which(abs(cumprof-0.25) == min(abs(cumprof-0.25)))[1]] #h25
        trad[p,4] <- prof_h[which(abs(cumprof-0.50) == min(abs(cumprof-0.50)))[1]] #h50
        trad[p,5] <- prof_h[which(abs(cumprof-0.75) == min(abs(cumprof-0.75)))[1]] #h75
        trad[p,6] <- max(prof_h) #mch or h100
        #
        if (length(prof_a) >= 3) {
			peaks <- array(0, length(prof_a))
			for (i in 2:(length(peaks)-1)) if (prof_a[i] > prof_a[i-1] & prof_a[i] > prof_a[i+1]) peaks[i] <- 1 else peaks[i] <- 0
			trad[p,7] <- sum(peaks) #npeaks
        #
			ap1 <- prof_a[seq(1, length(prof_a), 2)]
			ap2 <- prof_a[seq(2, length(prof_a), 2)]
			if (length(ap1) == length(ap2)) ent_a <- ap1 + ap2 else ent_a <- ap1 + c(ap2,0)
			ent_a[ent_a == 0] <- 0.000001
			trad[p,8] <- -sum((ent_a/sum(ent_a)) * log(ent_a/sum(ent_a))) #ent
        #
			dummyid <- which(prof_a == max(prof_a[prof_h > gmax0c]))
			if (length(dummyid) > 1) dummyid <- dummyid[length(dummyid)]
			oc_relief <- max(prof_h) - prof_h[dummyid]
			trad[p,9] <- max(prof_a[prof_h > gmax0c]) / oc_relief #fslope
        #
			trad[p,10] <- genergy/tenergy #g2t
		}
  }

  #unwrap phase
  pha_unw <- pha/NA
  for (pp in 1:nrow(pha)) pha_unw[pp,] <- unwrap(as.numeric(pha[pp,]))
  
  #remove extra freqs
  freq30 <- seq(.01, .3, by=.01)
  freqid <- match(freq30, freq)
  pha_unw <- pha_unw[,freqid]
  amp <- amp[,freqid]
  colnames(pha_unw) <- sub("0.", ".", paste0("pha", format(freq30, digits=2)))
  colnames(amp) <- sub("0.", ".", paste0("amp", format(freq30, digits=2)))
  
#   #extract and check phase heights
#   phaseh_unw <- pha_unw / NA
#   #for (i in 1:30) phaseh_unw[,i] <- pha_unw[,i] / (2*pi*freq30[i]) #line below is better
#   phaseh_unw <- sweep(pha_unw , 2, (2*pi*freq30), `/`)
#   phaseh_max <- array(NA, 30)
#   phaseh_min <- array(NA, 30)
#   phaseh_mean <- array(NA, 30)
#   phaseh_ranges <- array(NA, 30)
#   for (i in 1:30) {
#     phaseh_mean[i] <- mean(phaseh_unw[,i])
#     phaseh_max[i] <- max(phaseh_unw[,i])
#     phaseh_min[i] <- min(phaseh_unw[,i])
#     phaseh_ranges[i] <- max(phaseh_unw[,i]) - min(phaseh_unw[,i])
#   }
#   par(mfrow=c(2,2))
#   plot(freq30, phaseh_max, xlab="Frequency (cyc/m)", ylab="Maximum phase height (m)")
#   plot(freq30, phaseh_mean, xlab="Frequency (cyc/m)", ylab="Mean phase height (m)")
#   plot(freq30, phaseh_min, xlab="Frequency (cyc/m)", ylab="Minimum phase height (m)")
#   plot(freq30, phaseh_ranges, xlab="Frequency (cyc/m)", ylab="Phase height range(m)")


#save results
setwd(indir)
save(lasfiles, xcor, ycor, ptden, trad, amp, pha_unw, log, file="R_project.RData")
write.csv(cbind(lasfiles, xcor, ycor, ptden, trad, amp, pha_unw), file="metrics.csv", row.names=F)
write.csv(cbind(lasfiles, log), file="log.csv", row.names=F)


#check log file
log <- data.frame(log)
if (sum(log$no_ground[log$no_ground==1]) > 0) {
  warning(paste0(length(log$no_ground[log$no_ground==1]), " tiles with no ground return"), immediate.=T)
  cat(paste(lasfiles[log$no_ground==1],"\n"))
}
if (vegflag == "T") {
  if (sum(log$no_veg[log$no_veg==1]) > 0) {
    warning(paste0(length(log$no_veg[log$no_veg==1]), " tiles with no vegetation return:"), immediate.=T)
    cat(paste(lasfiles[log$no_veg==1],"\n"))
  }
}
if (sum(log$wlen_gt60[log$wlen_gt60==1]) > 0) {
  warning(paste0(length(log$wlen_gt60[log$wlen_gt60==1]), " tiles with waveform length > 60 m"), immediate.=T)
  cat(paste(lasfiles[log$wlen_gt60==1],"=>", log$wlen[log$wlen_gt60==1], "m", "\n"), sep="")
}
if (sum(log$ground_lt_binsize[log$ground_lt_binsize==1]) > 0) {
  warning(paste0(length(log$ground_lt_binsize[log$ground_lt_binsize==1]), " tiles with ground height < bin size"), immediate.=T)
  cat(paste(lasfiles[log$ground_lt_binsize==1],"\n"))
}
if (sum(log[,6][log[,6]==1]) > 0) {
  warning(paste0(length(log[,6][log[,6]==1]), " tiles with point density < ", min_ptden), immediate.=T)
  cat(paste(lasfiles[log[,6]==1],"=>", log$ptden[log[,6]==1], "pts/m2", "\n"), sep="")
}



  
  