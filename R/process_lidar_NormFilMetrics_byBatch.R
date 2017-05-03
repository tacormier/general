library(lidR)
library(raster)
################################################

Args <- commandArgs(trailingOnly=TRUE)
print(Args)

# TEST
# rdata.txt <- "/mnt/r/Mex_Lidar/Cartodata/Jalisco/LAS/T1/Tiles_30m_las_RDATA/Jalisco_rdataList_62.txt"

rdata.txt <- Args[1]
# Would usually put this up near the top, but don't want it to print all the functions
# when executing ls.str()
rasterOptions(tmpdir="/home/tcormier/RasTmpDir/")
source("/mnt/a/tcormier/scripts/general/R/handy_functions_TC.R")
####################### NORMALIZATION ####################################   
rdata.files <- scan(rdata.txt, what='character')
# Open the first file so we can get some information about the parameters 
# and do some stuff before starting the loop - like open the DTM.
load(rdata.files[1])

# First, outside of rdata loop, open DTM if normalize=Y
# For tiles, this works bc for each batch of las tiles, the dtm is the same.
# For plots, need to submit one at a time, not in batches.
if (normalize == 'Y') {
  dtm <- raster(dtm.file)
}

if (calc.mets == 'Y' & (met.type == 'prof-based' | met.type == 'both')){
  library(signal)
}

# Loop over rdata files
for (r in (1:length(rdata.files))) {
  #Already opened file #1, so skip that one.
  if (r != 1) {
    load(rdata.files[r])
  }
  
  # For normalization need these variables:
  # las2norm = lasfile; dtm.file; normalize; outnorm; normlog.dir
  # normlog
  
  if (normalize == 'Y') {
    print(paste0("Normalizing ", las2norm.file))
    # Normalize lasfile
    las2norm <- readLAS(las2norm.file)
    
    # let's first snag the mean elevation for the tile/plot. Need to put it here
    # because if file is already normalized, can't get at raw elevation.
    # ***** Did not yet write something at the end to append this to the table*****
    elev <- mean(las2norm@data$Z[las2norm@data$Classification == 2], na.rm=T)
    
    # Use DTM to normalize las
    lasnorm <- lasnormalize(las2norm, dtm)
    writeLAS(lasnorm, outnorm)
      
    if (file.exists(outnorm)) {
      normlog <- "normfile-created"
    } else {
      normlog <- "normfile-failed"
    }
    normlog.df <- cbind.data.frame(las2norm.file, normlog)
    names(normlog.df) <- c("lf", "norm_status")
    normlog.file <- paste0(normlog.dir, stripExtBase(las2norm.file), "_normlog.txt")
    write.table(normlog.df, normlog.file, row.names=F, quote=F)
  } # End normalization section
  
  
  ####################### FILTERING ####################################
  if (QA.flags == 'Y') {
    if (normalize == 'Y') {
      las2qa <- lasnorm
    } else {
      las2qa <- readLAS(las2qa.file)
    }
    print(paste0("Filtering ", las2qa.file))
    
    # For the sake of ease and not trying to have mult jobs simultaneously writing to the same 
    # log files, I'll just write one per plot for now and cat them later. 
    # You know what would fix this? A DATABASE.
    tc.log.file <- paste0(QAlog.dir, stripExtBase(las2qa.file), "_QA_log_TC.csv")
    # unlink(tc.log.file)
    # write.table(paste0("file,", "status"), file=tc.log.file, row.names=F, quote=F, sep=",", col.names=F)
    fg.log.file <- paste0(QAlog.dir, stripExtBase(las2qa.file), "_QA_log_FG.csv")
    
    # Setting up some logs
    tc.log <- as.data.frame(matrix(data=NA, ncol=2, nrow=1))
    names(tc.log) <- c("lf", "QA_status")
    
    # Now Fabio's log
    dummyn1 <- paste0("ptden_lt", min_ptden)
    dummyn2 <- paste0("fgt_gt1")
    dummyn3 <- paste0("fgt")
    dummyn4 <- paste0("flt_minus", abs(minht), "m_gt1")
    dummyn5 <- paste0("flt_minus", abs(minht), "m")
    # dummyn6 <- paste0("htamp_lt_", minhtamp, "m")
    log <- array(0, c(length(las2qa.file), 12))
    # We aren't using all of these, so I should clean this up later - for right now, I just want
    # to get metrics running and for things to write into the correct columns
    colnames(log) <- c("no_ground", "no_veg", dummyn1, "ptden", dummyn2, dummyn3, dummyn4, dummyn5, "dummyn6", "htamp", "nodata", "absmaxht")
    ptden <- array(NA, length(las2qa.file))
    
    tc.log$lf <- las2qa.file
    
    # Some gymnastics with the lasfile to make it work with existing code - hack.
    names.orig <- names(las2qa@data)
    lasdata <- as.data.frame(las2qa@data)
    # changing names to match the rest of our code - hardcoded
    names(lasdata) <- c("x", "y", "z", "gpstime", "i", "r", "n", "c", "a", "pulseID")
    
    #if lasdata is empty, next
    if (nrow(lasdata)==0) {
      tc.log$QA_status <- "failed_QA"
      write.table(tc.log, file=tc.log.file, row.names=F, quote=F, sep=",")
      log <- cbind.data.frame(lf=las2qa.file, log)
      log$nodata <- 1
      write.table(log, file=fg.log.file, row.names=F, quote=F, sep=",")
      print(paste0(tc.log$lf, " las file has no data"))
      next()
    }
    
    #remove rows with no elevation if they exist
    naid <- which(is.na(lasdata$z))
    if (length(naid) > 0) lasdata <- lasdata[-naid,]
    
    #tile quality check
    if (!(2 %in% lasdata$c)) {log[,1] <- 1} #No ground return
    dummy <- c(1,3,4,5) %in% lasdata$c
    if (vegflag == "T") {
      if (sum(dummy+0) == 0) {log[,2] <- 1} #No vegetation return
    }
    
    # *** TALK WITH WAYNE ABOUT EXACTLY HOW AND WHERE TO IMPLEMENT ABSMAXHT FLAG. 
    # Do we do the %-based filtering and if there are values left that are over our absht threshold,
    # we trash the tile? That would mean that more than 2% of the tile is trash, so I think we trash it.
    # That's what I implemented for now.
    maxht <- quantile(lasdata$z, c(0.99))
    fgt_maxht <- length(lasdata$z[lasdata$z > maxht]) / length(lasdata$z) * 100
    if (fgt_maxht > 2) {log[,5] <- 1; log[,6] <- fgt_maxht} #More than 2% of normalized heights > maxht m
    flt_minht <- length(lasdata$z[lasdata$z < minht]) / length(lasdata$z) * 100
    if (flt_minht > 2) {log[,7] <- 1; log[,8] <- flt_minht} #More than 2% of normalized heights < minht m
    
    #get point density
    ptden <- nrow(lasdata)/(lasarea(las2qa))
    if (ptden < min_ptden) {log[,3] <- 1; log[,4] <- ptden}
    
    # Next if there is no ground or vegetation return, or if more than 2% of the normalized heights are outliers
    if (sum(log[, c(1,2,5,7)]) >= 1) {
      tc.log$QA_status <- "failed_QA"
      write.table(tc.log, file=tc.log.file, row.names=F, quote=F, sep=",")
      log <- cbind.data.frame(lf=las2qa.file, log)
      write.table(log, file=fg.log.file, row.names=F, quote=F, sep=",")
      print(paste0(tc.log$lf, " has failed QA."))
      next()
    }
    
    #remove points considered outliers in tiles that passed the 1st quality check
    lasdata <- lasdata[lasdata$z >= minht & lasdata$z <= maxht,]
    
    # If more than 2% of normalized heights are still > our absolute highest height we'll accept, trash the tile.
    if (length(lasdata$z[lasdata$z > absmaxht]) > 0) {log[,12] <- 1} 
    if (sum(log[, 12] == 1)) {
      tc.log$QA_status <- "failed_QA"
      write.table(tc.log, file=tc.log.file, row.names=F, quote=F, sep=",")
      log <- cbind.data.frame(lf=las2qa.file, log)
      write.table(log, file=fg.log.file, row.names=F, quote=F, sep=",")
      print(paste0(tc.log$lf, " has failed QA."))
      next()
    }
    
    #save normalized, filtered LAS
    lasdata$z <- round(lasdata$z, digits=4)
    # write.table(lasdata, file=outtxtnorm, sep=",", row.names=F, col.names=F, quote=F)
    # Convert back to las file and save
    # Put names back first
    names(lasdata) <- names.orig
    df2las <- LAS(lasdata)
    # Create header from original file and then overwrite fields that are updated
    lasheader <- las2qa@header@data
    lasheader$`Number of point records` <- df2las@header@data$`Number of point records`
    lasheader$`Number of 1st return` <- df2las@header@data$`Number of 1st return`
    lasheader$`Number of 2nd return` <- df2las@header@data$`Number of 2nd return`
    lasheader$`Number of 3rd return` <- df2las@header@data$`Number of 3rd return`
    lasheader$`Number of 4th return` <- df2las@header@data$`Number of 4th return`
    lasheader$`Number of 5th return` <- df2las@header@data$`Number of 5th return`
    lasheader$`Min X` <- df2las@header@data$`Min X`
    lasheader$`Min Y` <- df2las@header@data$`Min Y`
    lasheader$`Min Z` <- df2las@header@data$`Min Z`
    lasheader$`Max X` <- df2las@header@data$`Max X`
    lasheader$`Max Y` <- df2las@header@data$`Max Y`
    lasheader$`Max Z` <- df2las@header@data$`Max Z`
    
    df2las@header@data <- lasheader
    writeLAS(df2las, outfil)
    
    if (file.exists(outfil)) {
      tc.log$QA_status <- "filtered_las"
      outfil.txt <- paste0(stripExt(outfil), "_filtered_laslist.txt")
      write.table(outfil, outfil.txt, append=T, row.names=F, col.names = F, quote=F)
    } else {
      tc.log$status <- "writeLAS_failed"
    }
    
    log <- cbind.data.frame(lf=las2qa.file, log)
    
    write.table(tc.log, file=tc.log.file, row.names=F, quote=F, sep=",")
    write.table(log, file=fg.log.file, row.names=F, quote=F, sep=",")
  } # End filtering code
  
  ####################### METRICS ####################################
  if (calc.mets == 'Y') {
    if (QA.flags == 'N' & normalize == 'Y') {
      las2mets.file <- outnorm
      las2mets <- lasnorm
      # if met.type=fusion and the user does not want to calc a profile or save waveforms, no need to open the lasfile at all.
    } else if (QA.flags == 'N' & normalize == 'N' & (saveprof == 'Y' | saveWave == 'Y' | met.type == 'prof-based' | met.type == 'both')) {
      las2mets <- readLAS(las2mets.file)
    } else if (QA.flags == 'Y') {
      las2mets.file <- outfil
      las2mets <- df2las
    }
    
    print(paste0("Calculating metrics for ", las2mets.file))
    
    lasdata <- las2mets@data
    #if lasdata is empty, got to next file
    if (nrow(lasdata)==0) {
      print(paste0(las2mets.file, " has no data."))
      next()
    }
    
    # calculate profile and save if applicable. If any of these conditions
    # are true, we need to calculate a profile.
    if (saveprof == "Y" | saveWave == 'Y' | met.type == 'prof-based' | met.type == 'both') { 
      print(paste0("Computing profile for ", las2mets.file))
      #get profile and find ground peak and top of ground return
      profile <- makeprof(lasdata, binsize, ht.col, int.col) 
      #save profile if required
      if (saveprof == "Y") {
        write.csv(profile, file=profname, quote = F, row.names=F)
      }
    }
    
    if (saveWave == 'Y' | met.type == 'prof-based' | met.type == 'both') {
      # Find ground peak below 25% of max profile height
      #gmax0 <- max(lasdata$z[lasdata$c==2], na.rm=T) - min(lasdata$z) #doesn't work... can't trust ground classification. Leaving it in for legacy.
      gmax0 <- ceiling(0.20*max(profile$height)) # changed to 20% per Fabio's email on 4/10/2017
      # All ground counts beneath gmax0
      gcounts <- profile$counts[1:which(profile$height==gmax0)]
      
      # Identify the ground peak
      gpeaks <- array(0, length(gcounts))
      if (length(gpeaks) >= 3) {
        # Find peaks below gmax0 and ID them with a 1
        for (i in 2:(length(gpeaks)-1)) if (gcounts[i] > gcounts[i-1] & gcounts[i] > gcounts[i+1]) gpeaks[i] <- 1 
      }
      # If length of gpeaks is less than 3, or if no clear ground peaks identified in for loop above, take 1st max:
      if (sum(gpeaks) == 0) {
        peakid <- which(gcounts == max(gcounts))[1]
      } else {
        # Otherwise, identify the FIRST peak below 25% of the max profile height as the ground peak
        peakid <- which(gpeaks==1)[1]
        # In case there is a higher count beneath peakid - if so, change peakid to this position.
        if (gcounts[peakid] <= max(gcounts[1:(peakid-1)])) peakid <- which(gcounts == max(gcounts[1:(peakid-1)]))[1]
      }
      
      # find top of ground return (up to 25% of max profile height)
      cdif <- profile$counts[peakid:which(profile$height==gmax0)] - profile$counts[(peakid+1):(which(profile$height==gmax0)+1)] 
      gtopid <- peakid + which(cdif <= 0)[1] -1
      
      if (is.na(gtopid)) gtopid <- peakid + which(cdif == min(cdif, na.rm=T))[1] -1
      
      #plot full profile in pdf - quick and less info than ggplot version for field plots
      if (saveWave == 'Y') {
        #initiate pdf to save profile figs.
        pdf(file=wavename, width=7, height=7)
        # if (p %in% seq(1,length(lasfiles),4)) par(mfrow=c(2,2), oma=c(2.5,2.5,.5,.5), mar=c(1,1,.5,.5), mgp=c(1.5, .3, 0), tcl=-0.3) #c(bottom, left, top, right)
        plot(profile$counts, profile$height, xlab="Counts", ylab="Height", type="n")
        abline(h=profile$height[gtopid], lty=1, col="gray80")
        abline(h=profile$height[peakid], lty=2, col="red")
        lines(profile$counts, profile$height, col="gray30")
        legend("topright", basename(las2mets.file), bty="n", cex=0.6) 
        
        dev.off()
      }
    } # end profile and waveform generation if applicable
    
    if (met.type == "prof-based" | met.type == "both") {
      #define metrics, etc
      freq <- seq(0.01, 0.3, by=0.001) #maybe use 0.0001 to make sure we have no phase jumps? Depends on processing time. If 0.0001 is used, changes are needed below
      nf <- length(freq)
      amp <- as.data.frame(array(NA, c(1, nf)))
      colnames(amp) <- gsub("p0.", "p.", paste0("amp", format(freq, digits=2)))
      
      pha <- array(NA, c(1, nf))
      colnames(pha) <- gsub("a0.", "a.", paste0("pha", format(freq, digits=2)))
      
      trad <- array(NA, c(1, 10))
      colnames(trad) <- c("meanh", "stdevh", "h25", "h50", "h75", "mch", "npeaks", "ent", "fslope", "g2t")
      
      #get ground and canopy counts before trimming the profile
      #genergy <- sum(profile$counts[profile$height <= gmax0])
      genergy <- sum(profile$counts[profile$height <= profile$height[gtopid]])
      tenergy <- sum(profile$counts)
      
      #make ground peak = 0 m
      gmax0c <- profile$height[gtopid] - min(profile$height[peakid:nrow(profile)]) #corrected max ground height
      profile <- profile[peakid:nrow(profile),] #trim profile
      profile$height <- profile$height - min(profile$height) #correct heights
      
      ##################################
      
      #extract metrics from profile
      prof_a <- profile$counts
      prof_h <- profile$height
      
      #Fourier
      nh <- length(prof_h)
      integ <- array(NA, nh) 
      
      for (j in 1:nf) { #loop over frequencies
        for (i in 1:nh) integ[i] <- exp(2 * pi * 1i * freq[j] * prof_h[i]) * prof_a[i] #loop over height bins to calc fourier integrand
        fourier <- sum(integ) / sum(prof_a)
        amp[1,j] <- Mod(fourier) #amplitude
        pha[1,j] <- Arg(fourier) #wrapped phase
      }
      
      #traditional
      trad[1,1] <- sum(prof_h*prof_a)/sum(prof_a) #meanh
      trad[1,2] <-  sqrt(sum(prof_h^2*prof_a)/sum(prof_a) - trad[1,1]^2) #stdevh
      #
      cumprof <- cumsum(prof_a)/sum(prof_a)
      trad[1,3] <- prof_h[which(abs(cumprof-0.25) == min(abs(cumprof-0.25)))[1]] #h25
      trad[1,4] <- prof_h[which(abs(cumprof-0.50) == min(abs(cumprof-0.50)))[1]] #h50
      trad[1,5] <- prof_h[which(abs(cumprof-0.75) == min(abs(cumprof-0.75)))[1]] #h75
      trad[1,6] <- max(prof_h) #mch or h100
      #
      if (length(prof_a) >= 3) {
        peaks <- array(0, length(prof_a))
        for (i in 2:(length(peaks)-1)) if (prof_a[i] > prof_a[i-1] & prof_a[i] > prof_a[i+1]) peaks[i] <- 1
        trad[1,7] <- sum(peaks) #npeaks
        #
        ap1 <- prof_a[seq(1, length(prof_a), 2)]
        ap2 <- prof_a[seq(2, length(prof_a), 2)]
        if (length(ap1) == length(ap2)) ent_a <- ap1 + ap2 else ent_a <- ap1 + c(ap2,0)
        ent_a[ent_a == 0] <- 0.000001
        trad[1,8] <- -sum((ent_a/sum(ent_a)) * log(ent_a/sum(ent_a))) #ent
        #
        dummyid <- which(prof_a == max(prof_a[prof_h > gmax0c]))
        if (length(dummyid) > 1) dummyid <- dummyid[length(dummyid)]
        oc_relief <- max(prof_h) - prof_h[dummyid]
        trad[1,9] <- max(prof_a[prof_h > gmax0c]) / oc_relief #fslope
        #
        trad[1,10] <- genergy/tenergy #g2t
      }
      
      #unwrap phase
      pha_unw <- as.data.frame(pha/NA)
      for (pp in 1:nrow(pha)) {
        pha_unw[pp,] <- unwrap(as.numeric(pha[pp,]))
      }
      
      #remove extra freqs
      freq30 <- seq(.01, .3, by=.01)
      freqid <- match(freq30, freq)
      pha_unw <- pha_unw[,freqid]
      amp <- amp[,freqid]
      colnames(pha_unw) <- sub("0.", ".", paste0("pha", format(freq30, digits=2)))
      colnames(amp) <- sub("0.", ".", paste0("amp", format(freq30, digits=2)))
      
    }
    
    if (met.type == "fusion" | met.type == "both") { 
      if (met.type == "fusion") {
        f.outmetrics <- out.mets
      } else if (met.type == "both") {
        f.outmetrics <- paste0(tmpdir, unlist(strsplit(basename(las2mets.file), "\\."))[1], "_fusion.csv")
      }
      
      # Reformat lf for windows tool.
      lf.reform <- paste0(tmpdir, unlist(strsplit(basename(las2mets.file), "\\."))[1], "_laslist_wine.txt")
      
      # perform the reformatting and write out new file
      tbl.ref <- gsub("/mnt", "Z:/mnt", las2mets.file)
      tbl.ref <- gsub("/", "\\\\\\\\", tbl.ref)
      # one little hack-fix in case we end up with 8 backslashes due to a double fwd slash (I know...ridiculous)
      tbl.ref <- gsub("\\\\\\\\\\\\\\\\", "\\\\\\\\", tbl.ref)
      
      write.table(tbl.ref, lf.reform, quote = F, row.names = F, col.names = F)
      
      
      lf.reform_txt <- gsub("/mnt", "Z:/mnt", lf.reform)
      lf.reform_txt <- gsub("/", "\\\\\\\\", lf.reform_txt)
      lf.reform_txt <- gsub("\\\\\\\\\\\\\\\\", "\\\\\\\\", lf.reform_txt)
      f.outmetrics.reform <- gsub("/mnt", "Z:/mnt", f.outmetrics)
      f.outmetrics.reform <- gsub("/", "\\\\\\\\", f.outmetrics.reform)
      f.outmetrics.reform <- gsub("\\\\\\\\\\\\\\\\", "\\\\\\\\", f.outmetrics.reform)
      
      # Submit to fusion to calc metrics
      cloudmet.cmd <- paste0("/usr/bin/wine \\\\mnt\\\\a\\\\tcormier\\\\FUSION2\\\\cloudmetrics.exe /minht:1.5 /above:3 /new ", lf.reform_txt, " ", f.outmetrics.reform)
      system(cloudmet.cmd)
      
      # end fusion metrics.
    } 
    
    if (met.type != "fusion" & met.type != "prof-based" & met.type != "both") {
      print(paste0("Invalid met.type option. Possible types are 'prof-based', 'fusion', or 'both.' You entered ", met.type))
      next()
    } # end met.type if/else
    
    # ***** Combine results (if applicable) and save to one table *****
    # if met.type is fusion, the file already saved out as out.mets, but we want to fix the
    # weird field names and re-write it out.
    print(paste0("Finalizing metrics table: ", out.mets))
    if (met.type =='fusion') {
      f.metrics <- read.csv(f.outmetrics)
      names.orig <- names(f.metrics)
      # Fix dumb field names
      name.fix <- gsub("\\.", "_", names.orig)
      name.fix1 <- gsub("X_", "", name.fix)
      name.fix2 <- gsub("_____", "_div_", name.fix1)
      name.fix3 <- gsub("____", "_times_", name.fix2)
      
      names(f.metrics) <- paste0("fusion_", name.fix3)
      
      # Fix file paths:
      f.metrics$fusion_DataFile <- gsub("Z:", "", f.metrics$fusion_DataFile)
      f.metrics$fusion_DataFile <- gsub("\\\\\\\\", "/", f.metrics$fusion_DataFile)
      # Add elevation field
      f.metrics$mean_elevation <- elev
      write.csv(f.metrics, file=out.mets, row.names=F, quote=F)
      
    } else if (met.type == 'prof-based') {
      #save results
      prof.metrics <- cbind.data.frame(las2mets.file, trad, amp, pha_unw)
      names(prof.metrics) <- paste0("prof-based_", names(prof.metrics))
      # Add elevation field
      prof.metrics$mean_elevation <- elev
      write.csv(prof.metrics, file=out.mets, row.names=F, quote=F)
      
    } else if (met.type == 'both') {
      prof.metrics <- cbind.data.frame(las2mets.file, trad, amp, pha_unw)
      names(prof.metrics) <- paste0("prof-based_", names(prof.metrics))
      
      f.metrics <- read.csv(f.outmetrics)
      names.orig <- names(f.metrics)
      # Fix dumb field names
      name.fix <- gsub("\\.", "_", names.orig)
      name.fix1 <- gsub("X_", "", name.fix)
      name.fix2 <- gsub("_____", "_div_", name.fix1)
      name.fix3 <- gsub("____", "_times_", name.fix2)
      
      names(f.metrics) <- paste0("fusion_", name.fix3)
      
      # Fix file paths:
      f.metrics$fusion_DataFile <- gsub("Z:", "", f.metrics$fusion_DataFile)
      f.metrics$fusion_DataFile <- gsub("\\\\\\\\", "/", f.metrics$fusion_DataFile)
      
      # Put tables together in harmony
      mets.final <- cbind.data.frame(prof.metrics, f.metrics)
      # All elevation field
      mets.final$mean_elevation <- elev
      write.csv(mets.final, file=out.mets, row.names=F, quote=F)
    }
    
    # write success/fail log.
    if (file.exists(out.mets)) {
      metlog <- "metrics-run"
    } else {
      metlog <- "metrics-failed"
    }
    metlog.df <- cbind.data.frame(las2mets.file, metlog)
    names(metlog.df) <- c("lf", "norm_status")
    metlog.file <- paste0(metlog.dir, stripExtBase(las2mets.file), "_metlog.txt")
    write.table(metlog.df, metlog.file, row.names=F, quote=F)
      
  }
}

############## Output File Copying to Somewhere - either storage bucket or back to cluster ####################





















