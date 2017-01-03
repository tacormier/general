###################################################################################################
# Create matrix plot of correlation matrix
# Circle size represents strength of correlation
# Circle color represents direction of correlation
# From http://gallery.r-enthusiasts.com/graph/Correlation_matrix_circles_152
circle.corr <- function(corr, col=c("black","white"), bg = "white",
                        cex = 1, order = FALSE, title = "", ...){
  
  if (is.null(corr))
    return(invisible())
  if ((!is.matrix(corr)) || (round(min(corr, na.rm = TRUE),
                                   6) < -1) || (round(max(corr, na.rm = TRUE), 6) > 1))
    stop("Need a correlation matrix!")
  n <- nrow(corr)
  m <- ncol(corr)
  
  #   ## reorder the variables using principal component analysis
  #   if (order) {
  #     if(!n==m){
  #       stop("The matrix must be squre if order is TRUE!")
  #     }
  #     x.eigen <- eigen(corr)$vectors[, 1:2]
  #     e1 <- x.eigen[, 1]
  #     e2 <- x.eigen[, 2]
  #     alpha <- ifelse(e1 > 0, atan(e2/e1), atan(e2/e1) + pi)
  #     corr <- corr[order(alpha), order(alpha)]
  #   }
  
  ## set up variable names
  rname <- rownames(corr)
  cname <- colnames(corr)
  if (is.null(rname))
    rname <- 1:n
  if (is.null(cname))
    cname <- 1:m
  rname <- as.character(rname)
  cname <- as.character(cname)
  
  ## calculate label-text width approximately
  par(mar = c(0, 0, 2, 0), bg = "white")
  plot.new()
  plot.window(c(0, m), c(0, n), asp = 1)
  xlabwidth <- max(strwidth(rname, cex = cex))
  ylabwidth <- max(strwidth(cname, cex = cex))
  
  ## set up an empty plot with the appropriate dimensions
  plot.window(c(-xlabwidth + 0.5, m + 0.5), c(0, n + 1 + ylabwidth),
              asp = 1, xlab="", ylab="")
  rect(0.5, 0.5, m + 0.5, n + 0.5, col = bg)  ##background color
  
  ## add variable names and title
  text(rep(-xlabwidth/2, n), n:1, rname, col = "red", cex = cex)
  text(1:m, rep(n + 1 + ylabwidth/2, m), cname, srt = 90, col = "red",
       cex = cex)
  title(title)
  
  ## add grid
  segments(rep(0.5, n + 1), 0.5 + 0:n, rep(m + 0.5, n + 1),
           0.5 + 0:n, col = "gray")
  segments(0.5 + 0:m, rep(0.5, m + 1), 0.5 + 0:m, rep(n + 0.5,
                                                      m), col = "gray")
  
  ## assign circles' fill color
  nc <- length(col)
  if(nc==1)
    bg <- rep(col, n*m)
  else{
    ff <- seq(-1,1, length=nc+1)
    bg2 = rep(0, n * m)
    for (i in 1:(n * m)){
      bg2[i] <- rank(c(ff[2:nc], as.vector(corr)[i]),
                     ties.method = "random")[nc]
    }
    bg <- (col[nc:1])[bg2]
  }
  
  ## plot n*m circles using vector language, suggested by Yihui Xie
  ## the area of circles denotes the absolute value of coefficient
  symbols(rep(1:m, each = n), rep(n:1, m), add = TRUE, inches = F,
          circles = as.vector(sqrt(abs(corr))/2), bg = as.vector(bg))
  
  # Create binary vector. 1 where corr stat is greater than threshold, 0 where not
  zz <- as.vector(ifelse(corr >= 0.75 | corr <= -0.75, 1, 0))
  
  points(rep(1:m, each = n)[zz==1], rep(n:1, m)[zz==1], pch=16, col="yellow")
}
#################################
panel.cor <- function(x, y, digits=2, cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  test <- cor.test(x,y)
  Signif <- ifelse(round(test$p.value,3)<0.001,"p<0.001",paste("p=",round(test$p.value,3)))  
  text(0.5, 0.25, paste("r=",txt))
  text(.5, .75, Signif)
}

panel.smooth<-function (x, y, col = "plum4", bg = NA, pch = 18, 
                        cex = 0.8, col.smooth = "red", span = 2/3, iter = 3, ...) 
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
          col = col.smooth, ...)
}

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE, breaks=30)
  breaks <- h$breaks; nB <- length(breaks)
  # breaks <- 30
  # nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="darkseagreen4", border="darkseagreen4", ...)
}

colorRange <- c('#69091e', '#e37f65', 'gray', '#aed2e6', '#042f60')
## colorRamp() returns a function which takes as an argument a number
## on [0,1] and returns a color in the gradient in colorRange
myColorRampFunc <- colorRamp(colorRange)

panel.circ <- function(w, z, ...) {
  correlation <- cor(w, z)
  
  ## because the func needs [0,1] and cor gives [-1,1], we need to
  ## shift and scale it
  col <- rgb( myColorRampFunc( (1+correlation)/2 )/255 )
  
  ## square it to avoid visual bias due to "area vs diameter"
  radius <- sqrt(abs(correlation))
  radians <- seq(0, 2*pi, len=50)     # 50 is arbitrary
  x <- radius * cos(radians)
  y <- radius * sin(radians)
  ## make them full loops
  x <- c(x, tail(x,n=1))
  y <- c(y, tail(y,n=1))
  
  ## I trick the "don't create a new plot" thing by following the
  ## advice here: http://www.r-bloggers.com/multiple-y-axis-in-a-r-plot/
  ## This allows
  par(new=TRUE)
  plot(0, type='n', xlim=c(-1,1), ylim=c(-1,1), axes=FALSE, asp=1)
  polygon(x, y, border=col, col=col)
}


# mets.all <- data4[,c(6,11,13,14,15,19,21,22,23,24)]
mets.all <- data4[,c(3,11,13,14,15,17,21,23,24)]
# w/ upper panel as circle plot
pairs(mets.all, cex.labels=1, font.labels=2, upper.panel=panel.circ, lower.panel=panel.smooth, diag.panel=panel.hist)


# w/ upper panel as R and P values.
pairs(mets.all, cex.labels=1, font.labels=2, lower.panel=panel.smooth, upper.panel=panel.cor,diag.panel=panel.hist)
# mets.cor <- cor(mets.all)















