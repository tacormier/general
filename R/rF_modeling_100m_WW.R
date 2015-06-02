setwd("/mnt/d/temp/wwalker/Xingu/scale/MDDB")
library("randomForest")
library("foreign")

#RMSE function
rmse <- function(x,y) sqrt((sum((x-y)^2)/length(x)))

# Read input files
fuse_2_100 <- read.table(file="Fuse_100.csv", header=TRUE, sep=",", dec=".")

# LS 100
LS100g0 <- subset(fuse_2_100,select=c(glasbio.x,npixels.x,aveb1,ave2,aveb3,aveb4,aveb5,aveb6,aveb7))

LS100rfg0 <- randomForest(glasbio.x ~., data=LS100g0, importance=TRUE)
LS100rfg0

cor.LS100rfg0 <- cor(LS100g0$glasbio, LS100rfg0$pred)
cor.LS100rfg0
rmse.LS100rfg0 <- rmse(LS100g0$glasbio, LS100rfg0$pred)
rmse.LS100rfg0

# ALOS 100
ALOS100g0 <- subset(fuse_2_100,select=c(glasbio.x,npixels.x,aveHH,aveHV,aveHHHV))

ALOS100rfg0 <- randomForest(glasbio.x ~., data=ALOS100g0, importance=TRUE)
ALOS100rfg0

cor.ALOS100rfg0 <- cor(ALOS100g0$glasbio, ALOS100rfg0$pred)
cor.ALOS100rfg0
rmse.ALOS100rfg0 <- rmse(ALOS100g0$glasbio, ALOS100rfg0$pred)
rmse.ALOS100rfg0

# Fuse 100
Fuse100g0 <- subset(fuse_2_100,select=c(glasbio.x,npixels.x,aveb1,ave2,aveb3,aveb4,aveb5,aveb6,aveb7,aveHH,aveHV,aveHHHV))

Fuse100rfg0 <- randomForest(glasbio.x ~., data=Fuse100g0, importance=TRUE)
Fuse100rfg0

cor.Fuse100rfg0 <- cor(Fuse100g0$glasbio, Fuse100rfg0$pred)
cor.Fuse100rfg0
rmse.Fuse100rfg0 <- rmse(Fuse100g0$glasbio, Fuse100rfg0$pred)
rmse.Fuse100rfg0