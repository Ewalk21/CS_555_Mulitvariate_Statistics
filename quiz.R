setwd("/home/ewalk21/Desktop/Mathemagic/Fall2019/CS555_Multivar_Stats")
rm(list=ls())
d=read.table("T1-8.DAT")
library(nortest)
library(MVN)
summary(d)
d=d[,1:3]
summary(d)

 #part a
mvn=mvn(d[,1:3], mvnTest=c("mardia"),multivariateOutlierMethod = "adj",showOutliers = TRUE,showNewData = TRUE)
d2=mvn$newData
summary(d)
summary(d2)

#part b
shapiro.test(d$V1)
shapiro.test(d$V2)
shapiro.test(d$V2)
ad.test(d$V1)
ad.test(d$V2)
ad.test(d$V3)
cvm.test(d$V1)
cvm.test(d$V2)
cvm.test(d$V3)
lillie.test(d$V1)
lillie.test(d$V2)
lillie.test(d$V3)

#part c
mvn=mvn(d[,1:3], mvnTest=c("mardia"))
mvn=mvn(d[,1:3], mvnTest=c("hz"))
mvn=mvn(d[,1:3], mvnTest=c("royston"))