setwd("/home/ewalk21/Desktop/Mathemagic/Fall2019/CS555_Multivar_Stats/Homework_555/HW_2")
rm(list=ls())
d=read.delim("T4-6.DAT")
library(nortest)
library(MVN)
summary(d)

qqnorm(d$X27)
qqnorm(d$X13)
qqnorm(d$X14)
qqnorm(d$X20)
qqnorm(d$X11)
#qqnorm(d$X2) binary var
#qqnorm(d$X1) binary var

hist(d$X27)
hist(d$X13)
hist(d$X14)
hist(d$X20)
hist(d$X11)


mvn(d[,1:5], mvnTest=c("mardia"))
mvn(d[,1:5], mvnTest=c("royston"))
mvn(d[,1:5], mvnTest=c("hz"))

mvn(d[,3:4], mvnTest=c("mardia"))
mvn(d[,3:4], mvnTest=c("royston"))
mvn(d[,3:4], mvnTest=c("hz"))

boxplot(d$X27)
boxplot(d$X13)
boxplot(d$X14)
boxplot(d$X20)
boxplot(d$X11)

z27=scale(d$X27)
z13=scale(d$X13)
z14=scale(d$X14)
z20=scale(d$X20)
z11=scale(d$X11)

z27_rm=z27[abs(z27)<3]
z13_rm=z13[abs(z13)<3]
z14_rm=z14[abs(z14)<3]
z20_rm=z20[abs(z20)<3]
z11_rm=z11[abs(z11)<3]

length(z27_rm)
length(z27)
length(z13_rm)
length(z13)
length(z14_rm)
length(z14)
length(z20_rm)
length(z20)
length(z11_rm)
length(z11)
#z14 and z11 both have 1 outlier.
