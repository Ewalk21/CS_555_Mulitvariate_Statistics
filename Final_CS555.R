setwd("D:\\Mathemagic\\Fall2019\\CS555_Multivar_Stats\\Final")
rm(list=ls())
# Problem 4. (40 points) We are interested in finding the important predictors of accumulated wealth at the
# time of retirement, assess their adjusted effect sizes (in direction and magnitude) and use the best linear
# regression model for interpretation and prediction. We want to analyze the Pension.txt dataset
# that contains 194 observations on 17 variables: pyears - years of employment, prftshr -
# indicator for profit sharing company, choice - indicator for company giving a choice to contribute,
# female, married, age, educ - years of education, finc25, finc35, finc50, finc75, finc100, finc101-
# indicators for 25, 35, 50, 75, 100 and 101 levels of retirement contribution, wealth89 - wealth in
# thousands of dollars, race, stckin89 - percent of the portfolio in stock, irain89 - percent of the portfolio in
# IRA. The list below contains steps that might help you with your analysis.


# a) (5 points) Read the data into an appropriate data structure in R. Use the head() function to check
# if the first six rows of data are read properly.
d1 = read.table("Pension.txt",header=T)
summary(d1)
head(d1)

# b) (5 points) Use the dim(), names(), summary(), hist(), boxplot() functions to obtain the dimensions
# of the dataset, the names of all variables, their summary statistics and histograms/barplots.
str(d1)
dims=dim(d1)
labs=names(d1)
summary(d1)

hist(d1$pyears)
boxplot(d1$pyears)
barplot(d1$pyears)

hist(d1$prftshr)
hist(d1$choice)
hist(d1$female)
hist(d1$married)

hist(d1$age)
boxplot(d1$age)

unique(d1$educ)
hist(d1$educ)
boxplot(d1$educ)

hist(d1$finc25)
hist(d1$finc35)
hist(d1$finc50)
hist(d1$finc75)
hist(d1$finc100)
hist(d1$finc101)

barplot(d1$wealth89)
hist(d1$wealth89,breaks=20)
boxplot(d1$wealth89)

hist(d1$race)
hist(d1$stckin89)
hist(d1$irain89)


# c) (5 points) Install and load the R package "car". Use the scatterplotMatrix() function to graphically
# explore the relationship between all pairs of variables. Comment.
library(car)
scatterplotMatrix(d2[,1:10])
#This function provides a convenient interface to the pairs function to 
#produce enhanced scatterplot matrices, including univariate displays 
#on the diagonal and a variety of fitted lines, smoothers, variance 
#functions, and concentration ellipsoids. spm is an abbreviation for 
#scatterplotMatrix.
d2=na.omit(d1)
summary(d2$wealth89)
quantile(d2$wealth89)
d2$wealth89=cut(d2$wealth89,breaks=c(-7,65,140,253,1485),
                            labels=c("verylow","low","high","veryhigh"))
plot(d2$wealth89)
scatterplotMatrix(~pyears+choice+age+educ+stckin89+irain89+race+finc100+married+pyears+female | wealth89, data=d2)
plot(d1)
labs
head(d2)
# d) (5 points) Install the R package "MASS". Use the stepAIC() function to build the best main
# effects model according to the AIC minimization criterion. Discuss R^2 and residual goodness of
# fit diagnostics.
library(MASS)
d3=na.omit(d1)
d3$prftshr=as.factor(d3$prftshr)
d3$choice=as.factor(d3$choice)
d3$female=as.factor(d3$female)
d3$married=as.factor(d3$married)
#d3$educ=as.factor(d3$educ)
d3$finc25=as.factor(d3$finc25)
d3$finc35=as.factor(d3$finc35)
d3$finc50=as.factor(d3$finc50)
d3$finc75=as.factor(d3$finc75)
d3$finc100=as.factor(d3$finc100)
d3$finc101=as.factor(d3$finc101)
d3$race=as.factor(d3$race)
d3$stchin89=as.factor(d3$stckin89)
d3$irain89=as.factor(d3$irain89)

basemod=lm(wealth89~. ,data=d3)
summary(basemod) #adj R2: .2542   mult R^2: .317
stepmod=stepAIC(basemod, direction = "both" ,data = d3)
summary(stepmod) #adj R2: .2668   mult R^2: .2938


# e) (5 points) Use the stepAIC() function to build the best model that includes main effects and 2-
#   way interactions including the interaction of age with itself according to the AIC minimization
# criterion. Discuss R^2 and residual goodness of fit diagnostics.
stepmod2=stepAIC(basemod,~.^2+I(age^2), direction = "both" ,data = d3)
summary(stepmod2) #adj R2: .5884   mult R^2: .6924
anova(stepmod,stepmod2)

# f) (10 points) Based on the results from part d) and e), write a short section discussing the predictors
# of accumulated wealth at the time of retirement and their adjusted effect sizes.
summary(basemod)
summary(stepmod)
summary(stepmod2)

# g) (5 points) What other potentially important variables would you like to be able to add to this
# dataset? What feature engineering techniques with application to which variables would you
# consider implementing in subsequent analyses?




####_____________Problem 6_____________ 6.17
# a)
d3=read.table('T6-8.dat')
#d3=read.table('T6-2.dat')
x_bar=colMeans(d3)
S=var(d3)
C=rbind(c(-1,-1,1,1),
        c(1,-1,1,-1),
        c(1,-1,-1,1))
C
x_bar
S
dims=dim(d3)
n=dims[1]
p=dims[2]

T2=n*t(C%*%x_bar)%*%solve(C%*%S%*%t(C))%*%(C%*%x_bar)
T2
Ft=(((n-1)*(p-1))/(n-p+1))*qf(.95,p-1,n-p+1)
Ft
#T2 = 153.72
#Ft = 9.409
# T > F, => reject H_{0} C*mu = 0

# b)
#95% confidence intervals
#num format effect
cx1=t(C[1,])%*%x_bar
m=((n-1)*(p-1)/(n-p+1))
inter1=sqrt(m*qf(.95,p-1,n-p+1))*sqrt((t(C[1,])%*%S%*%C[1,])/n)
inter1
confinterval1=c(cx1-inter1,cx1+inter1)
confinterval1   #  -415.7364 -198.1074

#Parity effect
cx2=t(C[2,])%*%x_bar
inter2=sqrt(m*qf(.95,p-1,n-p+1))*sqrt((t(C[2,])%*%S%*%C[2,])/n)
inter2
confinterval2=c(cx2-inter2,cx2+inter2)
confinterval2  # 130.4567 282.1995

#Parity type effect
cx3=t(C[3,])%*%x_bar
inter3=sqrt(m*qf(.95,p-1,n-p+1))*sqrt((t(C[3,])%*%S%*%C[3,])/n)
inter3
confinterval3=c(cx3-inter3,cx3+inter3)
confinterval3  # -76.66680  31.82305


#c) 
# for the number format effect, we found that subjects consitently took more time with words than arabic digit.
# for the parity effect, we found that subjects consitently took more time with different word/arabic pairs
# for parity type effect, we found that the effect does not significantly differ from 0. 

#d)
library(MVN)
#num format effect
fe=(d3[,3]+d3[,4])-(d3[,1]+d3[,2])
#Parity effect
pe=(d3[,1]+d3[,3])-(d3[,2]+d3[,4])
#Parity type effect
pt=(d3[,1]+d3[,4])-(d3[,2]+d3[,3])

mean(fe)
mean(pe)
mean(pt)
hist(fe)
hist(pe)
hist(pt)
d_all=cbind(fe,pe,pt)
summary(d_all)
mvn(d_all,mvnTest = "royston")
mvn(d3,mvnTest = "royston")
#both the original data, and the scored-per-subject data is normal.



####_____________Problem 7_____________ 8.18, table 1.5 pg.39
#using sample covariance
d4=read.table('T1-5.dat')
d4
summary(d4)
S=var(d4)
eig=eigen(S)
eig
plot(eig$values,type="l")
varexp=eig$values/sum(eig$values)

varexp[1]           #0.872948
sum(varexp[1:2])    #0.9540751
sum(varexp[1:3])    #0.986968
sum(varexp[1:4])    #0.9942105

newvars=t(eig$vectors) %*% d4


#Using sample correlation
R=cor(d4)

eigr=eigen(R)
eigr
plot(eigr$values,type="l")
varexpr=eigr$values/7

varexpr[1]           #0.3338261
sum(varexpr[1:2])    #0.5318262
sum(varexpr[1:3])    #0.7038356
sum(varexpr[1:4])    #0.8077051

newvars=t(eigr$vectors) %*% d4
#Results differ significantly. I second guessed myself and scaled each feature, then obtained the covariance matrix
#and realized its the same as the correlation matrix, so no error there.
#implementing PCA with to covariance matrix explained the variance in the data much quicker 
#than with the correlation matrix.
#using the covariance matrix, the first 2 principal components explain 95.4% of the variance in the data, 
#and 3 explain 98.7% 



