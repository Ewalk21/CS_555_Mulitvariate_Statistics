#___________________________________HOMEWORK_3____________________________
#Evan Walker
#windows
setwd('D:\\Mathemagic\\Fall2019\\CS555_Multivar_Stats\\Homework_555\\HW_3')
rm(list=ls())


# ____________________Problem_1________________________
#We are interesting in assessing the changes over time in the skulls of ancient Egyptians. 
#The complete dataset (T6-13.dat) is available on Blackboard and description of the four variables is given on page 347 of the textbook. 

#____________________________________________________________________________________________________
# a)Read the data into R and obtain summary statistics of the variables. Comment.
d=read.table('T6-13.dat')
summary(d)
names(d)=c('MB','BH','BL','NH','TP')
summary(d)
dim(d)
boxplot(d[,1:4])
hist(d$MB,breaks=20)
hist(d$BH,breaks=20)
hist(d$BL,breaks=20)
hist(d$NH,breaks=20)
#from visual inspection, the distribution of the four variables seem normal. the boxplot shows that the means of MB nad BH are close, while BL smaller and NH smaller still
#all distributions seem to have similar variance/spread, based on boxplot (over all 3 time periods)

#____________________________________________________________________________________________________
# b)State and verify all distributional assumptions for these data that are necessary to ascertain that the analyses on part c) are correct.
#need multivaraite normal and homoscedasticity
library(MVN)
d1=d[which(d$TP==1),1:4]
d2=d[which(d$TP==2),1:4]
d3=d[which(d$TP==3),1:4]
mvn(d1,mvnTest = "royston")
mvn(d1,mvnTest = "mardia")
mvn(d1,mvnTest = "hz")

mvn(d2,mvnTest = "royston")
mvn(d2,mvnTest = "mardia")
mvn(d2,mvnTest = "hz")

mvn(d3,mvnTest = "royston")
mvn(d3,mvnTest = "mardia")
mvn(d3,mvnTest = "hz")
#All three time periods pass multivariate normality tests
#need now to check box's M-test for homoscedasticity
install.packages('heplots')
library(heplots)
boxM(d[,1:4],d[,"TP"])
#p-value=0.3943 => fail to reject the the Null hypothesis => the covariance matrices of the 4 varaibles across the three time periods are equal.
#assumptions sufficiently met for MANOVA

#____________________________________________________________________________________________________
# c)Test the global multivariate hypothesis that there were no changes in any of the four the skull measurements over the three time periods. 
maov=manova(cbind(MB,BH,BL,NH)~TP,data=d)
summary(maov)
#based on the p-value = 0.01182, we reject the null in favor of the alternative that the 
#means amongs the three variables do indeed differ between time periods

#____________________________________________________________________________________________________
# d)Test the four univariate global hypotheses that there were no changes in the skull measurements over the three time periods. 
summary.aov(maov)  # <--- equivalent to below, all in one swoop
summary(aov(MB~TP,data=d))  #pval= 0.009259  => reject null, mean changes differ significantly
summary(aov(BH~TP,data=d))  #pval= 0.8695   => accept null, mean changes do not differ significantly
summary(aov(BL~TP,data=d))  #pval= 0.01716  => reject null, mean changes differ significantly
summary(aov(NH~TP,data=d))  #pval= 0.9668   =>accept null, mean changes do not differ significantly

#____________________________________________________________________________________________________
# e)Test all necessary pairwise hypotheses to identify the particular time periods and skull measurements that differ.
d1=d[d$TP==1 | d$TP==2,]
d2=d[d$TP==1 | d$TP==3,]
d3=d[d$TP==3 | d$TP==2,]

#differences between time period 1 & 2
summary(aov(MB~TP,data=d1)) #pval= 0.439
summary(aov(BH~TP,data=d1)) #pval= 0.448
summary(aov(BL~TP,data=d1)) #pval= 0.941
summary(aov(NH~TP,data=d1)) #pval= 0.686
#all accept null that the means among the variables do NOT differ between time period 1 & 2

#differences between time period 1 & 3
summary(aov(MB~TP,data=d2)) #pval= 0.00817
summary(aov(BH~TP,data=d2)) #pval= 0.027
summary(aov(BL~TP,data=d2)) #pval= 0.0247
summary(aov(NH~TP,data=d2)) #pval= 0.968
#nasal height mean does NOT change significantly in mean between periods 1 & 3, all others change significantly between 1 & 3

#differences between time period 3 & 2
summary(aov(MB~TP,data=d3)) #pval= 0.0576
summary(aov(BH~TP,data=d3)) #pval= 0.38
summary(aov(BL~TP,data=d3)) #pval= 0.0106
summary(aov(NH~TP,data=d3)) #pval= 0.694
#maximum breadth of the skull and basialveolar length do change significantly in mean between time period 2 & 3
#mean nasal height and basibregmatic height of skull do NOT change significantly between periods 2 & 3

# where Time period
# 1 = 4000 B.C.
# 700 years difference
# 2 = 3300 B.C.
# 1450 years difference
# 3 = 1850 B.C.



# _________________________Problem_2___________________
# We want to see how sensitive to the validity of the assumption of homoscedasticity (equal variances in all populations) ANOVA is.
# We will assess that by simulating many datasets consisting of three samples under the null hypothesis of common mean but using different variances,
# calculating the dataset specific p-values and empirically estimating the type I error rates.

#____________________________________________________________________________________________________
# a)Simulate 10,000 datasets consisting of three groups containing 20, 30 and 50 subjects following 
# normal distributions with a common mean of 20 and variances of 2, 12, and 22 respectively. 
iter = 1:10000
d1=as.data.frame(matrix(rep(0,20),ncol=20,nrow=10000))
d2=as.data.frame(matrix(rep(0,30),ncol=30,nrow=10000))
d3=as.data.frame(matrix(rep(0,50),ncol=50,nrow=10000))
for (i in iter) {
  d1[i,]=rnorm(20, mean = 20, sd = 2)
  d2[i,]=rnorm(30, mean = 20, sd = 12)
  d3[i,]=rnorm(50, mean = 20, sd = 22)
}

#____________________________________________________________________________________________________
# b)Apply ANOVA to each dataset, extract the F-test p-value and store it in a vector PVAL.
g1=rep(1,20)
g2=rep(2,30)
g3=rep(3,50)
PVAL=matrix(ncol=2,nrow=10000)
for (i in iter) {
  vec=cbind(t(d1[i,]),g1)
  vec=rbind(vec,cbind(t(d2[i,]),g2))
  vec=rbind(vec,cbind(t(d3[i,]),g3))
  colnames(vec)= c("c","g")
  anova_vec=aov(c~g,data=as.data.frame(vec))
  pval=summary(anova_vec)[[1]][["Pr(>F)"]]
  Fval=summary(anova_vec)[[1]][["F value"]]
  pval=pval[!is.na(pval)]
  Fval=Fval[!is.na(Fval)]
  PVAL[i,]=c(pval,Fval)
}
colnames(PVAL)=c("pval","Fval")
PVAL=as.data.frame(PVAL)
summary(PVAL)

#____________________________________________________________________________________________________
# c)Calculate the empirical type I error rate (proportion of elements on PVAL less than 0.05). Comment.
len=dim(PVAL)[1]
dims=dim(PVAL[PVAL$pval<.05,])
prop=dims[1]/len
#we know that the data is normal, since we generated it. and we know that the means are equal since we designed it to be so
#hence we expect the pvalue to be large most of the time as to accept the null that the means are equal
#we find that 192 of 10000 had p-val<0.05, which is what we would expect
#the type 1 error rate is .0192