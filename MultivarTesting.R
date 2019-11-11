data(iris)
d=iris
head(d)
names(d)
d=d[,1:4]
names(d)=c("SL","SW","PL","PW")

#hotelsters T^2 test
n=150
muo=c(4,5,10,2) #mu guess
xbar=colMeans(d)
S=var(d)
T2=n*t(xbar-muo)%*%solve(S)%*%(xbar-muo)
Ft=(n-1)*4/(n-4)*qf(.95,3,n-4)


#if they fail, then must do 4 univariate t-tests at alpha/4


#CR
tq=qt(1-(.05/(2*4)),n-1)
c1=c(xbar[1]-tq*S[1,1]/sqrt(n),xbar[1]+tq*S[1,1]/sqrt(n))
c2=c(xbar[2]-tq*S[2,2]/sqrt(n),xbar[2]+tq*S[2,2]/sqrt(n))
c3=c(xbar[3]-tq*S[3,3]/sqrt(n),xbar[3]+tq*S[3,3]/sqrt(n))
c4=c(xbar[4]-tq*S[4,4]/sqrt(n),xbar[4]+tq*S[4,4]/sqrt(n))
c=c(c1,c2,c3,c4)
c

# PG.276 in book
# this is two of the same sample studied at different labs
# can use paired design
setwd("/home/evan/Downloads")
d=read.table("T6-1.dat")
d
names(d)=c("b1","s1","b2","s2")
boxplot(d)

diff=d[,3:4] - d[,1:2]
dim=dim(diff)
n=dim[1]
p=dim[2]
d_bar=colMeans(diff)
S=var(diff)
T2=n*t(d_bar)%*%solve(S)%*%d_bar
T2

(n-1)*p/(n-p)*qf(.95,p,n-p)

t.test(d$b1,d$b2,paired = TRUE)
t.test(d$s1,d$s2,paired = TRUE)

library(MVN)
mvn(diff,mvnTest = "royston")

#_________________ANOVA TESTING____________
data(iris)
d=iris
head(d)
names(d)
names(d)=c("SL","SW","PL","PW","SPEC")
library(MVN)
summary(aov(d$SW~d$SPEC))
pairwise.t.test(d$SW,d$SPEC,p.adj="bonf")
#^^all p-vals < .05, then all three species have diferent SW

#now by hand, still SW
x_bar=colMeans(d[,1:4])
x_barbar=x_bar[2]
xbar=tapply(d$SW,d$SPEC,mean)
tapply(d$SW,d$SPEC,length)

v1=d$SW-x_bar
#instead of tapply, maybe rep(,c(50,50,50))
v1 = as.vector(rep(xbar-x_barbar,c(50,50,50)))
v2 = as.vector(d$SW-rep(xbar,tapply(d$SW,d$SPEC,length)))

SStr=t(v1)%*%v1
SSres=t(v2)%*%v2
F=((SStr/(3-1))/(SSres/(150-3)))
pval=1-pf(F,3-1,150-3)


##_______________profile testing____________
#Pg.325
x1bar=c(6.8,7,3.9,4.7)
x2bar=c(6.6,7,4,4.5)
n1=30
n2=30
Spool=matrix(c(.606,.262,.066,.161,
               .262,.637,.173,.143,
               .066,.173,.81,.029,
               .161,143,.029,.306),ncol=4)
plot.ts(x1bar)
plot.ts(x2bar)
C=matrix(c(-1,1,0,0,
           0,-1,1,0,
           0,0,-1,1),byrow=T,ncol=4)

#parrallel testing
T1=t(x1bar-x2bar)%*%t(C)%*%solve(C%*%Spool%*%t(C)*((1/n1)+(1/n2)))%*%C%*%(x1bar-x2bar)
T1
p=4
critValue=(n1+n2-2)*(p-1)/(n1+n2-p)*qf(.95,p-1,n1+n2-p)

#Coincidence testing
one=rep(1,4)
T2=t(one)%*%(x1bar-x2bar)/sqrt((1/n1 + 1/n2)*t(one)%*%Spool%*%one)
critval=qt(.975,n1+n2-2)


#_______________MANOVA____________________
library(MVN)
data(iris)
d=iris
head(d)
names(d)
#d=d[,1:4]
names(d)=c("SL","SW","PL","PW","SPEC")
#need to check homogeniety of variances of the 3 species, and MVN
s1=d[which(d$SPEC=="setosa"),1:3]
s2=d[which(d$SPEC=="versicolor"),1:3]
s3=d[which(d$SPEC=="virginica"),1:3]
mvn(s1,mvnTest = "royston")
mvn(s1,mvnTest = "mardia")
mvn(s1,mvnTest = "hz")

mvn(s2,mvnTest = "royston")
mvn(s2,mvnTest = "mardia")
mvn(s2,mvnTest = "hz")

mvn(s3,mvnTest = "royston")
mvn(s3,mvnTest = "mardia")
mvn(s3,mvnTest = "hz")

p=3
g=3
n=rep(50,3)
u=(sum(1/(n-1))-1/sum(n-1))*(2*p^2+3*p-1)/(6(p+1)*(g-1))
S1=var(s1)
S2=var(s2)
S3=var(s3)
Spooled=(S1+S2+S3)/3
D=c(det(S1),det(S2),det(S3))
M=sum(n-1)*log(det(Spooled))-sum((n-1)*log(D))
C=(1-u)*M
cr.value=qchisq(.95,p*(p+1)*(g-1)/2)


#___________________EXTRACREDIT_______________
setwd("D:\\Mathemagic\\Fall2019\\CS555_Multivar_Stats\\ExtraCerdit")
d=read.table('T6-6.dat')
d1=read.table('T6-5.dat')

summary(d)
summary(d1)
head(d)

library(MVN)
mvn(d1,mvnTest = "mardia")
mvn(d,mvnTest = "mardia")

S1=var(d)
S2=var(d1)

x1bar=colMeans(d)
x2bar=colMeans(d1)

g=2
N1=15
N2=16
p=4
q=2

#Spoold1=(1/(N1-g))*((N1-1)*S1+(N2-1)*S2)
Spoold1=(1/(N1+N2-8))*((N1-1)*S1+(N2-1)*S2)
#Spoold=(1/(N2-g))*((N1-1)*S1+(N2-1)*S2)

B=matrix(c(1,1,1,
           1,2,4,
           1,3,9,
           1,4,16),byrow=T,ncol=3)

W1=(N1+N2-8)*S1
W2=(N2+N2-8)*S2
W1



# not sure yet
setwd("D:\\Mathemagic\\Fall2019\\CS555_Multivar_Stats")
d=read.table("lowbwt.dat",header=T)
head(d)
names(d)=tolower(names(d))
summary(d)
dim(d)
d=d[,-c(1,2)]
str(d)
d$race.cat=factor(d$race,levels=c(1,2,3),labels=c("white","african","other"))
head(d)
str(d)
d=d[,-c(3)]
head(d)
library(MASS)
basemod=lm(bwt~.,data=d)
summary(basemod)
stepmod=stepAIC(basemod)
summary(stepmod)
stepmod=stepAIC(basemod,.^2)
summary(stepmod)


#PCA 
setwd("D:\\Mathemagic\\Fall2019\\CS555_Multivar_Stats")
d=read.table('T6-15.dat')
d
sigma=var(d)
eig=eigen(sigma)
plot(eig$values,type="l")
varexp=eig$values/sum(eig$values)
#first princcomp explains 62 % of data.
sum(varexp[1:3])

newvars=t(eig$vectors) %*% d




