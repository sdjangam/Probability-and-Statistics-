# free memory
rm(list = ls())
gc()

quantile(iris$Sepal.Length)
quantile(iris$Sepal.Length, c(.1, .3, .65))

var(iris$Sepal.Length)
mean(iris$Sepal.Length)
sd(iris$Sepal.Length)


hist(iris$Sepal.Length)

table(iris$Species)
pie(table(iris$Species))

barplot(table(iris$Species))

cov(iris$Sepal.Length, iris$Petal.Length)
cov(iris[,1:4])
cor(iris$Sepal.Length, iris$Petal.Length)
cor(iris[,1:4])

aggregate(Sepal.Length ~ Species, summary, data=iris)

boxplot(Sepal.Length~Species, data=iris)
boxplot(Sepal.Width~Species, data=iris)

with(iris, plot(Sepal.Length, Sepal.Width, col=Species, pch=as.numeric(Species)))

plot(jitter(iris$Sepal.Length), jitter(iris$Sepal.Width))

pairs(iris)

library(scatterplot3d)
scatterplot3d(iris$Petal.Width, iris$Sepal.Length, iris$Sepal.Width)

library(rgl) 
plot3d(iris$Petal.Width, iris$Sepal.Length, iris$Sepal.Width)

library(ggplot2)
qplot(Sepal.Length, Sepal.Width, data=iris, facets=Species ~.)

x<-rt(300, df = 5)
y <- rt(200, df = 5)
qqplot(x,y)


#Example: Alumni Donations

## Install packages from CRAN; use any USA mirror 
library(lattice)
don <- read.csv("C:/Users/gmanish/Downloads/GreatLearning/v2/contribution.csv")
don[1:5,]
table(don$Class.Year)
barchart(table(don$Class.Year),horizontal=FALSE,xlab="Class Year",col="black")

don$TGiving=don$FY00Giving+don$FY01Giving+don$FY02Giving+don$FY03Giving+don$FY04Giving
mean(don$TGiving)
sd(don$TGiving)
quantile(don$TGiving,probs=seq(0,1,0.05))
quantile(don$TGiving,probs=seq(0.95,1,0.01))
hist(don$TGiving)
hist(don$TGiving[don$TGiving!=0][don$TGiving[don$TGiving!=0]<=1000])

## or, if you want to achieve the above histogram slower in two steps
## ff1=don$TGiving[don$TGiving!=0]
## ff1
## ff2=ff1[ff1<=1000]
## ff2
## hist(ff2,main=paste("Histogram of TGivingTrunc"),xlab="TGivingTrunc")

boxplot(don$TGiving,horizontal=TRUE,xlab="Total Contribution")
boxplot(don$TGiving,outline=FALSE,horizontal=TRUE,xlab="Total Contribution")

ddd=don[don$TGiving>=30000,]
ddd
ddd1=ddd[,c(1:5,12)]
ddd1
ddd1[order(ddd1$TGiving,decreasing=TRUE),]

boxplot(TGiving~Class.Year,data=don,outline=FALSE)
boxplot(TGiving~Gender,data=don,outline=FALSE)
boxplot(TGiving~Marital.Status,data=don,outline=FALSE)
boxplot(TGiving~AttendenceEvent,data=don,outline=FALSE)

t4=tapply(don$TGiving,don$Major,mean,na.rm=TRUE)
t4
t5=table(don$Major)
t5
t6=cbind(t4,t5)
t7=t6[t6[,2]>10,]
t7[order(t7[,1],decreasing=TRUE),]
barchart(t7[,1],col="black")

t4=tapply(don$TGiving,don$Next.Degree,mean,na.rm=TRUE)
t4
t5=table(don$Next.Degree)
t5
t6=cbind(t4,t5)
t7=t6[t6[,2]>10,]
t7[order(t7[,1],decreasing=TRUE),]
barchart(t7[,1],col="black")
densityplot(~TGiving|factor(Class.Year),
data=don[don$TGiving<=1000,][don[don$TGiving<=1000,]$TGiving>0,],
plot.points=FALSE,col="black")

t11=tapply(don$TGiving,don$Class.Year,FUN=sum,na.rm=TRUE)
t11
barplot(t11,ylab="Average Donation")

barchart(tapply(don$FY04Giving,don$Class.Year,FUN=sum,na.rm=TRUE),horizontal=FALSE,ylim=c(0,225000),col="black")
barchart(tapply(don$FY03Giving,don$Class.Year,FUN=sum,na.rm=TRUE),horizontal= FALSE,ylim=c(0,225000),col="black")
barchart(tapply(don$FY02Giving,don$Class.Year,FUN=sum,na.rm=TRUE),horizontal= FALSE,ylim=c(0,225000),col="black")
barchart(tapply(don$FY01Giving,don$Class.Year,FUN=sum,na.rm=TRUE),horizontal= FALSE,ylim=c(0,225000),col="black")
barchart(tapply(don$FY00Giving,don$Class.Year,FUN=sum,na.rm=TRUE),horizontal= FALSE,ylim=c(0,225000),col="black")

don$TGivingIND=cut(don$TGiving,c(-1,0.5,10000000),labels=FALSE)-1
mean(don$TGivingIND)
t5=table(don$TGivingIND,don$Class.Year)
t5 
barplot(t5,beside=TRUE)
mosaicplot(factor(don$Class.Year)~factor(don$TGivingIND))
t50=tapply(don$TGivingIND,don$Class.Year,FUN=mean,na.rm=TRUE)
t50
barchart(t50,horizontal=FALSE,col="black")

don$FY04GivingIND=cut(don$FY04Giving,c(-1,0.5,10000000),labels=FALSE)-1
t51=tapply(don$FY04GivingIND,don$Class.Year,FUN=mean,na.rm=TRUE)
t51
barchart(t51,horizontal=FALSE,col="black")

Data=data.frame(don$FY04Giving,don$FY03Giving,don$FY02Giving,don$FY01Giving,don$FY00Giving)
correlation=cor(Data)
correlation
plot(Data)
library(ellipse)  
plotcorr(correlation)

mosaicplot(factor(don$Gender)~factor(don$TGivingIND))
mosaicplot(factor(don$Marital.Status)~factor(don$TGivingIND))
t2=table(factor(don$Marital.Status),factor(don$TGivingIND))
mosaicplot(t2)
mosaicplot(factor(don$AttendenceEvent)~factor(don$TGivingIND))
t2=table(factor(don$Marital.Status),factor(don$TGivingIND),factor(don$AttendenceEvent))
t2
mosaicplot(t2[,,1])
mosaicplot(t2[,,2])


