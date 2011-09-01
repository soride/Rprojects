### Analyze ironX and DISTANCE sampling
## rm(list = ls())
setwd("/Users/solson/Rprojects/recce1/")
                                        #source("6-simsmerge.r")
load(file="/Users/solson/Rprojects/recce1/out-6.Rdata")

library("DSpat")
library("gplots")
library("epicalc")


all <- read.csv(file="all.csv",all)  ## from 6-simstokesvisualize.r
all2  <- read.csv(file="all2.csv",all2)


## Z-score parameters and set contrasts
zk<-((all2[,11]-mean(all2$k))/sd(all2$k))  ## zscores
zdegord<-((all2[,7]-mean(all2$degord))/sd(all2$degord))
contrasts(all2$type) <- contr.treatment(levels(all2$type), base=3)
##set D to base contrast

## Helpful functions
expci95<-function(coef,se) {
    expcoef<-exp(coef)
    bci<-exp(coef-1.96*se)
    tci<-exp(coef+1.96*se)
    out<-cbind(expcoef,bci,tci)
    out
}


## Sample/km estimation GLM w/ poisson
fit<-glm(samples~type+zk+degord, offset=log(km), data=all2,family=poisson)
summary(fit)

fitout<-rbind(expci95(coef(fit)[1], 0.0321049),expci95(coef(fit)[2], 0.0321049),expci95(coef(fit)[3], 0.0299953),expci95(coef(fit)[4],  0.0002858),expci95(coef(fit)[5], 0.012564))

c(mean(all2$k),sd(all2$k),mean(all2$degord),sd(all2$degord))

##Controling for gorilla density and clustering:
##Compared to D, IXR multiplies the mean samples/km by 11.3 [10.6,12.0] 95%CI
##Compared to D, IXT multiplies the mean samples/km by 23.4
##[22.1,24.8] 95%CI

## Deviance estimation GLM w/ poisson by each sampling type

all3<-cbind(all2,zk,zdegord)
bIXR<-glm(dev~zk+zdegord,data=all3[all3$type=="IXR",])
bIXT <- glm(dev~zk+zdegord,data=all3[all3$type=="IXT",])
bD <- glm(dev~zk+zdegord, data=all3[all3$type=="D",])



