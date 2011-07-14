### Simulating ironX and DISTANCE sampling
## rm(list = ls())
setwd("/Users/solson/Dropbox/WCS/1-Working_papers/Recce_vs_distance")
source("func-ironX0.r")
source("func-movereplicateIronX.r")
source("func-pointdistanceALL.r")
source("func-rotmove.r")
source("func-sampling.r")
source("func-simulate.r")

## Set up equations for the random sine curve walk
f=6 #frequency of random walk sine curve
A=0.4 #amplitude of random walk in km
w=.0029999 #half-width of sampling walk
yp10<-function(x) A*sin(x*f*pi/10) + w #lines(x,ym10(x),type="l")
ym10<-function(x) A*sin(x*f*pi/10) - w #lines(x,yp10(x),type="l")
yp8<-function(x) A*sin(x*f*pi/3.826834) + w #lines(x,ym8(x),type="l") 7.653669/2
ym8<-function(x) A*sin(x*f*pi/3.82683) - w #lines(x,yp8(x),type="l")


## Basic randomization
basic<-list()
basic[[1]] <- sampledf <- data.frame(random_basic(n=20),
                                     fecal_prev=0.1)   # number of points
basic[[2]] <- ironX0    #input pattern df
basic[[3]] <- nironXs <- 1 #count of ironXs on the landscape
bi1n20  <- simulate(basic)  #
                                        #save(bi1n20, file = "bin1n20.RData")



## Cluster T  loose radius limit!!
## Cluster parameters44
k = 15                  #kappa=intensity of Poisson process of cluster centers
r = 0.1                 #r=radius of clusters
mu = 4000             #mumean number of points per cluster
fecal_prev=0.1  #prev of positive fecal antibody
tclust<-list()
tclust[[1]] <- sampledf <- data.frame(random_clusterT(k,r,mu,fecal_prev))   # number of points
tclust[[2]] <- ironX0    #input pattern df
tclust[[3]] <- nironXs <- 12 #count of ironXs on the landscape
ti12mu4000 <- simulate(tclust)

save(ti12mu400, file = "ti12mu400.RData")
save(ti12mu4000, file = "ti12mu4000.RData")
save(ti12mu1.7e6, file = "ti12mu1.7e6.RData")




## Cluster M  strong radius limit!!
## Cluster parameters
k = 15                  #kappa=intensity of Poisson process of cluster centers
r = 0.1                 #r=radius of clusters
mu = 40             #mumean number of points per cluster
fecal_prev=0.1  #prev of positive fecal antibody
mclust<-list()
mclust[[1]] <- sampledf <- data.frame(random_clusterM(k,r,mu,fecal_prev))   # number of points
mclust[[2]] <- ironX0    #input pattern df
mclust[[3]] <- nironXs <- 12 #count of ironXs on the landscape
mi12mu40 <- simulate(mclust)

save(mi12mu40,file="mi1mu40.Rdata")
save(mi12mu400,file="mi1mu400.Rdata")
save(mi12mu4000,file="mi1mu4000.Rdata")
