### Simulating ironX and DISTANCE sampling
## rm(list = ls())

## load libraries
library(spatstat)
library(DSpat)
library(gplots)
library(epicalc)
library(lattice)
library(Hmisc)

## load functions
setwd("/Users/solson/Rprojects/recce1/")  ##set working directory
source("func-ironX0.r")  ##define IronX sampling
source("func-movereplicateIronX.r")  ##function to move and replicate IronX on a landscape
source("func-samplingstokes.r")  ##random sampling functions parameterized for a 65km x 65 km landscape
source("func-pointdistanceALL.r")  ##calculates distance from points to lines
source("func-rotmove.r") ##same as func-rotatepts - rotates x,y coords around a given radian
source("func-rotatepts.r") ##same as func-rotmove - rotates x,y coords around a given radian
source("func-distancesim.r") ##simulates poop sampling using classic Distance transects
source("func-simIXRstokes.r") ##simulates poop sampling a randomly placed Iron X
source("func-simIXTstokes.r") ##simulates poop sampling a targeted IronX
source("func-mixsim.r") ##simulates a mix of Distance and Recce sampling
source("func-sims4.r") ##wrapper function that pulls everything together
source("func-init.r")  ##initials for simulation inluding sin functions for recces
## execute simulations
source("7-source.r") ##code to run the simulations after loading sampledfs in this document
##output functions
source("7-merge.r") ##code gathers all the simulation outputs for graphing into one dataframe - may still need some editing -
source("7-visualh.r") ##code creates graphics and figures


## Density to gorilla feces conversion
area=65*65  ## area of landscape
pilespday=5  ## piles per day per gorilla
piled=1 ##days
poopconversion=area*pilespday*piled

## Set up runs
nruns=15
ntransects=16

##Upper density at 7 (147875 piles)
n=7*poopconversion

##Random or k=100 down to clusters of 1 km in radius 
## confirm each sample size is greater than the total number of piles needed
## dim(sampledf) > 147875 etc...
sampledf <-data.frame(random_basic(n,fecal_prev=0.1))
sampledf40<-data.frame(random_clusterT(k=5,r=(40/65),mu=50000,fecal_prev=0.1))
sampledf35<-data.frame(random_clusterT(k=5,r=(35/65),mu=50000,fecal_prev=0.1))
sampledf30<-data.frame(random_clusterT(k=5,r=(30/65),mu=50000,fecal_prev=0.1))
sampledf25<-data.frame(random_clusterT(k=5,r=(25/65),mu=50000,fecal_prev=0.1))
sampledf20<-data.frame(random_clusterT(k=5,r=(20/65),mu=50000,fecal_prev=0.1))
sampledf15<-data.frame(random_clusterT(k=5,r=(15/65),mu=50000,fecal_prev=0.1))
sampledf10<-data.frame(random_clusterT(k=5,r=(10/65),mu=50000,fecal_prev=0.1))
sampledf9<-data.frame(random_clusterT(k=5,r=(9/65),mu=50000,fecal_prev=0.1))
sampledf8<-data.frame(random_clusterT(k=5,r=(8/65),mu=50000,fecal_prev=0.1))
sampledf7<-data.frame(random_clusterT(k=5,r=(7/65),mu=50000,fecal_prev=0.1))
sampledf6<-data.frame(random_clusterT(k=5,r=(6/65),mu=50000,fecal_prev=0.1))
sampledf5<-data.frame(random_clusterT(k=5,r=(5/65),mu=50000,fecal_prev=0.1))
sampledf4<-data.frame(random_clusterT(k=5,r=(4/65),mu=50000,fecal_prev=0.1))
sampledf3<-data.frame(random_clusterT(k=5,r=(3/65),mu=50000,fecal_prev=0.1))
sampledf2<-data.frame(random_clusterT(k=5,r=(2/65),mu=50000,fecal_prev=0.1))
sampledf1<-data.frame(random_clusterT(k=5,r=(1/65),mu=50000,fecal_prev=0.1))