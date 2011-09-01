### Simulating ironX and DISTANCE sampling
## rm(list = ls())
setwd("/Users/solson/Rprojects/recce1/")
source("func-ironX0.r")
source("func-movereplicateIronX.r")
source("func-samplingstokes.r")
source("func-pointdistanceALL.r")
source("func-rotmove.r")
source("func-distancesim.r")
source("func-simIXRstokes.r")
source("func-simIXTstokes.r")
source("func-sims4.r")
source("func-init.r")  ##initial sin functions for recces

## Density to gorilla feces conversion
area=65*65  ## area of landscape
pilespday=5  ## piles per day per gorilla
piled=1 ##days
poopconversion=area*pilespday*piled

## Set up runs
nruns=15

##Density at 2.16 (n=45630)
n=2.16*poopconversion
##Random or k=100
sampledf <-data.frame(random_basic(n,fecal_prev=0.1)) #number of samples
k100<-sims4(nruns,sampledf,nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,yp14,ym14,y14prime2,ypV,ymV,yVprime2,i1area,darea,dlength,i1length)
save(k100,file="/Users/solson/Rprojects/recce1/simn5e4/k100.Rdata")
print("Woohoo 1")
rm(k100)
                                        #load(file="/Users/solson/Rprojects/recce1/simn5e5/k100.Rdata")

                                        #k=20
                                        #sampledf20<-data.frame(random_clusterT(k=5,r=(20/65),mu=100000,fecal_prev=0.1))[1:n,]
k20<-sims(nruns,sampledf20,nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k20,file="/Users/solson/Rprojects/recce1/simn5e5/k20.Rdata")
print("Woohoo 2")
rm(k20)
                                        #load(file="/Users/solson/Rprojects/recce1/simn5e5/k20.Rdata")

                                        #k=10
sampledf10<-data.frame(random_clusterT(k=5,r=(10/65),mu=100000,fecal_prev=0.1))[1:n,]
k10<-sims(nruns,sampledf10,nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k10,file="/Users/solson/Rprojects/recce1/simn5e5/k10.Rdata")
                                        #rm(k10)
                                        #load(file="/Users/solson/Rprojects/recce1/simn5e5/k10.Rdata")

                                        #k=5
sampledf5<-data.frame(random_clusterT(k=5,r=(5/65),mu=100000,fecal_prev=0.1))[1:n,]
k5<-sims(nruns,sampledf5,nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k5,file="/Users/solson/Rprojects/recce1/simn5e5/k5.Rdata")
rm(k5)
                                        #load(file="/Users/solson/Rprojects/recce1/simn5e5/k5.Rdata")

                                        #k=2.5
sampledf3<-data.frame(random_clusterT(k=5,r=(2.5/65),mu=100000,fecal_prev=0.1))[1:n,]
k3<-sims(nruns,sampledf3,nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k3,file="/Users/solson/Rprojects/recce1/simn5e5/k3.Rdata")
rm(k5)
                                        #load(file="/Users/solson/Rprojects/recce1/simn5e5/k3.Rdata")




quartz()

pdf(file="test.jpg")
plot(c(0,65),c(0,65),type="n")
points(out[[8]][,5:6],pch=19,col="red") #sampled points
points(out[[10]][,5:6],pch=19,col="red") #sampled points
matplot(t(out[[3]][,c(1,3)]),t(out[[3]][,c(2,4)]),type='l',add=TRUE,col="red",lty=1)
## Plotting Distance transects
test<-dr1
plot(test[[4]] $lines,lty=1, add=TRUE,col="blue")
points(test[[10]][,2:3],pch=19,col="blue")
dev.off()


