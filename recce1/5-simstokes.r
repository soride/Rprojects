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
source("func-sims.r")

w=.001 #.002999 #half-width of sampling walk
f10=3 #frequency of random walk sine curve
A10=0.25 #amplitude of random walk in km
yp10<-function(x) A10*sin(x*f10*pi/10) + w #lines(x,ym10(x),type="l")
ym10<-function(x) A10*sin(x*f10*pi/10) - w #lines(x,yp10(x),type="l")
y10prime2<-function (x) sqrt(1+(A10*(f10*pi/10)*cos(x*f10*pi/10))^2)
curve10<-seq(-10,10,by=.01) #sequence for plotting sin curves
f8=2 #frequency of random walk sine curve
A8=0.1 #amplitude of random walk in km
yp8<-function(x) A8*sin(x*f8*pi/3.826834) + w #lines(x,ym8(x),type="l") 7.653669/2
ym8<-function(x) A8*sin(x*f8*pi/3.826834) - w
                                        #lines(x,yp8(x),type="l")
y8prime2<-function (x) sqrt(1+(A8*(f8*pi/3.826834)*cos(x*f8*pi/3.826834))^2)
curve8<-seq(-7.653669/2,7.653669/2,by=0.01)
i1area<-4*(integrate(yp10,-10,10)[[1]] - integrate (ym10,-10,10)[[1]])+ 4*(integrate(yp8,0,7.65366)[[1]] - integrate (ym8,0,7.65366)[[1]]) #area between
                                        #2 curves for 20 k segments
                                        #and between 2 curves 7.6 km
darea<-2*16*0.002  #area under distance transects
i1length<-4*(integrate(y10prime2,-10,10))[[1]]+4*(integrate(y10prime2,0,7.65366))[[1]]

## Set up runs
nruns=15

##Density at 2.16 (n=549250)
n=549250
##Random or k=100
sampledf <-data.frame(random_basic(n,fecal_prev=0.1)) #number of samples
k100<-sims(nruns,sampledf,nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k100,file="/Users/solson/Rprojects/recce1/simn5e5/k100.Rdata")
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


