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
source("func-simsIXs.r")
source("func-simsD.r")


w=.001 #.002999 #half-width of sampling walk
f10=3 #frequency of random walk sine curve
A10=0.25 #amplitude of random walk in km
yp10<-function(x) A10*sin(x*f10*pi/10) + w #lines(x,ym10(x),type="l")
ym10<-function(x) A10*sin(x*f10*pi/10) - w #lines(x,yp10(x),type="l")
curve10<-seq(-10,10,by=.01) #sequence for plotting sin curves
f8=2 #frequency of random walk sine curve
A8=0.1 #amplitude of random walk in km
yp8<-function(x) A8*sin(x*f8*pi/3.826834) + w #lines(x,ym8(x),type="l") 7.653669/2
ym8<-function(x) A8*sin(x*f8*pi/3.826834) - w #lines(x,yp8(x),type="l")
curve8<-seq(-7.653669/2,7.653669/2,by=0.01)
i1area<-4*(integrate(yp10,-10,10)[[1]] - integrate (ym10,-10,10)[[1]])+ 4*(integrate(yp8,0,7.65366)[[1]] - integrate (ym8,0,7.65366)[[1]]) #area between
                                        #2 curves for 20 k segments
                                        #and between 2 curves 7.6 km
darea<-2*16*0.002  #area under distance transects


# Set up runs
sampledf3<-read.csv(file="/Users/solson/Rprojects/recce1/simn5e5/sampledf3.csv")

sampledf100_2<-read.csv(file="/Users/solson/Rprojects/recce1/simn3e5/sampledf100_2.csv")
sampledf20_2<-read.csv(file="/Users/solson/Rprojects/recce1/simn3e5/sampledf20_2.csv")
sampledf3_2<-read.csv(file="/Users/solson/Rprojects/recce1/simn3e5/sampledf3_2.csv")

sampledf100_3<-read.csv(file="/Users/solson/Rprojects/recce1/simn1e5/sampledf100_3.csv")
sampledf20_3<-read.csv(file="/Users/solson/Rprojects/recce1/simn1e5/sampledf20_3.csv")
sampledf10_3<-read.csv(file="/Users/solson/Rprojects/recce1/simn1e5/sampledf10_3.csv")
sampledf5_3<-read.csv(file="/Users/solson/Rprojects/recce1/simn1e5/sampledf5_3.csv")
sampledf3_3<-read.csv(file="/Users/solson/Rprojects/recce1/simn1e5/sampledf3_3.csv")

# Breakdown into 3 parts, 5 runs each
nruns=5

#Density at 2.16 (n=5e5)
k3Dp1<-simsD(nruns,sampledf3[,-1],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k3Dp1,file="/Users/solson/Rprojects/recce1/simn5e5/k3Dp1.Rdata")
rm(k3Dp1)
#load(file="/Users/solson/Rprojects/recce1/simn5e5/k3Dp1.Rdata")
print("1 done")
k3Dp2<-simsD(nruns,sampledf3[,-1],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k3Dp2,file="/Users/solson/Rprojects/recce1/simn5e5/k3Dp2.Rdata")
rm(k3Dp2)
#load(file="/Users/solson/Rprojects/recce1/simn5e5/k3Dp2.Rdata")
print("2 done")
k3Dp3<-simsD(nruns,sampledf3[,-1],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k3Dp3,file="/Users/solson/Rprojects/recce1/simn5e5/k3Dp3.Rdata")
rm(k3Dp3)
#load(file="/Users/solson/Rprojects/recce1/simn5e5/k3Dp3.Rdata")
print("3 done")



#Density at 2.16 (n=5e5)
k100Dp1<-simsD(nruns,sampledf100_2[,-1],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k100Dp1,file="/Users/solson/Rprojects/recce1/simn3e5/k100Dp1.Rdata")
rm(k100Dp1)
#load(file="/Users/solson/Rprojects/recce1/simn3e5/k100Dp1.Rdata")
print("4 done")
k100Dp2<-simsD(nruns,sampledf100_2[,-1],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k100Dp2,file="/Users/solson/Rprojects/recce1/simn3e5/k100Dp2.Rdata")
rm(k100Dp2)
#load(file="/Users/solson/Rprojects/recce1/simn3e5/k100Dp2.Rdata")
print("5 done")
k100Dp3<-simsD(nruns,sampledf100_2[,-1],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k100Dp3,file="/Users/solson/Rprojects/recce1/simn3e5/k100Dp3.Rdata")
rm(k100Dp3)
#load(file="/Users/solson/Rprojects/recce1/simn3e5/k100Dp3.Rdata")
print("6 done")

k20Dp1<-simsD(nruns,sampledf20_2[,-1],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k20Dp1,file="/Users/solson/Rprojects/recce1/simn3e5/k20Dp1.Rdata")
rm(k20Dp1)
#load(file="/Users/solson/Rprojects/recce1/simn3e5/k20Dp1.Rdata")
print("7 done")
k20Dp2<-simsD(nruns,sampledf20_2[,-1],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k20Dp2,file="/Users/solson/Rprojects/recce1/simn3e5/k20Dp2.Rdata")
rm(k20Dp2)
#load(file="/Users/solson/Rprojects/recce1/simn3e5/k20Dp2.Rdata")
print("8 done")
k20Dp3<-simsD(nruns,sampledf20_2[,-1],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k20Dp3,file="/Users/solson/Rprojects/recce1/simn3e5/k20Dp3.Rdata")
rm(k20Dp3)
#load(file="/Users/solson/Rprojects/recce1/simn3e5/k20Dp3.Rdata")
print("9 done")

k3Dp1<-simsD(nruns,sampledf3_2[,-1],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k3Dp1,file="/Users/solson/Rprojects/recce1/simn3e5/k3Dp1.Rdata")
rm(k3Dp1)
#load(file="/Users/solson/Rprojects/recce1/simn3e5/k3Dp1.Rdata")
print("10 done")
k3Dp2<-simsD(nruns,sampledf3_2[,-1],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k3Dp2,file="/Users/solson/Rprojects/recce1/simn3e5/k3Dp2.Rdata")
rm(k3Dp2)
#load(file="/Users/solson/Rprojects/recce1/simn3e5/k3Dp2.Rdata")
print("11 done")
k3Dp3<-simsD(nruns,sampledf3_2[,-1],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k3Dp3,file="/Users/solson/Rprojects/recce1/simn3e5/k3Dp3.Rdata")
rm(k3Dp3)
#load(file="/Users/solson/Rprojects/recce1/simn3e5/k3Dp3.Rdata")
print("12 done")



##Density at .54 (n=(549250/4))
k100dp1<-simsD(nruns,sampledf100_3[,-1],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k100dp1,file="/Users/solson/Rprojects/recce1/simn1e5/k100dp1.Rdata")
rm(k100dp1)
#load(file="/Users/solson/Rprojects/recce1/simn1e5/k100dp1.Rdata")
print("13 done")
k100dp2<-simsD(nruns,sampledf100_3[,-1],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k100dp2,file="/Users/solson/Rprojects/recce1/simn1e5/k100dp2.Rdata")
rm(k100dp2)
#load(file="/Users/solson/Rprojects/recce1/simn1e5/k100dp2.Rdata")
print("14 done")
k100dp3<-simsD(nruns,sampledf100_3[,-1],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k100dp3,file="/Users/solson/Rprojects/recce1/simn1e5/k100dp3.Rdata")
rm(k100dp3)
#load(file="/Users/solson/Rprojects/recce1/simn1e5/k100dp3.Rdata")
print("15 done")

k20dp1<-simsD(nruns,sampledf20_3[,-1],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k20dp1,file="/Users/solson/Rprojects/recce1/simn1e5/k20dp1.Rdata")
rm(k20dp1)
#load(file="/Users/solson/Rprojects/recce1/simn1e5/k20dp1.Rdata")
print("16 done")
k20dp2<-simsD(nruns,sampledf20_3[,-1],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k20dp2,file="/Users/solson/Rprojects/recce1/simn1e5/k20dp2.Rdata")
rm(k20dp2)
#load(file="/Users/solson/Rprojects/recce1/simn1e5/k20dp2.Rdata")
print("17 done")
k20dp3<-simsD(nruns,sampledf20_3[,-1],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k20dp3,file="/Users/solson/Rprojects/recce1/simn1e5/k20dp3.Rdata")
rm(k20dp3)
#load(file="/Users/solson/Rprojects/recce1/simn1e5/k20dp3.Rdata")
print("18 done")

k10dp1<-simsD(nruns,sampledf10_3[,-1],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k10dp1,file="/Users/solson/Rprojects/recce1/simn1e5/k10dp1.Rdata")
rm(k10dp1)
#load(file="/Users/solson/Rprojects/recce1/simn1e5/k10dp1.Rdata")
print("19 done")
k10dp2<-simsD(nruns,sampledf10_3[,-1],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k10dp2,file="/Users/solson/Rprojects/recce1/simn1e5/k10dp2.Rdata")
rm(k10dp2)
#load(file="/Users/solson/Rprojects/recce1/simn1e5/k10dp2.Rdata")
print("20 done")
k10dp3<-simsD(nruns,sampledf10_3[,-1],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k10dp3,file="/Users/solson/Rprojects/recce1/simn1e5/k10dp3.Rdata")
rm(k10dp3)
#load(file="/Users/solson/Rprojects/recce1/simn1e5/k10dp3.Rdata")
print("21 done")

k5dp1<-simsD(nruns,sampledf5_3[,-1],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k5dp1,file="/Users/solson/Rprojects/recce1/simn1e5/k5dp1.Rdata")
rm(k5dp1)
#load(file="/Users/solson/Rprojects/recce1/simn1e5/k5dp1.Rdata")
print("22 done")
k5dp2<-simsD(nruns,sampledf5_3[,-1],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k5dp2,file="/Users/solson/Rprojects/recce1/simn1e5/k5dp2.Rdata")
rm(k5dp2)
#load(file="/Users/solson/Rprojects/recce1/simn1e5/k5dp2.Rdata")
print("23 done")
k5dp3<-simsD(nruns,sampledf5_3[,-1],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k5dp3,file="/Users/solson/Rprojects/recce1/simn1e5/k5dp3.Rdata")
rm(k5dp3)
#load(file="/Users/solson/Rprojects/recce1/simn1e5/k5dp3.Rdata")
print("24 done")

k3dp1<-simsD(nruns,sampledf3_3[,-1],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k3dp1,file="/Users/solson/Rprojects/recce1/simn1e5/k3dp1.Rdata")
rm(k3dp1)
#load(file="/Users/solson/Rprojects/recce1/simn1e5/k3dp1.Rdata")
print("25 done")
k3dp2<-simsD(nruns,sampledf3_3[,-1],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k3dp2,file="/Users/solson/Rprojects/recce1/simn1e5/k3dp2.Rdata")
rm(k3dp2)
#load(file="/Users/solson/Rprojects/recce1/simn1e5/k3dp2.Rdata")
print("26 done")
k3dp3<-simsD(nruns,sampledf3_3[,-1],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k3dp3,file="/Users/solson/Rprojects/recce1/simn1e5/k3dp3.Rdata")
rm(k3dp3)
#load(file="/Users/solson/Rprojects/recce1/simn1e5/k3dp3.Rdata")
print("27 done")






k100dp3<-simsD(nruns,sampledf100_3[,-1],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k100dp3,file="/Users/solson/Rprojects/recce1/simn1e5/k100dp3.Rdata")
rm(k100dp3)
#load(file="/Users/solson/Rprojects/recce1/simn1e5/k100dp3.Rdata")
print("15 done")













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


