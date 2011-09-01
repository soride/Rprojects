### Simulating ironX and DISTANCE sampling
## rm(list = ls())
setwd("/Users/sarah/Rprojects/recce1/")
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
nruns=15

##Density at 2.16 (n=549250)
n= 549250
#Random or k=100
#sampledf <-data.frame(random_basic(n,fecal_prev=0.1)) #number of samples
#write.csv(file="/Users/sarah/Rprojects/recce1/simn5e5/sampledf100.csv",sampledf)
#k100<-sims(nruns,sampledf,nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
#save(k100,file="/Users/sarah/Rprojects/recce1/simn5e5/k100.Rdata")
#rm(k100)
#load(file="/Users/solson/Rprojects/recce1/simn5e5/k100.Rdata")
#print("k100 done")
#k=20 
#sampledf20<-data.frame(random_clusterT(k=5,r=(20/65),mu=100000,fecal_prev=0.1))[1:n,]
#write.csv(file="/Users/sarah/Rprojects/recce1/simn5e5/sampledf20.csv",sampledf20)
#k20<-sims(nruns,sampledf20,nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
#save(k20,file="/Users/sarah/Rprojects/recce1/simn5e5/k20.Rdata")
#rm(k20)
#load(file="/Users/solson/Rprojects/recce1/simn5e5/k20.Rdata")
#print("k20 done")
#k=10 
#sampledf10<-data.frame(random_clusterT(k=5,r=(10/65),mu=100000,fecal_prev=0.1))[1:n,]
#write.csv(file="/Users/sarah/Rprojects/recce1/simn5e5/sampledf10.csv",sampledf10)
#k10<-sims(nruns,sampledf10,nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
#save(k10,file="/Users/sarah/Rprojects/recce1/simn5e5/k10.Rdata")
#rm(k10)
#load(file="/Users/solson/Rprojects/recce1/simn5e5/k10.Rdata")
#print("k10 done")
#k=5 
#sampledf5<-data.frame(random_clusterT(k=5,r=(5/65),mu=100000,fecal_prev=0.1))[1:n,]
#write.csv(file="/Users/sarah/Rprojects/recce1/simn5e5/sampledf5.csv",sampledf5)
#k5<-sims(nruns,sampledf5,nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
#save(k5,file="/Users/sarah/Rprojects/recce1/simn5e5/k5.Rdata")
#rm(k5)
#load(file="/Users/solson/Rprojects/recce1/simn5e5/k5.Rdata")
#print ("k5 done")
#k=2.5 

#sampledf3<-data.frame(random_clusterT(k=5,r=(2.5/65),mu=100000,fecal_prev=0.1))[1:n,]
#write.csv(file="/Users/sarah/Rprojects/recce1/simn5e5/sampledf3.csv",sampledf3)
k3IXs<-simsIXs(nruns,sampledf3,nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k3IXs,file="/Users/sarah/Rprojects/recce1/simn5e5/k3IXs.Rdata")
rm(k3IXs)
#load(file="/Users/solson/Rprojects/recce1/simn5e5/k3IXs.Rdata")
print("k3IXs done")
#RUN DISTANCE FOR k3D!!!!



##Density at 1.08 (n=(549250/2))
n= (549250/2)
#Random or k=100
#sampledf_2<-sampledf[1:n,] #<-data.frame(random_basic(n,fecal_prev=0.1)) #number of samples
#write.csv(file="/Users/sarah/Rprojects/recce1/simn3e5/sampledf100_2.csv", sampledf_2)
k100IXs<-simsIXs(nruns,sampledf_2,nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k100IXs,file="/Users/sarah/Rprojects/recce1/simn3e5/k100IXs.Rdata")
rm(k100IXs)
#load(file="/Users/solson/Rprojects/recce1/simn3e5/k100IXs.Rdata")
#RUN DISTANCE FOR k100D
print("6 done")
#k=20 
#sampledf20_2 <-sampledf20[1:n,] #<-data.frame(random_clusterT(k=5,r=(20/65),mu=100000,fecal_prev=0.1))[1:n,]
#write.csv(file="/Users/sarah/Rprojects/recce1/simn3e5/sampledf20_2.csv", sampledf20_2)
k20IXs<-simsIXs(nruns,sampledf20_2,nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k20IXs,file="/Users/sarah/Rprojects/recce1/simn3e5/k20IXs.Rdata")
rm(k20IXs)
#load(file="/Users/solson/Rprojects/recce1/simn3e5/k20IXs.Rdata")
#RUN DISTANCE FOR k20D
print("7 done")
#k=10 
#sampledf10_2 <- sampledf10[1:n,] #<-data.frame(random_clusterT(k=5,r=(10/65),mu=100000,fecal_prev=0.1))[1:n,]
#write.csv(file="/Users/sarah/Rprojects/recce1/simn3e5/sampledf10_2.csv", sampledf10_2)
#k10<-sims(nruns,sampledf10_2,nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
#save(k10,file="/Users/sarah/Rprojects/recce1/simn3e5/k10.Rdata")
#rm(k10)
#load(file="/Users/solson/Rprojects/recce1/simn3e5/k10.Rdata")
#print("8 done")
#k=5 
#sampledf5_2<-sampledf5[1:n,]  #<-data.frame(random_clusterT(k=5,r=(5/65),mu=100000,fecal_prev=0.1))[1:n,]
#write.csv(file="/Users/sarah/Rprojects/recce1/simn3e5/sampledf5_2.csv", sampledf5_2)
#k5<-sims(nruns,sampledf5_2,nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
#save(k5,file="/Users/sarah/Rprojects/recce1/simn3e5/k5.Rdata")
#rm(k5)
#load(file="/Users/solson/Rprojects/recce1/simn3e5/k5.Rdata")
#print ("9 done")
#k=2.5 
#sampledf3_2<-sampledf3[1:n,] #<-data.frame(random_clusterT(k=5,r=(2.5/65),mu=100000,fecal_prev=0.1))[1:n,]
#write.csv(file="/Users/sarah/Rprojects/recce1/simn3e5/sampledf3_2.csv", sampledf3_2)
k3<-sims(nruns,sampledf3_2,nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k3,file="/Users/sarah/Rprojects/recce1/simn3e5/k3.Rdata")
rm(k3)
#load(file="/Users/solson/Rprojects/recce1/simn3e5/k3.Rdata")
print("10 done")


##Density at .54 (n=(549250/4))
n= (549250/4)
#Random or k=100
#sampledf_3<-sampledf[1:n,] #<-data.frame(random_basic(n,fecal_prev=0.1)) #number of samples
#write.csv(file="/Users/sarah/Rprojects/recce1/simn1e5/sampledf100_3.csv", sampledf_3)
k100<-sims(nruns,sampledf_3,nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k100,file="/Users/sarah/Rprojects/recce1/simn1e5/k100.Rdata")
rm(k100)
#load(file="/Users/solson/Rprojects/recce1/simn1e5/k100.Rdata")
print("11 done")
#k=20 
#sampledf20_3 <-sampledf20[1:n,] #<-data.frame(random_clusterT(k=5,r=(20/65),mu=100000,fecal_prev=0.1))[1:n,]
#write.csv(file="/Users/sarah/Rprojects/recce1/simn1e5/sampledf20_3.csv", sampledf20_3)
k20<-sims(nruns,sampledf20_3,nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k20,file="/Users/sarah/Rprojects/recce1/simn1e5/k20.Rdata")
rm(k20)
#load(file="/Users/solson/Rprojects/recce1/simn1e5/k20.Rdata")
print("12 done")
#k=10 
#sampledf10_3 <- sampledf10[1:n,] #<-data.frame(random_clusterT(k=5,r=(10/65),mu=100000,fecal_prev=0.1))[1:n,]
#write.csv(file="/Users/sarah/Rprojects/recce1/simn1e5/sampledf10_3.csv", sampledf10_3)
k10<-sims(nruns,sampledf10_3,nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k10,file="/Users/sarah/Rprojects/recce1/simn1e5/k10.Rdata")
rm(k10)
#load(file="/Users/solson/Rprojects/recce1/simn1e5/k10.Rdata")
print("13 done")
#k=5 
#sampledf5_3<-sampledf5[1:n,]  #<-data.frame(random_clusterT(k=5,r=(5/65),mu=100000,fecal_prev=0.1))[1:n,]
#write.csv(file="/Users/sarah/Rprojects/recce1/simn1e5/sampledf5_3.csv", sampledf5_3)
k5<-sims(nruns,sampledf5_3,nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k5,file="/Users/sarah/Rprojects/recce1/simn1e5/k5.Rdata")
rm(k5)
#load(file="/Users/solson/Rprojects/recce1/simn1e5/k5.Rdata")
print ("14 done")
#k=2.5 
#sampledf3_3<-sampledf3[1:n,] #<-data.frame(random_clusterT(k=5,r=(2.5/65),mu=100000,fecal_prev=0.1))[1:n,]
#write.csv(file="/Users/sarah/Rprojects/recce1/simn1e5/sampledf3_3.csv", sampledf3_3)
k3<-sims(nruns,sampledf3_3,nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,i1area,darea)
save(k3,file="/Users/sarah/Rprojects/recce1/simn1e5/k3.Rdata")
rm(k3)
#load(file="/Users/solson/Rprojects/recce1/simn1e5/k3.Rdata")
print("15 done")



