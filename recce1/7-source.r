## The output files (k100,k40,k6,k5...) contain a list with different items
## k100[[1]] = random locations of all poop
## k100[[2]] = summary of random IronX simulations x nruns
## k100[[3]] = spatial maps of random IronX sampling pattern for a simulation (list x number of simulations or nruns)
## k100[[4]] = poop samples and locations collected under a random IronX simulation (list x nruns)
## k100[[5]] = summary of targeted IronX simulations x nruns
## k100[[6]] = spatial maps of targeted IronX sampling pattern for a simulation (list x number of simulations or nruns)
## k100[[7]] = poop samples and locations collected under a targeted IronX simulation (list x nruns)
## k100[[8]] = summary of 16 Distance transect simulations x nruns
## k100[[9]] = spatial maps of 16 Distance transect pattern for a simulation (list x number of simulations or nruns)
## k100[[10]] = poop samples and locations collected under a 16 Distance transect pattern simulation (list x nruns)
## k100[[11]] = summary of Mixed (16 Distance transect plus 12 recces in between) simulations x nruns
## k100[[12]] = spatial maps of Mixed simulation (list x number of simulations or nruns)
## k100[[13]] = poop samples and locations collected under a Mixed pattern simulation (list x nruns)
## k100[[14]] = spatial maps for plotting of Mixed simulation (list x number of simulations or nruns)
## k100[[15]] = summary of all simulations or rbind(k100[[2]],k100[[5]],k100[[8]],k100[[11]])
## columns in item [[15]] 
## 1-run number(1-nruns), 2-number of samples collected, 3-estimated days required to sample pattern, 4-estimated gorilla density (individuals/km2)
## 5 & 6 - x, y, random starts for sampling (center points for IronX and lower left corner for transects)
## 7-true gorilla density (individuals/km2), 8-number of IronXs or transects in landscape, 9-distance walked while sampling, 10-area sampled
## 11-categorical variable (1=random IronX, 2=targeted IronX, 3=Distance, 4=Mixed)

## Density to gorilla feces conversion
area=65*65  ## area of landscape
pilespday=5  ## piles per day per gorilla
piled=1 ##days
poopconversion=area*pilespday*piled


## Set up runs
nruns=15

##Density at 7
n=7*poopconversion

k100<-sims4(nruns,sampledf[1:n,],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,yp14,ym14,y14prime2,i1area,darea,dlength,i1length)
save(k100,file="/Users/solson/Rprojects/recce1/gd7/k100.Rdata")
print(paste("GD",n/poopconversion,"k100","completed",sep=" "))
k40<-sims4(nruns,sampledf40[1:n,],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,yp14,ym14,y14prime2,i1area,darea,dlength,i1length)
save(k40,file="/Users/solson/Rprojects/recce1/gd7/k40.Rdata")
print(paste("GD",n/poopconversion,"k40","completed",sep=" "))
k35<-sims4(nruns,sampledf35[1:n,],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,yp14,ym14,y14prime2,i1area,darea,dlength,i1length)
save(k35,file="/Users/solson/Rprojects/recce1/gd7/k35.Rdata")
print(paste("GD",n/poopconversion,"k35","completed",sep=" "))
k30<-sims4(nruns,sampledf30[1:n,],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,yp14,ym14,y14prime2,i1area,darea,dlength,i1length)
save(k30,file="/Users/solson/Rprojects/recce1/gd7/k30.Rdata")
print(paste("GD",n/poopconversion,"k30","completed",sep=" "))
k25<-sims4(nruns,sampledf25[1:n,],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,yp14,ym14,y14prime2,i1area,darea,dlength,i1length)
save(k25,file="/Users/solson/Rprojects/recce1/gd7/k25.Rdata")
print(paste("GD",n/poopconversion,"k25","completed",sep=" "))
k20<-sims4(nruns,sampledf20[1:n,],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,yp14,ym14,y14prime2,i1area,darea,dlength,i1length)
save(k20,file="/Users/solson/Rprojects/recce1/gd7/k20.Rdata")
print(paste("GD",n/poopconversion,"k20","completed",sep=" "))
k15<-sims4(nruns,sampledf15[1:n,],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,yp14,ym14,y14prime2,i1area,darea,dlength,i1length)
save(k15,file="/Users/solson/Rprojects/recce1/gd7/k15.Rdata")
print(paste("GD",n/poopconversion,"k15","completed",sep=" "))
k10<-sims4(nruns,sampledf10[1:n,],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,yp14,ym14,y14prime2,i1area,darea,dlength,i1length)
save(k10,file="/Users/solson/Rprojects/recce1/gd7/k10.Rdata")
print(paste("GD",n/poopconversion,"k10","completed",sep=" "))
k9<-sims4(nruns,sampledf9[1:n,],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,yp14,ym14,y14prime2,i1area,darea,dlength,i1length)
save(k9,file="/Users/solson/Rprojects/recce1/gd7/k9.Rdata")
print(paste("GD",n/poopconversion,"k9","completed",sep=" "))
k8<-sims4(nruns,sampledf8[1:n,],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,yp14,ym14,y14prime2,i1area,darea,dlength,i1length)
save(k8,file="/Users/solson/Rprojects/recce1/gd7/k8.Rdata")
print(paste("GD",n/poopconversion,"k8","completed",sep=" "))
k7<-sims4(nruns,sampledf7[1:n,],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,yp14,ym14,y14prime2,i1area,darea,dlength,i1length)
save(k7,file="/Users/solson/Rprojects/recce1/gd7/k7.Rdata")
print(paste("GD",n/poopconversion,"k7","completed",sep=" "))
k6<-sims4(nruns,sampledf6[1:n,],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,yp14,ym14,y14prime2,i1area,darea,dlength,i1length)
save(k6,file="/Users/solson/Rprojects/recce1/gd7/k6.Rdata")
print(paste("GD",n/poopconversion,"k6","completed",sep=" "))
k5<-sims4(nruns,sampledf5[1:n,],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,yp14,ym14,y14prime2,i1area,darea,dlength,i1length)
save(k5,file="/Users/solson/Rprojects/recce1/gd7/k5.Rdata")
print(paste("GD",n/poopconversion,"k5","completed",sep=" "))
k4<-sims4(nruns,sampledf4[1:n,],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,yp14,ym14,y14prime2,i1area,darea,dlength,i1length)
save(k4,file="/Users/solson/Rprojects/recce1/gd7/k4.Rdata")
print(paste("GD",n/poopconversion,"k4","completed",sep=" "))
k3<-sims4(nruns,sampledf3[1:n,],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,yp14,ym14,y14prime2,i1area,darea,dlength,i1length)
save(k3,file="/Users/solson/Rprojects/recce1/gd7/k3.Rdata")
print(paste("GD",n/poopconversion,"k3","completed",sep=" "))
k2<-sims4(nruns,sampledf2[1:n,],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,yp14,ym14,y14prime2,i1area,darea,dlength,i1length)
save(k2,file="/Users/solson/Rprojects/recce1/gd7/k2.Rdata")
print(paste("GD",n/poopconversion,"k2","completed",sep=" "))
k1<-sims4(nruns,sampledf1[1:n,],nironXs=1,ntransects=16,ironX0,w=0.001,yp10,ym10,yp8,ym8,yp14,ym14,y14prime2,i1area,darea,dlength,i1length)
save(k1,file="/Users/solson/Rprojects/recce1/gd7/k1.Rdata")
print(paste("GD",n/poopconversion,"k1","completed",sep=" "))


rm(k100,k40,k35,k30,k25,k20,k15,k10,k9,k8,k7,k6,k5,k4,k3,k2,k1)



n=6*poopconversion

