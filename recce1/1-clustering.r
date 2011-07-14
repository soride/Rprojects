                                        #rm(list = ls())

xpoints<-c(0,sqrt(50), 0,0,-sqrt(50),-10,0, -sqrt(50),  0,0, sqrt(50),10,0) #IronX x-coords
ypoints<-c(0,sqrt(50),10,0, sqrt(50),  0,0, -sqrt(50),-10,0,-sqrt(50), 0,0) #IronX y-coords


## Create clustered values

clust <- rMatClust(10, 0.05, 50)
                                        # plot(clust)
## rMatClust  Matern Cluster Process
## variation in kappa
quartz()
par(mfrow=c(2,3), mgp=c(0,0,0), pty="m")
for (i in c(1,2,4,8,16,32))
    plot(rMatClust(i, 0.04, 50), main=paste("Matern kappa=",i))

## variation in radius
quartz()
par(mfrow=c(2,3), mgp=c(0,0,0), pty="m")
for (i in c(0.02,0.04,.08,.16,.32,.64))
    plot(rMatClust(20, i, 50), main=paste("Matern radius=",i))

## variation in mu
quartz()
par(mfrow=c(2,3), mgp=c(0,0,0), pty="m")
for (i in c(5,10,20,30,40,50))
    plot(rMatClust(20, 0.04, i), main=paste("Matern mu=",i))
##ape parameters

## variation in kappa
quartz()
par(mfrow=c(2,3), mgp=c(0,0,0), pty="m")
for (i in c(1,2,4,8,16,32))
    plot(rMatClust(i, 0.09, 25), main=paste("Matern kappa=",i))




## rThomas  Thomas Cluster Process
## variation in kappa
quartz()
par(mfrow=c(2,3), mgp=c(0,0,0), pty="m")
for (i in c(1,2,4,8,16,32))
    plot(rThomas(i, 0.04, 50), main=paste("Thomas kappa=",i))

## variation in radius
quartz()
par(mfrow=c(2,3), mgp=c(0,0,0), pty="m")
for (i in c(0.02,0.04,.08,.16,.32,.64))
    plot(rThomas(20, i, 50), main=paste("Thomas radius=",i))

## variation in mu
quartz()
par(mfrow=c(2,3), mgp=c(0,0,0), pty="m")
for (i in c(5,10,20,30,40,50))
    plot(rThomas(20, 0.04, i), main=paste("Thomas mu=",i))









