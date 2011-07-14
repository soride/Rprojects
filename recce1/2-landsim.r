### Simulating ironX and DISTANCE sampling
## rm(list = ls())
## xpoints<-c(0,sqrt(50), 0,0,-sqrt(50),-10,0, -sqrt(50),  0,0, sqrt(50),10,0) #IronX x-coords
## ypoints<-c(0,sqrt(50),10,0, sqrt(50),  0,0, -sqrt(50),-10,0,-sqrt(50), 0,0) #IronX y-coords

### Plotting commands below
## Random basic
sampledf<-data.frame(random_basic(n=50), fecal_prev=0.1)        #df of random samples
row<-sample(1:nrow(sampledf),5)                                                         #n controls number of ironXs
xvals<-sampledf[row,2]    #x center points of IronXs
yvals<-sampledf[row,3]    #y center points of ironY
ironX<-move_ironXs(ironX0,xvals,yvals)
patterndf<-data.frame(ironX)                                                            #dataframe of recce segments
mind<-psampleDistances(patterndf,sampledf)                                      #measure distances
rb1<-cbind(sampledf, mind) #combine into one df
rb1.list<-list(rb1,patterndf)
                                        #combine into one list



## Cluster M  #strong radius limit!!
## Cluster parameters
k = 15                  #kappa=intensity of Poisson process of cluster centers
r = 0.1                 #r=radius of clusters
mu = 50#400                     #mumean number of points per cluster
fecal_prev=0.1  #prev of positive fecal antibody
sampledf<-data.frame(random_clusterM(k,r,mu,fecal_prev)) #df of random samples
row<-sample(1:nrow(sampledf),5)                                                         #n controls number of ironXs
xvals<-sampledf[row,2] #x value of iron cross center points
yvals<-sampledf[row,3] #y value of iron cross center points
                                        #nxpoints<-xpoints+x                                                                            #for plotting ironX
                                        #nypoints<-ypoints+y                                                                            #for plotting ironX
ironX<-move_ironXs(ironX0,xvals,yvals)
patterndf<-data.frame(ironX)                                                            #dataframe of recce segments
mind<-psampleDistances(patterndf,sampledf)                                      #measure distances
cm1<-cbind(sampledf, mind)
                                        #combine into one df
cm1.list<-list(cm1,patterndf) #combine into one list


## Cluster T
## Cluster parameters
k = 15                  #kappa=intensity of Poisson process of cluster centers
r = 0.05                #r=radius of clusters
mu = 50#4000    #mumean number of points per cluster
fecal_prev=0.1  #prev of positive fecal antibody
nX = 1
sampledf<-data.frame(random_clusterT(k,r,mu,fecal_prev)) #df of random samples
row<-sample(1:nrow(sampledf),5)                                                         #n controls number of ironXs
xvals<-sampledf[row,2]  #x center points of ironXs
yvals<-sampledf[row,3]  #y center points of ironXs
                                        #nxpoints<-xpoints+x
                                        #nypoints<-ypoints+y
ironX<-move_ironXs(ironX0,xvals,yvals)
patterndf<-data.frame(ironX)                                                            #dataframe of recce segments
mind<-psampleDistances(patterndf,sampledf)                                      #measure distances
ct1<-cbind(sampledf, mind)                                                                      #combine into one df
ct1.list<-list(ct1,patterndf)

### Plotting commands
## Confirm point distance calculations are correct
par(mfrow=c(1,3))
plot(0,0,xlim=c(0,200),ylim=c(0,200),type="n",main="random")
points(rb1[,2],rb1[,3],pch=20,col=rb1[,4]+1)
points(rb1.list[[2]][,1:2],col=3) #plot IronX points
points(rb1.list[[2]][,3:4],col=3)
                                        #text(rb1[,2],rb1[,3], round(rb1[,5]), col=rb1[,4]+1)
plot(0,0,xlim=c(0,200),ylim=c(0,200),type="n",main="matern")
points(cm1[,2],cm1[,3],pch=20,col=cm1[,4]+1)
points(cm1.list[[2]][,1:2],col=3)
points(cm1.list[[2]][,3:4],col=3)
                                        #lines(nxpoints,nypoints,col="green") #plot IronX
                                        #text(cm1[,2],cm1[,3], round(cm1[,5]), col=cm1[,4]+1)
plot(0,0,xlim=c(0,200),ylim=c(0,200),type="n",main="thomas")
points(ct1[,2],ct1[,3],pch=20,col=ct1[,4]+1)
points(ct1.list[[2]][,1:2],col=3)
points(ct1.list[[2]][,3:4],col=3)
                                        #lines(nxpoints,nypoints, col="green") #plot IronX
                                        #text(ct1[,2],ct1[,3], round(ct1[,5]), col=ct1[,4]+1)



                                        #       1. get detection functions!!!
                                        #               recces (Alain's file plus
                                        #       2. transects (Stokes is WCS ...
                                        #
                                        #       assumptions don't seem to correspond to parameters necessary to get large n
                                        #       r=0.20 = 40km radius hence diameter of "population"=80km
                                        #       mu=25  average size of population???
                                        #       2-3 feces/day/ape
                                        #       ape density = 2.16/km2
