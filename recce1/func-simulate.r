### Simulating ironX and DISTANCE sampling
## rm(list = ls())
## library (spatstat)
### Plotting commands below
## Random basic

## Set up equations for the random sine curve walk
f=6 #frequency of random walk sine curve
A=0.4 #amplitude of random walk in km
w=.0029999 #half-width of sampling walk
yp10<-function(x) A*sin(x*f*pi/10) + w #lines(x,ym10(x),type="l")
ym10<-function(x) A*sin(x*f*pi/10) - w #lines(x,yp10(x),type="l")
yp8<-function(x) A*sin(x*f*pi/3.826834) + w #lines(x,ym8(x),type="l") 7.653669/2
ym8<-function(x) A*sin(x*f*pi/3.82683) - w #lines(x,yp8(x),type="l")

inputs<-list()
inputs[[1]] <- sampledf<-data.frame(random_basic(n=20),
                                    fecal_prev=0.1)   #df of samples
inputs[[2]] <- ironX0
inputs[[3]] <- nironXs <- 1 # number of ironXs

simulate<-function(inputs)
{
    nsegs = 8
    row<-sample(1:nrow(inputs[[1]]),inputs[[3]])   #here n controls number of ironXs
    xvals<-inputs[[1]][row,2]    #x center points of IronXs
    yvals<-inputs[[1]][row,3]    #y center points of ironY
    angles<-c(-pi/4,-pi/2,-3*pi/4,0,25*pi/180,-65*pi/180,25*pi/180,-65*pi/180)
    ##angles of line segments to horizontal
    patterndf<-data.frame(move_ironXs(inputs[[2]],xvals,yvals,nsegs,angles))
    ##dataframe of x1y1 and x2y2 line segments
    distdf<-data.frame(pointdistanceALL(patterndf,inputs[[1]],nsegs=8))  #measure  distances d
    alldf<-merge(distdf,patterndf,by=c("fignumber","segnumber"))
    origindf<-rotmove(alldf)
    insidepts10<-origindf[origindf$segnumber<5 &
                          abs(origindf$x.prime)<10.002999,]
                                        #select rows that have values
                                        #that fall between the curves
                                        #x.prime needs to be less than
                                        #10.0029999

    final10<-subset(insidepts10,insidepts10[,18]>ym10(insidepts10[,17])
                    & insidepts10[,18]<yp10(insidepts10[,17])) #select  x y points
                                        #between the 2 functions
    insidepts8<-origindf[origindf$segnumber>4 &
                         abs(origindf$x.prime)<3.828334 ,]
                                        #x.prime needs to be less than
                                        #7.656669/2
    final8<-subset(insidepts8,insidepts8[,18]>ym8(insidepts8[,17]) &
                   insidepts8[,18]<yp8(insidepts8[,17]))
                                        #select xy points between the 2 functions
    out<-list(xvals,yvals,patterndf,distdf,alldf,origindf,insidepts10,final10,insidepts8,final8)
                                        #out<-list(patterndf,final10,final8)
    out
}

                                        #out[[1]]=xvals
                                        #out[[2]]=yvals
                                        #out[[3]]=patterndf - location of iron cross(es)
                                        #out[[4]]=distdf - distance from each sample point (px,py) to nearest
                                        #iron cross line segment, by fig number and segment number (1-8)
                                        #out[[5]]=alldf - merge of
                                        #patterndf and distdf by fig
                                        #number and segment number
                                        #(nx1,ny1) and (nx2,ny2) and
                                        #(ncxs,ncys) are the vertices
                                        #and center points of
                                        #distributed iron crosses
                                        #out[[6]]=origindf, x.prime
                                        #and y.prime, sample points
                                        #referenced to the origin (0,0),  merged into alldf
                                        #out[[7]]=insidepts10 all px and py points converted to x.prime and y.prime that
                                        #associate with segments 10 km in length
                                        #out[[8]]=just the insidepts10 that fall within the sine curves
                                        #out[[9]]=insidepts8 all px and py points coverted to x.prime and
                                        #y.prime that associate with segments 7.6km in length
                                        #out[[10]]=just the insidepts8 that fall within the sine curves


