### Simulating ironX and DISTANCE sampling
## rm(list = ls())
### Plotting commands below
## Simple set up
xvals<-c(-5,15)
yvals<-c(-5,15)
nsegs<-8
angles<-c(-pi/4,-pi/2,-3*pi/4,0,25*pi/180,-65*pi/180,25*pi/180,-65*pi/180) ##angles of line segments to horizontal
patterndf<-data.frame(move_ironXs(ironX0,xvals,yvals,nsegs,angles)) ##dataframe of x1y1 and x2y2 line segments
sampledf<-data.frame(matrix(c(NA,8.5,3.5,1,.1,NA,3.5,-8.5,1,.1,NA,-8.5,-3.5,0,.1,NA,-3.5,8.5,1,.1),ncol=5,byrow=T))
distdf<-data.frame(pointdistanceALL(patterndf,sampledf,nsegs=8))  #measure distances d
alldf<-merge(distdf,patterndf,by=c("fignumber","segnumber"))
origindf<-rotmove(alldf)
insidepts10<-origindf[origindf$segnumber<5 &
                      abs(origindf$x.prime)<10.002999,]
                                        #select rows that have values that fall
                                        #between the 10km recce curves
                                        #x.prime needs to be less than
                                        #10.0029999
f=6 #frequency of random walk sine curve
A=0.4 #amplitude of random walk in km
w=.0029999 #half-width of sampling walk
yp10<-function(x) A*sin(x*f*pi/10) + w #lines(x,ym10(x),type="l")
ym10<-function(x) A*sin(x*f*pi/10) - w #lines(x,yp10(x),type="l")
yp10.x.prime<-yp10(insidepts10$x.prime)
ym10.x.prime<-ym10(insidepts10$x.prime)
final10<-subset(insidepts10,insidepts10[,18]>ym10(insidepts10[,17]) & insidepts10[,18]<yp10(insidepts10[,17])) #select
                                        #x y points between the 2
                                        #functions

insidepts8<-origindf[origindf$segnumber>4 &
                     abs(origindf$x.prime)<7.656669/2 ,]
                                        #x.prime needs to be less than 7.656669/2
yp8<-function(x) A*sin(x*f*pi/3.826834) + w #lines(x,ym8(x),type="l")
                                        #7.653669/2
ym8<-function(x) A*sin(x*f*pi/3.82683) - w #lines(x,yp8(x),type="l")
yp8.x.prime<-yp10(insidepts8$x.prime)
ym8.x.prime<-ym10(insidepts8$x.prime)
final8<-subset(insidepts8,insidepts8[,18]>ym8(insidepts8[,17]) & insidepts8[,18]<yp8(insidepts8[,17])) #select

xcurve<-seq(-10,10,by=.1) #sequene for plotting sin curves
plus10<-function(x) x+10  #functions to move ironX xpoints and ypoints
minus5<-function(x) x-5
plus15<-function(x) x+15

par(mfrow=c(1,2))
plot(c(-20,30),c(-20,30),type="n")
xpoints<-c(0,sqrt(50), 0,0,-sqrt(50),-10,0, -sqrt(50),  0,0,
           sqrt(50),10,0) #IronX x-coords
ypoints<-c(0,sqrt(50),10,0, sqrt(50),  0,0, -sqrt(50),-10,0,-sqrt(50), 0,0)
lines(plus15(xpoints),plus15(ypoints))
lines(minus5(xpoints),minus5(ypoints))
points(origindf[,5:6],col=c("red","purple","green","brown")) #random
                                        #sample points
points(origindf[,14:15],pch=20,col="blue")  #centerpoints of segments
                                        #associated w/ sample points
plot(c(-20,30),c(-20,30),type="n")
points(origindf[,17:18], col=c("red","purple","green","brown"))
                                        #centered and rotated sample points
lines(c(-10,10),c(0,0))  #visual reference point for horizontal




