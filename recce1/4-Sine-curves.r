##Plot ironX0
xpoints<-c(0,sqrt(50), 0,0,-sqrt(50),-10,0, -sqrt(50),  0,0, sqrt(50),10,0) #IronX x-coords
ypoints<-c(0,sqrt(50),10,0, sqrt(50),  0,0, -sqrt(50),-10,0,-sqrt(50), 0,0) #IronX y-coords
plot(0,0,xlim=c(-15,15),ylim=c(-15,15),type="n")  #visualization
lines(xpoints,ypoints)

                                        #10 km segments
xvals<-seq(-10,10,by=0.1)
f=6 #frequency of random walk sine curve
A=0.4 #amplitude of random walk in km
x=seq(-10,10,0.001)
yp10<-function(x) A*sin(x*f*pi/10) +.002
ym10<-function(x) A*sin(x*f*pi/10) -.002
lines(x,ym10(x),type="l")
lines(x,yp10(x),type="l")
integrate(yp10,-10,10)[[1]] - integrate (ym10,-10,10)[[1]] #area between
                                        #2 curves

                                        #7.6 km segments
yp8<-function(x) A*sin(x*f*pi/7.653669) +.002
ym8<-function(x) A*sin(x*f*pi/7.653669) -.002
lines(x,ym8(x),type="l")
lines(x,yp8(x),type="l")
integrate(yp8,0,7.65366)[[1]] - integrate (ym8,0,7.65366)[[1]] #area
                                        #between 2 curves


xseed<-sample(seq(0,9,0.001),1)  #set random start for transects
study.area=owin(xrange=c(xseed,xseed+2),yrange=c(0,65)) #one vertical set of transects
xp <-create.lines(study.area,nlines=4,width=.002,spacing=100,angle=180)
label <- c(1:4)
col1<-cbind(label,xp[,2:3]+16,xp[,4:6])
col2<-cbind(label,col1[,2:3]+16,xp[,4:6])
col3<-cbind(label,col2[,2:3]+16,xp[,4:6])
linesdf <- rbind(xp,col1,col2,col3) #4 verticals sets
study.area=owin(xrange=c(0,65),yrange=c(0,65))
ls=lines_to_strips(linesdf,study.area) #converted to strips
plot(0,0,xlim=c(0,65),ylim=c(0,65),type="n")  #visualization
plot(ls$lines,lty=1.5,lwd=2, add=TRUE,col="black")
##Mixed sampling approach
x=seq(0,14,0.001)
w=.001 #.002999 #half-width of sampling walk
f14=4 #frequency of random walk sine curve
A14=0.25 #amplitude of random walk in km
yp14<-function(x) A14*sin(x*f14*pi/14) + w #lines(x,ym10(x),type="l")
ym14<-function(x) A14*sin(x*f14*pi/14) - w #lines(x,yp10(x),type="l")
lines(x+ls$lines[[1]][1,1],ym14(x)+ ls$lines[[1]][1,2],type="l")
lines(x+ls$lines[[1]][1,1],yp14(x)+ ls$lines[[1]][1,2],type="l")
integrate(yp16,0,16)[[1]] - integrate (ym16,0,16)[[1]] ##area between
fV=4 #frequency of random walk sine curve
AV=0.25 #amplitude of random walk in km
V=ls$lines[[1]][2,2]-ls$lines[[1]][1,2]
x=seq(0,V,0.001)
ypV<-function(x) AV*sin(x*fV*pi/16) + w #lines(x,ym8(x),type="l") 7.653669/2
ymV<-function(x) AV*sin(x*fV*pi/16) - w #lines(x,yp8(x),type="l")
lines(rotatepts(x+ls$lines[[1]][1,4],ymV(x)-ls$lines[[1]][1,3],pi/2),type="l")
lines(rotatepts(x+ls$lines[[1]][1,4],ypV(x)-ls$lines[[1]][1,3],pi/2),type="l")
integrate(ypV,0,V)[[1]] - integrate (ymV,0,V)[[1]] #area
                                        #between 2 curves



                                        #http://en.wikipedia.org/wiki/Rotation_(mathematics)
rotatepts<-function (x,y,rad){
    primex<-vector()
    primey<-vector
    for (i in 1:21)
    {
        primex<-x*cos(rad) - y*sin(rad)
        primey<-x*sin(rad) + y*cos(rad)
    }
    return (cbind(primex,primey))
}
pointsR<-rotatepts(xpoints,ypoints,pi/10)

plot(0,0,xlim=c(-15,15),ylim=c(-15,15),type="n")  #visualization
lines(xpoints,ypoints)
lines(pointsR[,c(1:2)])



insidepts<-subset(tests,yrand>ym1(tests[,1]) & yrand<yp1(tests[,1])) #select
                                        #x y points between the 2
                                        #functions

## Visualize sine curves on IronX
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

pdf(file="4-sine-curves.pdf")
plot(0,0,xlim=c(-15,15),ylim=c(-15,15),type="n")  #visualization
lines(xpoints,ypoints,lty=2)
lines(x,ym10(x),type="l",lwd=2)
lines(x,yp10(x),type="l",lwd=2)
lines(rotatepts(x,ym10(x),pi/2),type="l",lwd=2)
lines(rotatepts(x,yp10(x),pi/2),type="l",lwd=2)
lines(rotatepts(x,ym10(x),pi/4),type="l",lwd=2)
lines(rotatepts(x,yp10(x),pi/4),type="l",lwd=2)
lines(rotatepts(x,ym10(x),-pi/4),type="l",lwd=2)
lines(rotatepts(x,yp10(x),-pi/4),type="l",lwd=2)
x7<-seq(0,7.653669,0.001)
lines(rotatepts(x7,ym8(x7),3*pi/8)[,1]-10,rotatepts(x7,ym8(x7),3*pi/8)[,2],lwd=2)
lines(rotatepts(x7,yp8(x7),3*pi/8)[,1]-10,rotatepts(x7,yp8(x7),3*pi/8)[,2],lwd=2)
lines(rotatepts(x7,yp8(x7),11*pi/8)[,1]+10,rotatepts(x7,yp8(x7),11*pi/8)[,2],lwd=2)
lines(rotatepts(x7,yp8(x7),11*pi/8)[,1]+10,rotatepts(x7,yp8(x7),11*pi/8)[,2],lwd=2)
lines(rotatepts(x7,ym8(x7),7*pi/8)[,1],rotatepts(x7,ym8(x7),7*pi/8)[,2]-10,lwd=2)
lines(rotatepts(x7,yp8(x7),7*pi/8)[,1],rotatepts(x7,yp8(x7),7*pi/8)[,2]-10,lwd=2)
lines(rotatepts(x7,ym8(x7),15*pi/8)[,1],rotatepts(x7,ym8(x7),15*pi/8)[,2]+10,lwd=2)
lines(rotatepts(x7,yp8(x7),15*pi/8)[,1],rotatepts(x7,yp8(x7),15*pi/8)[,2]+10,lwd=2)
dev.off()


## 1. Identify points associated with segments 1:4 and 5:8
for segments 1:4
identify center point of ironX #random point x and random point y
center all points on (0,0) (subtract centerX and centerY of ironX)
rotate points that are on segments 1:3
1 will rotate -pi/4
2 will rotate -pi/2
3 will rotate -3*pi/4
4 does not need to rotate
get centered and rotated points= crx cry

for segmens 5:8
identify center point of the segment
center point of segment 5 (subtract ncxs and ncys from segment vertices)
center point of segment 6
center point of segment 7
center point of segment 8
center all points on (0,0) (subtract centerX and centerY of seg)
rotat points that are on segements
5 and 7 will rotate  25* pi/180
6 and 8  will rotate -65* pi/180

##Visualize sampling curves for mixed
pdf(file="4-sine-curves-mixed.pdf")
par=c(lwd=2)
xseed<-sample(seq(0,9,0.001),1)  #set random start for transects
study.area=owin(xrange=c(xseed,xseed+2),yrange=c(0,65)) #one vertical set of transects
xp <-create.lines(study.area,nlines=4,width=.002,spacing=100,angle=180)
label  <- c(1:4)
col1<-cbind(label,xp[,2:3]+16,xp[,4:6])
col2<-cbind(label,col1[,2:3]+16,xp[,4:6])
col3<-cbind(label,col2[,2:3]+16,xp[,4:6])
linesdf <- rbind(xp,col1,col2,col3) #4 verticals sets
study.area=owin(xrange=c(0,65),yrange=c(0,65))
ls=lines_to_strips(linesdf,study.area) #converted to strips
plot(0,0,xlim=c(0,65),ylim=c(0,65),type="n")  #visualization
plot(ls$lines,lty=1.5,lwd=2, add=TRUE,col="black")
##Mixed sampling approach
x=seq(0,14,0.001)
w=.001 #.002999 #half-width of sampling walk
f14=4 #frequency of random walk sine curve
A14=0.25 #amplitude of random walk in km
yp14<-function(x) A14*sin(x*f14*pi/14) + w
ym14<-function(x) A14*sin(x*f14*pi/14) - w
for (i in 1:12)
{
    lines(x+ls$lines[[1]][i,1],ym14(x)+ ls$lines[[1]][i,2],type="l",col="green")
    lines(x+ls$lines[[1]][i,1],yp14(x)+ ls$lines[[1]][i,2],type="l",col="green")
}
fV=4 #frequency of random walk sine curve
AV=0.25 #amplitude of random walk in km
V=ls$lines[[1]][2,2]-ls$lines[[1]][1,2]
x=seq(0,V,0.001)
ypV<-function(x) AV*sin(x*fV*pi/16) + w
ymV<-function(x) AV*sin(x*fV*pi/16) - w
lines(rotatepts(x+ls$lines[[1]][1,4],ymV(x)-ls$lines[[1]][1,3],pi/2),type="l",col="green")
lines(rotatepts(x+ls$lines[[1]][1,4],ypV(x)-ls$lines[[1]][1,3],pi/2),type="l",col="green")
lines(rotatepts(x+ls$lines[[1]][3,4],ymV(x)-ls$lines[[1]][3,3],pi/2),type="l",col="green")
lines(rotatepts(x+ls$lines[[1]][3,4],ypV(x)-ls$lines[[1]][3,3],pi/2),type="l",col="green")
lines(rotatepts(x+ls$lines[[1]][14,2],ymV(x)-ls$lines[[1]][14,1],pi/2),type="l",col="green")
lines(rotatepts(x+ls$lines[[1]][14,2],ypV(x)-ls$lines[[1]][14,1],pi/2),type="l",col="green")
dev.off()

mixedareaH <-12*(integrate(yp14,0,16)[[1]] - integrate
                 (ym14,0,16)[[1]]) ##area between
mixedareaV<-3*(integrate(ypV,0,V)[[1]] - integrate (ymV,0,V)[[1]]) #area
                                        #between 2 curves



##Visualize sampling curves for mixed
pdf(file="7-mixedex.pdf")
par=c(lwd=2)
xseed<-sample(seq(0,9,0.001),1)  #set random start for transects
study.area=owin(xrange=c(xseed,xseed+2),yrange=c(0,65)) #one vertical set of transects
xp <-create.lines(study.area,nlines=4,width=.002,spacing=100,angle=180)
label  <- c(1:4)
col1<-cbind(label,xp[,2:3]+16,xp[,4:6])
col2<-cbind(label,col1[,2:3]+16,xp[,4:6])
col3<-cbind(label,col2[,2:3]+16,xp[,4:6])
linesdf <- rbind(xp,col1,col2,col3) #4 verticals sets
study.area=owin(xrange=c(0,65),yrange=c(0,65))
ls=lines_to_strips(linesdf,study.area) #converted to strips
plot(0,0,xlim=c(0,65),ylim=c(0,65),type="n")  #visualization
plot(ls$lines,lty=1.5,lwd=3,col="darkgreen",add=TRUE)
points(hk100[[1]][,2:3],pch=19,cex=0.005)
##Mixed sampling approach
x=seq(0,14,0.001)
w=.001 #.002999 #half-width of sampling walk
f14=4 #frequency of random walk sine curve
A14=0.25 #amplitude of random walk in km
yp14<-function(x) A14*sin(x*f14*pi/14) + w
ym14<-function(x) A14*sin(x*f14*pi/14) - w
for (i in 1:12)
{
    lines(x+ls$lines[[1]][i,1],ym14(x)+ ls$lines[[1]][i,2],type="l",col="green",lwd=3)
    lines(x+ls$lines[[1]][i,1],yp14(x)+ ls$lines[[1]][i,2],type="l",col="green",lwd=3)
}
fV=4 #frequency of random walk sine curve
AV=0.25 #amplitude of random walk in km
V=ls$lines[[1]][2,2]-ls$lines[[1]][1,2]
x=seq(0,V,0.001)
ypV<-function(x) AV*sin(x*fV*pi/16) + w
ymV<-function(x) AV*sin(x*fV*pi/16) - w
lines(rotatepts(x+ls$lines[[1]][1,4],ymV(x)-ls$lines[[1]][1,3],pi/2),type="l",col="green",lwd=3)
lines(rotatepts(x+ls$lines[[1]][1,4],ypV(x)-ls$lines[[1]][1,3],pi/2),type="l",col="green",lwd=3)
lines(rotatepts(x+ls$lines[[1]][3,4],ymV(x)-ls$lines[[1]][3,3],pi/2),type="l",col="green",lwd=3)
lines(rotatepts(x+ls$lines[[1]][3,4],ypV(x)-ls$lines[[1]][3,3],pi/2),type="l",col="green",lwd=3)
lines(rotatepts(x+ls$lines[[1]][14,2],ymV(x)-ls$lines[[1]][14,1],pi/2),type="l",col="green",lwd=3)
lines(rotatepts(x+ls$lines[[1]][14,2],ypV(x)-ls$lines[[1]][14,1],pi/2),type="l",col="green",lwd=3)
dev.off()

mixedareaH <-12*(integrate(yp14,0,16)[[1]] - integrate
(ym14,0,16)[[1]]) ##area between
mixedareaV<-3*(integrate(ypV,0,V)[[1]] - integrate (ymV,0,V)[[1]]) #area
#between 2 curves




