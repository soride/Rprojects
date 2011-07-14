##Plot ironX0
xpoints<-c(0,sqrt(50), 0,0,-sqrt(50),-10,0, -sqrt(50),  0,0, sqrt(50),10,0) #IronX x-coords
ypoints<-c(0,sqrt(50),10,0, sqrt(50),  0,0, -sqrt(50),-10,0,-sqrt(50), 0,0) #IronX y-coords
plot(0,0,xlim=c(-15,15),ylim=c(-15,15),type="n")  #visualization
lines(xpoints,ypoints)

                                        #10 km segments
xvals<-seq(-10,10,by=0.1)
f=6 #frequency of random walk sine curve
A=0.4 #amplitude of random walk in km
x=1
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




