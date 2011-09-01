
w=.001 #half-width of sampling walk
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
i1length<-4*(integrate(y10prime2,0,20))[[1]]+4*(integrate(y10prime2,0,7.65366))[[1]]

f14=4 #frequency of random walk sine curve
A14=0.25 #amplitude of random walk in km
yp14<-function(x) A14*sin(x*f14*pi/14) + w #lines(x,ym10(x),type="l")
ym14<-function(x) A14*sin(x*f14*pi/14) - w #lines(x,yp10(x),type="l")
y14prime2<-function (x) sqrt(1+(A14*(f14*pi/14)*cos(x*f14*pi/14))^2)
fV=4 #frequency of random walk sine curve
AV=0.25 #amplitude of random walk in km

V=16.25                                        #V=ls$lines[[1]][2,2]-ls$lines[[1]][1,2]
ypV<-function(x) AV*sin(x*fV*pi/V) + w
ymV<-function(x) AV*sin(x*fV*pi/V) - w
yVprime2<-function (x) sqrt(1+(AV*(fV*pi/V)*cos(x*fV*pi/V))^2)

##area unique to each - changes each run
                                        #hcurvearea<-integrate(yp14,0,14)[[1]] - integrate (ym14,0,14)[[1]] ##area between
                                        #vcurvearea<-integrate(ypV,0,V)[[1]] - integrate (ymV,0,V)[[1]] #area
                                        #between 2 curves
                                        #mixarea<-darea+12*hcurvearea+3*vcurvearea
dlength <- 32
##mixlength <-
                                        #12*(integrate(y14prime2,0,14))[[1]]+3*(integrate(yVprime2,0,(ls$lines$end[1,2]-ls$lines$end[2,2])))[[1]]
##changes each time!

