### Simulating ironX and DISTANCE sampling
## rm(list = ls())
## library (spatstat)
### Plotting commands below
bi1n5e5r1.RData
                                        #out<-load("bi1n5e5r1.RData")

out<-bi1n5e5r1
quartz()
par(mfrow=c(2,2))
plot(c(0,65),c(0,65),type="n")
points(out[[8]][,5:6],pch=19) #random sample points
points(out[[10]][,5:6],pch=19) #random sample points
                                        #points(out[[5]][,14:15],pch=20,col="blue")  #centerpoints of segments
                                        #points(out[[3]][,1:2],pch=20,col="green")
                                        #points(out[[3]][,3:4],pch=20,col="green")
matplot(t(out[[3]][,c(1,3)]),t(out[[3]][,c(2,4)]),type='l',add=TRUE,col="red",lty=1)
                                        #associated w/ sample points
plot(c(-11,11),c(-.5,.5),type="n")
points(out[[8]][out[[8]]$segnumber<=4,17:18], pch=19)   #centered and rotated sample points
lines(curve10,yp10(curve10))
lines(curve10,ym10(curve10))
lines(c(-10,10),c(0,0))  #visual reference point for horizontal
plot(c(-7.7/2,7.7/2),c(-.5,.5),type="n")
points(out[[10]][out[[10]]$segnumber>=5,17:18], pch=19)   #centered and rotated sample points
lines(curve8,yp8(curve8))
lines(curve8,ym8(curve8))
lines(c(-7.65/2,7.65/2),c(0,0))  #visual reference point for horizontal
plot(c(-.05,.05),c(-.05,.05),type="n")
points(out[[6]][out[[6]]$segnumber<=4,17:18], pch=19)   #centered and rotated sample points
lines(curve10,yp10(curve10))
lines(curve10,ym10(curve10))
lines(c(-10,10),c(0,0))  #visual reference point for horizontal


integrate(yp10,-10,10)[[1]] - integrate (ym10,-10,10)[[1]] #area between
                                        #2 curves for 10 k segments
integrate(yp8,0,7.65366)[[1]] - integrate (ym8,0,7.65366)[[1]] #area
                                        #between 2 curves 7.6 km

evalx<-insidedf[,14] #xvalues
evaly<-insidedf[,15] #yvalues
curveyp<-yp10 (evalx)
curvepm<-ym10 (evalx)

