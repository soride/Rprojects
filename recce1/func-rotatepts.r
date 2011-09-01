
##http://en.wikipedia.org/wiki/Rotation_(mathematics)
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
#pointsR<-rotatepts(xpoints,ypoints,pi/10)
#plot(0,0,xlim=c(-15,15),ylim=c(-15,15),type="n")  #visualization
#lines(xpoints,ypoints)
#lines(pointsR[,c(1:2)])



