###IRONX####
## Define line segments for one ironX sample
ls1<-c( sqrt(50), sqrt(50),-sqrt(50),-sqrt(50), 20)     # line segment A 20 km  (#1)
ls2<-c(       0,        10,        0,      -10, 20)     # line segment B 20 km  (#2)
ls3<-c(-sqrt(50), sqrt(50), sqrt(50),-sqrt(50), 20)     # line segment C 20 km  (#3)
ls4<-c(     -10,         0,       10,        0, 20)     # line segment D 20 km  (#4)
ls5<-c( sqrt(50), sqrt(50),        0,       10, 7.653669)               # line segment E 5.18 km (#5)
ls6<-c(-sqrt(50), sqrt(50),      -10,        0, 7.653669)               # line segment F 5.18 km (#6)
ls7<-c(-sqrt(50),-sqrt(50),        0,      -10, 7.653669)               # line segment G 5.18 km (#7)
ls8<-c( sqrt(50),-sqrt(50),       10,        0, 7.653669)               # line segment H 5.18 km (#8)

ironX<-t(data.frame(cbind(ls1,ls2,ls3,ls4,ls5,ls6,ls7,ls8)))
colnames(ironX) <- c("x1", "y1", "x2", "y2", "magnitude")
ironX0<-data.frame(ironX)  #centered on (0,0) units=km

                                        #xpoints<-c(0,sqrt(50), 0,0,-sqrt(50),-10,0, -sqrt(50),  0,0, sqrt(50),10,0) #IronX x-coords
                                        #ypoints<-c(0,sqrt(50),10,0, sqrt(50),  0,0, -sqrt(50),-10,0,-sqrt(50), 0,0) #IronX y-coords

                                        #plot(0,0,xlim=c(-15,15),ylim=c(-15,15),type="n")  #visualization
                                        #lines(xpoints,ypoints)
                                        #lines(xpoints+100,ypoints+100)
