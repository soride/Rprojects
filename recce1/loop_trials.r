rm(list = ls())

xpoints<-c(0,sqrt(50), 0,0,-sqrt(50),-10,0, -sqrt(50),  0,0, sqrt(50),10,0) #IronX x-coords
ypoints<-c(0,sqrt(50),10,0, sqrt(50),  0,0, -sqrt(50),-10,0,-sqrt(50), 0,0) #IronX y-coords



n = 50 # set number of samples

allpts<-matrix(NA,nrow=n,ncol=7)
for (i in 1:n) 
	{
	allpts[i,]<-shortestdistancePointSegment(fecal_prev=0.05)
	}

colnames(allpts)<-c("d","seg","px","py","ix","iy","pf")
allpts

## Confirm point distance calculations are correct
plot(0,0,xlim=c(-15,15),ylim=c(-15,15),type="n")
lines(xpoints,ypoints)#, lwd=5) #plot IronX
#points(allpts[,3],allpts[,4],pch=20)
text(allpts[,3],allpts[,4], round(allpts[,1]), col=allpts[,7]+1)

## Create clustered values
0

Y <- rMatClust(20, 0.05, 100)
Y$x <- 1000*Y$x
Y$y <- 1000*Y$y
plot(Y$x, Y$y)


clust <- rMatClust(10, 0.05, 50)
plot(clust)
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
		



shortestdistancePointSegment <- function(fecal_prev=0.05) 
{
#Define ironX line segments
ls1<-c( sqrt(50), sqrt(50),-sqrt(50),-sqrt(50), 20) 	# line segment A 20 km  (#1)
ls2<-c(       0,        10,        0,      -10, 20) 	# line segment B 20 km  (#2)
ls3<-c(-sqrt(50), sqrt(50), sqrt(50),-sqrt(50), 20) 	# line segment C 20 km  (#3)
ls4<-c(     -10,         0,   	  10,        0, 20) 	# line segment D 20 km  (#4)
ls5<-c( sqrt(50), sqrt(50),        0,       10, 7.653669) 		# line segment E 5.18 km (#5)
ls6<-c(-sqrt(50), sqrt(50),      -10,        0, 7.653669) 		# line segment F 5.18 km (#6) 
ls7<-c(-sqrt(50),-sqrt(50),        0,      -10, 7.653669) 		# line segment G 5.18 km (#7)
ls8<-c( sqrt(50),-sqrt(50),       10,        0, 7.653669) 		# line segment H 5.18 km (#8)

ironX<-t(data.frame(cbind(ls1,ls2,ls3,ls4,ls5,ls6,ls7,ls8)))
colnames(ironX) <- c("x1", "y1", "x2", "y2", "magnitude")
ironX<-data.frame(ironX)

 ## Generate random fecal sample coordinates
 ## Parameters and random location and prevalence distribution for fecal antibodies
 px<- sample(seq(-15,15, by=0.001),1,replace=FALSE)  	# random point x
 py<- sample(seq(-15,15, by=0.001),1,replace=FALSE)		# random point y
 pf<- sample(c(0,1), 1, replace = TRUE, prob = c(1-fecal_prev,fecal_prev)) # fecal sample: neg=0,pos=1

 ## Matrix to catch values
 allseg<-matrix(NA,nrow=1,ncol=8)   #shortest distance from point to each line segment
 out<-matrix(NA,nrow=1,ncol=7)		#shortest distance and matched data
 ## START LOOP
 for (i in 1:8) 
 {
x1<-ironX$x1[i]
 y1<-ironX$y1[i]
 x2<-ironX$x2[i]
 y2<-ironX$y2[i]

 ## px,py is the point to test, x1,y1,x2,y2 is the line to check distance
 ## Returns distance from the line, or if the intersecting point on the line nearest the point 
 ## tested is outside the endpoints of the line, the distance to the nearest endpoint.
 ## Returns 9999 on 0 denominator conditions.
 
 lineMagnitude <- function(x1, y1, x2, y2) sqrt((x2 - x1)^2+(y2 - y1)^2)
 ans <- NULL
 ix <- iy <- 0   # intersecting point
 lineMag <- lineMagnitude(x1, y1, x2, y2)
 
 if( lineMag < 0.00000001) 					## Returns 9999 on 0 denominator conditions
 	{                  
   warning("short segment")
   return(9999)
 	}

 u <- (((px - x1) * (x2 - x1)) + ((py - y1) * (y2 - y1))) / (lineMag * lineMag)  ## slope of line

 	if((u < 0.00001) || (u > 1)) 
 	{             								## closest point does not fall within the line segment, 
   		ix <- lineMagnitude(px, py, x1, y1)      ## take the shorter distance to an endpoint
   		iy <- lineMagnitude(px, py, x2, y2)
   		if(ix > iy)  ans <- iy
   			else ans <- ix
	 } 
	 else 						  				## Intersecting point is on the line, use the formula
 	{
   		ix <- x1 + u * (x2 - x1)
   		iy <- y1 + u * (y2 - y1)
   		ans <- lineMagnitude(px, py, ix, iy)
 	}
 allseg[1,i]<-ans
 }
##STOP LOOP
 d<-min(allseg)
 seg<-round(which(allseg==min(allseg))[1])
 out[1]<-d
 out[2]<-seg
 out[3]<-px
 out[4]<-py
 out[5]<-ix
 out[6]<-iy
 out[7]<-pf
 out

}







