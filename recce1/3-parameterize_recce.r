#Parameterize recce based on Congo data
#install pkgs from http://lib.stat.cmu.edu/R/CRAN

library(splines)
library(mgcv)
library(RColorBrewer)
library(sp)
library(maptools)
library(shapefiles)

## Lat/Lon list of fecal samples logged thru Mar2011
## ebola=0 not tested; ebola=1 positive; ebola=2 negative (antibody test result)
## roadeast=0 west of road; roadeast=1 east of road

test1<-read.csv("sMar2011.csv")							
plot(test1[,3:4], col=ifelse(test1[,2]==1,"red","black"), pch=20)  #units lat/lon

## congo sampling map from Alain (March 2011 visit)
## congo.shp<-read.shape("/Users/olson/1.FILES/WCS2011/1.Working_papers/Fecal_landscape/2.Stats/shapefile_general.shp")
congo.shp<-readShapeSpatial("shapefile_general.shp")
lines(congo.shp)

## Create line segments from .shp coordinates (12 ironXs)

gps2seg<-function(congo.shp,id1,id2)
	{
  		gps_seg<-matrix(NA,nrow=10,ncol=4)
		colnames(gps_seg)<-c("gx1","gy1","gx2","gy2")  #GPS coordinates converted to segements
		gps_seg[1:5,1] <-coordinates(congo.shp)[[id1]][[1]][,1]
		gps_seg[1:5,2] <-coordinates(congo.shp)[[id1]][[1]][,2]
		gps_seg[1:4,3] <-coordinates(congo.shp)[[id1]][[1]][2:5,1]
		gps_seg[5,3]   <-coordinates(congo.shp)[[id1]][[1]][1,1]
		gps_seg[1:4,4] <-coordinates(congo.shp)[[id1]][[1]][2:5,2]
		gps_seg[5,4]   <-coordinates(congo.shp)[[id1]][[1]][1,2]
		gps_seg[6:10,1]<-coordinates(congo.shp)[[id2]][[1]][,1]
		gps_seg[6:10,2]<-coordinates(congo.shp)[[id2]][[1]][,2]
		gps_seg[6:9,3] <-coordinates(congo.shp)[[id2]][[1]][2:5,1]
		gps_seg[10,3]  <-coordinates(congo.shp)[[id2]][[1]][1,1]
		gps_seg[6:9,4] <-coordinates(congo.shp)[[id2]][[1]][2:5,2]
		gps_seg[10,4]  <-coordinates(congo.shp)[[id2]][[1]][1,2]
		gps_seg
	}

## Isolate each ironX using "Feces database for Lipkin-so.xls"
zone2<-gps2seg(congo.shp,1,2)  		#gps ironX 1
zoneJ<-gps2seg(congo.shp,3,4)  		#gps ironX 2
zoneA<-gps2seg(congo.shp,5,6)  		#gps ironX 3
zone999_1<-gps2seg(congo.shp,7,8)  	#gps ironX 4
zone999_2<-gps2seg(congo.shp,9,10)  #gps ironX 5
zoneH<-gps2seg(congo.shp,11,12)		#gps ironX 6
zoneI<-gps2seg(congo.shp,13,14)  	#gps ironX 7
zoneE<-gps2seg(congo.shp,15,16)  	#gps ironX 8
zoneM<-gps2seg(congo.shp,17,18)  	#gps ironX 9
zoneL<-gps2seg(congo.shp,19,20)  	#gps ironX 10
zoneG<-gps2seg(congo.shp,21,22)  	#gps ironX 11
zoneB<-gps2seg(congo.shp,23,24)  	#gps ironX 12
zoneF<-gps2seg(congo.shp,25,26)  	#gps ironX 13


#measure distance from fecal samples to nearest ironX
indata<-matrix(NA, nrow=nrow(test1),ncol=5)  	#create dataframe for fecal samples		
indata[,2]<-test1[,3]							#longitude px
indata[,3]<-test1[,4]							#latitude py
indata[,4]<-test1[,2]							#fecal status
colnames(indata)<-c("id","px","py","pf","zone")
sampledf<-data.frame(indata)
sampledf[,1]<-as.character(test1[,1])  			#id	
sampledf[,5]<-as.character(test1[,6])			#zone

#group samples into specific missions/ironX zones
smAprime<-subset(sampledf,zone=="A'")
smB1prime<-subset(sampledf,zone=="B1'")
smB2prime<-subset(sampledf,zone=="B2'")
smCprime<-subset(sampledf,zone=="C'")
smDprime<-subset(sampledf,zone=="D'")
sm1<-subset(sampledf,zone==1)
sm2<-subset(sampledf,zone==2)
sm3<-subset(sampledf,zone==3)
sm4<-subset(sampledf,zone==4)
smA<-subset(sampledf,zone=="A")
smB<-subset(sampledf,zone=="B")
smE<-subset(sampledf,zone=="E")
smF<-subset(sampledf,zone=="F")
smG<-subset(sampledf,zone=="G")
smH<-subset(sampledf,zone=="H")
smI<-subset(sampledf,zone=="I")
smJ<-subset(sampledf,zone=="J")
smL<-subset(sampledf,zone=="L")
smM<-subset(sampledf,zone=="M")



##Plotting gps samples and ironX recces
plot(test1[,3:4], col=ifelse(test1[,2]==1,"red","black"),type="n") 

## Not sampled in ironX configuration
points(smAprime[,2:3],  col="gray",pch=20)  #not ironX
points(smB1prime[,2:3], col="gray",pch=20)  #not ironX
points(smB2prime[,2:3], col="gray",pch=20)  #not ironX
points(smCprime[,2:3], col="gray",pch=20)   #not ironX
points(sm1[,2:3], col="gray",pch=20)		#not ironX
points(sm3[,2:3], col="gray" ,pch=20)		#not ironX
points(sm4[,2:3], col="gray" ,pch=20)		#not ironX

## Zone 2 	sampled 1 2010  SMALL ironX!!!
lines(coordinates(congo.shp)[[1]][[1]],col=rainbow(13)[1])
lines(coordinates(congo.shp)[[2]][[1]],col=rainbow(13)[1])
points(sm2[,2:3], col=rainbow(13)[1], pch=20)	

## Zone J 	sampled 3x 2008 2009 2010
lines(coordinates(congo.shp)[[3]][[1]],col=rainbow(13)[2])
lines(coordinates(congo.shp)[[4]][[1]],col=rainbow(13)[2])
points(smJ[,2:3], col=rainbow(13)[2], pch=20)

## Zone A	sampled 1x 2009
lines(coordinates(congo.shp)[[5]][[1]], col=rainbow(13)[3])
lines(coordinates(congo.shp)[[6]][[1]], col=rainbow(13)[3])
points(smA[,2:3], col=rainbow(13)[3], pch=20)

## Zone ??? no feces??  may be one unaccounted for grey dot
##lines(coordinates(congo.shp)[[7]][[1]],col=rainbow(13)[4])
##lines(coordinates(congo.shp)[[8]][[1]],col=rainbow(13)[4])

## Zone H  sampled 2x 2008 2009
lines(coordinates(congo.shp)[[11]][[1]], col=rainbow(13)[5])
lines(coordinates(congo.shp)[[12]][[1]], col=rainbow(13)[5])
points(smH[,2:3], col=rainbow(13)[5], pch=20)	

## Zone E sampled 2x Feb09 Nov09
lines(coordinates(congo.shp)[[15]][[1]], col=rainbow(13)[6])
lines(coordinates(congo.shp)[[16]][[1]], col=rainbow(13)[6])
points(smE[,2:3], col=rainbow(13)[6], pch=20)	

## Zone G sampled 2x Jan09 Aug09
lines(coordinates(congo.shp)[[21]][[1]],col=rainbow(13)[7])
lines(coordinates(congo.shp)[[22]][[1]],col=rainbow(13)[7])
points(smG[,2:3], col=rainbow(13)[7], pch=20)	

## Zone B sampled 1x 2008
lines(coordinates(congo.shp)[[23]][[1]], col=rainbow(13)[8])
lines(coordinates(congo.shp)[[24]][[1]], col=rainbow(13)[8])
points(smB[,2:3], col=rainbow(13)[8], pch=20)

## Zone ??? no feces??
##lines(coordinates(congo.shp)[[9]][[1]],  col=rainbow(13)[9])
##lines(coordinates(congo.shp)[[10]][[1]], col=rainbow(13)[9])

## Zone I sampled 2x 2008 2009  *SK-GOR 11 appears to be outlier...
lines(coordinates(congo.shp)[[13]][[1]],col=rainbow(13)[10])
lines(coordinates(congo.shp)[[14]][[1]],col=rainbow(13)[10])
points(smI[,2:3], col=rainbow(13)[10], pch=20)

## Zone M sampled 3x 2008 2009 2010
lines(coordinates(congo.shp)[[17]][[1]],col=rainbow(13)[11])
lines(coordinates(congo.shp)[[18]][[1]],col=rainbow(13)[11])
points(smM[,2:3], col=rainbow(13)[11], pch=20)

## Zone L sampled 2x 2008 2009
lines(coordinates(congo.shp)[[19]][[1]],col=rainbow(13)[12])
lines(coordinates(congo.shp)[[20]][[1]],col=rainbow(13)[12])
points(smL[,2:3], col=rainbow(13)[12], pch=20)	

## Zone F sampled 1x 2008 possible outlier SK-GOR 1	0.120410
lines(coordinates(congo.shp)[[25]][[1]],col=rainbow(13)[13])
lines(coordinates(congo.shp)[[26]][[1]],col=rainbow(13)[13])
points(smF[,2:3], col=rainbow(13)[13], pch=20)

legend(14.5,2.1,
	c("NoIronX", "Zone2", "ZoneJ","ZoneA",#"Zone??",
	"ZoneH","ZoneE","ZoneG","ZoneB",#"Zone??",
	"ZoneI","ZoneM","ZoneL","ZoneF"), pch=20, 
	col=c("gray",rainbow(13)[c(1,2,3,5,6,7,8,10,11,12,13)]), xjust=0, yjust=2) 
##End plotting




## Calculate distances from samples to respective zones/ironXs
patterndf<-as.data.frame(zone2)
sampledf<-as.data.frame(sm2)
mind<-psampleDistances(patterndf,sampledf) 		#units are decimal degrees
sm2d<-cbind(sampledf, mind)	

patterndf<-as.data.frame(zoneJ)
sampledf<-as.data.frame(smJ)
mind<-psampleDistances(patterndf,sampledf) 		#units are decimal degrees
smJd<-cbind(sampledf, mind)	

patterndf<-as.data.frame(zoneA)
sampledf<-as.data.frame(smA)
mind<-psampleDistances(patterndf,sampledf) 		#units are decimal degrees
smAd<-cbind(sampledf, mind)	

patterndf<-as.data.frame(zoneH)
sampledf<-as.data.frame(smH)
mind<-psampleDistances(patterndf,sampledf) 		#units are decimal degrees
smHd<-cbind(sampledf, mind)						#DIRECTLY ON LINE 

patterndf<-as.data.frame(zoneI)
sampledf<-as.data.frame(smI)
mind<-psampleDistances(patterndf,sampledf) 		#units are decimal degrees
smId<-cbind(sampledf, mind)	

patterndf<-as.data.frame(zoneE)
sampledf<-as.data.frame(smE)
mind<-psampleDistances(patterndf,sampledf) 		#units are decimal degrees
smEd<-cbind(sampledf, mind)						#DIRECTLY ON LINE

patterndf<-as.data.frame(zoneM)
sampledf<-as.data.frame(smM)
mind<-psampleDistances(patterndf,sampledf) 		#units are decimal degrees
smMd<-cbind(sampledf, mind)						#DIRECTLY ON LINE

patterndf<-as.data.frame(zoneL)
sampledf<-as.data.frame(smL)
mind<-psampleDistances(patterndf,sampledf) 		#units are decimal degrees
smLd<-cbind(sampledf, mind)						#DIRECTLY ON LINE

patterndf<-as.data.frame(zoneG)
sampledf<-as.data.frame(smG)
mind<-psampleDistances(patterndf,sampledf) 		#units are decimal degrees
smGd<-cbind(sampledf, mind)						

patterndf<-as.data.frame(zoneB)
sampledf<-as.data.frame(smB)
mind<-psampleDistances(patterndf,sampledf) 		#units are decimal degrees
smBd<-cbind(sampledf, mind)						#DIRECTLY ON LINE

patterndf<-as.data.frame(zoneF)
sampledf<-as.data.frame(smF)
mind<-psampleDistances(patterndf,sampledf) 		#units are decimal degrees
smFd<-cbind(sampledf, mind)						#DIRECTLY ON LINE


## Combine all zones w/ distances
allzoned<-rbind(sm2d,smJd,smAd,smId,smGd)
allzone999<-rbind(smHd, smEd,smMd,smLd,smBd,smFd) #too small to measure = 0 

p1<-allzoned[,-7]
p2<-allzone999[,-6]
p3<-cbind(p2,rep(0,nrow(p2))) # set distances that were too small to measure =0 
colnames(p3)[6]<-c("d")  

allz<-subset(rbind(p1,p3),d<0.005)  #remove outliers d > 0.005

## Convert to m 
## The radius of the semi major axis of the Earth at the equator is 6,378,160.0 m 
## resulting in a circumference of 40,075,161.2 m 
## The equator is divided into 360 degrees of longitude
## so each degree at the equator represents 111,319.9 m and we are near enough...

allz<-cbind(allz, allz[,6]*111319.9) #distance in meters
colnames(allz)[7]<-"dm"

dmhist<-hist(allz$dm) 									#distances (m) gps ironX to samples


## Fit probability density function for hist(allz$dm)
## Estimate lamda
## 
density <-dmhist$density
x <-dmhist$mids
exp_simp1<-nls( density ~ exp(-x/c), start=(c(c=5)))  # need to rescale fy to gy
AIC(exp_simp1)
#[1] -100.2121

F.g.hat<-function (x) exp(-x/5.907)
newx<-F.g.hat(seq(0:450))
plot(seq(0:450),newx)

u.hat<-integrate(F.g.hat,0,425)
n = length(x)
L_ironX = 4*20 + 4*sqrt(50) #length of standard L_ironX
p1_zone2<-zone2[1:4,]
p2_zone2<-zone2[6:9,]
calc_L_ironX <- function (zone) 
	{
	distance<-vector(length=nrow(zone))
	for (i in 1:nrow(zone))
		{
		distance[i]<-sqrt((zone[[i,1]]-zone[[i,3]])^2 + (zone[[i,2]]-zone[[i,4]])^2)
		}
	return (distance)
	}
		
L_Zone2 <- sum(calc_L_ironX(p1_zone2),calc_L_ironX(p2_zone2)) *111.3199 #distance in km
		

L = (19*L_ironX + 1*L_Zone2) * 1000 #distance in m
d.hat<-n/(2*L*u.hat[[1]])  #estimated density




# Buckland 2001 pg 48
y = seq(0,450,by=1)		# distances
lamda = 50				# guess
gy<-exp(-y/lamda)
plot(y,gy,type='l')





pdfhist<-hist(allz$dm,breaks="Sturges",plot=FALSE)
smoo<-smooth.spline(pdfhist$mids,pdfhist$density, df=4)
hist(allz$dm,breaks="Sturges",freq=FALSE, main="Histogram and fecal density detection function",
	xlab="distance (m)", ylab="probability")
lines(predict(smoo,c(1:450))$x,predict(smoo,c(1:450))$y,"l",col="green")




hist(allz$dm,breaks="Sturges",freq=FALSE, main="Histogram and fecal density detection function",
	xlab="distance (m)", ylab="probability")

plot(predict(smoo,c(1:450))$x,1/(predict(smoo,c(1:450))$y),"l",col="green")



nls( y ~ a + b * exp(c * x), start = c(a=1, b=1, c=-1) )  # TODO: Find some data




f<-pdfhist$density
d<-pdfhist$mids
test<-nls( f ~ 1/(exp(d/lamda)), start = c(lamda=5) )  # TODO: Find some data
plot(y,predict(test,y=c(1:450)),l) 


?selfStart

y<-allz$dm
exp_1<-glm(exp(y))