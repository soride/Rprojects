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

## Isolate each ironX using "Feces database for Lipkin-so.xls"
zone2<-gps2seg(congo.shp,1,2)           #gps ironX 1
zoneJ<-gps2seg(congo.shp,3,4)           #gps ironX 2
zoneA<-gps2seg(congo.shp,5,6)           #gps ironX 3
zone999_1<-gps2seg(congo.shp,7,8)       #gps ironX 4
zone999_2<-gps2seg(congo.shp,9,10)  #gps ironX 5
zoneH<-gps2seg(congo.shp,11,12)         #gps ironX 6
zoneI<-gps2seg(congo.shp,13,14)         #gps ironX 7
zoneE<-gps2seg(congo.shp,15,16)         #gps ironX 8
zoneM<-gps2seg(congo.shp,17,18)         #gps ironX 9
zoneL<-gps2seg(congo.shp,19,20)         #gps ironX 10
zoneG<-gps2seg(congo.shp,21,22)         #gps ironX 11
zoneB<-gps2seg(congo.shp,23,24)         #gps ironX 12
zoneF<-gps2seg(congo.shp,25,26)         #gps ironX 13


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

## Zone 2       sampled 1 2010  SMALL ironX!!!
lines(coordinates(congo.shp)[[1]][[1]],col=rainbow(13)[1])
lines(coordinates(congo.shp)[[2]][[1]],col=rainbow(13)[1])
points(sm2[,2:3], col=rainbow(13)[1], pch=20)

## Zone J       sampled 3x 2008 2009 2010
lines(coordinates(congo.shp)[[3]][[1]],col=rainbow(13)[2])
lines(coordinates(congo.shp)[[4]][[1]],col=rainbow(13)[2])
points(smJ[,2:3], col=rainbow(13)[2], pch=20)

## Zone A       sampled 1x 2009
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

## Zone F sampled 1x 2008 possible outlier SK-GOR 1     0.120410
lines(coordinates(congo.shp)[[25]][[1]],col=rainbow(13)[13])
lines(coordinates(congo.shp)[[26]][[1]],col=rainbow(13)[13])
points(smF[,2:3], col=rainbow(13)[13], pch=20)

legend(14.5,2.1,
       c("NoIronX", "Zone2", "ZoneJ","ZoneA",#"Zone??",
         "ZoneH","ZoneE","ZoneG","ZoneB",#"Zone??",
         "ZoneI","ZoneM","ZoneL","ZoneF"), pch=20,
       col=c("gray",rainbow(13)[c(1,2,3,5,6,7,8,10,11,12,13)]), xjust=0, yjust=2)
##End plotting


lzone<-c(55,rep(111,10))
nvisits<-c(1,1,1,2,1,2,2,2,3,2,3)
nsamples<-c(3,4,5,7,4,20,31,13,20,9,31)
zone<-c("sm2","smA","smB","smE","smF","smG","smH","smI","smJ","smL","smM")
effort<-data.frame(zone,nsamples,nvisits,lzone)

pdetect<-(nsamples/ltotal)
effort[,5]<-pdetect
effort[,6]<-30/effort[,5]
colnames(effort)[5:6] <- c("pdetect","km430n")
effort[order(effort$V5,decreasing=TRUE),]

sum(nsamples)/(sum(nvists*lzone)) #overall detection prob

##Plotting gps samples and ironX recces
quartz()
plot(test1[,3:4],col=ifelse(test1[,2]==1,"red","black"),type="n")
                                        #smH
lines(coordinates(congo.shp)[[11]][[1]], col=rainbow(13)[5])
lines(coordinates(congo.shp)[[12]][[1]], col=rainbow(13)[5])
                                        #smM
lines(coordinates(congo.shp)[[17]][[1]],col=rainbow(13)[11])
lines(coordinates(congo.shp)[[18]][[1]],col=rainbow(13)[11])
                                        #smG
lines(coordinates(congo.shp)[[21]][[1]],col=rainbow(13)[7])
lines(coordinates(congo.shp)[[22]][[1]],col=rainbow(13)[7])

points(test1[,3:4],col=ifelse(test1[,2]==1,"red","black"), pch=20)

mtext(3,"High sample yield zones: M(.09), H(.14), G(.9)")
