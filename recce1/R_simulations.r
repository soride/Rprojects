# Fecal sample simulations

xpoints<-c(0,sqrt(50), 0,0,-sqrt(50),-10,0, -sqrt(50),  0,0, sqrt(50),10,0) #IronX x-coords
ypoints<-c(0,sqrt(50),10,0, sqrt(50),  0,0, -sqrt(50),-10,0,-sqrt(50), 0,0)   #IronX y-coords

plot(0,0,xlim=c(-15,15),ylim=c(-15,15),type="n")
lines(xpoints,ypoints) #plot IronX


#Generate random fecal sample coordinates
# Parameters
apeD = 2.16 #individual density per km2 (Stokes Plos B et al. 2010 Ndoki-Likouala Landscape)
area = 900 #sampling area (personal estimate length of IronX +5 km on either side
fecal_prev = 0.05 #fecal antibody prevalence 

# More parameter calculations
apes <- nestD*area #no. of apes in sample area

#random location and prevalence distribution for fecal antibodies
mpoop<-matrix(data = NA, nrow = apes, ncol = 3, byrow = FALSE, dimnames = NULL)
mpoop[,1]<-sample(seq(-15,15, by=0.001),apes,replace=FALSE)
mpoop[,2]<-sample(seq(-15,15, by=0.001),apes,replace=FALSE)
mpoop[,3]<- sample(c(0,1), apes, replace = TRUE, prob = c(1-fecal_prev,fecal_prev)) 

#Poopscape figure
plot(0,0,xlim=c(-15,15),ylim=c(-15,15),type="n")
points(mpoop[,c(1,2)], pch=20, col=mpoop[,3]+1)
lines(xpoints,ypoints, lwd=5) #plot IronX



ironX<-lines(xpoints,ypoints,type="n") 


#Calculate distance between points and simple line transect
mtest<-matrix(data = NA, nrow=3, ncol=3)
mtest[,1]<-sample(seq(-10,10),3,replace=FALSE)
mtest[,2]<-sample(seq(-10,10),3,replace=FALSE)
mtest[,3]<- sample(c(0,1), 3, replace = TRUE, prob = c(1-fecal_prev,fecal_prev)) 

plot(0,0,xlim=c(-15,15),ylim=c(-15,15),type="n")
points(mtest[,c(1,2)], pch=20, col=mpoop[,3]+1)
lines(c(0,5),c(0,0), lwd=5) #plot IronX

px<-mtest[1,1]
py<-mtest[1,2]
x1<-sqrt(75)
y1<-5
x2<-(-sqrt(75))
y2<-(-5)
<-distancePointSegment(px,py,x1,y1,x2,y2) 



















