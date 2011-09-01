### Functions for simulating ironX and DISTANCE sampling
## rm(list = ls())
library(spatstat)
library(DSpat)
## Define line segment for DISTANCE sampling Ape density 2.16 ind/km2
## in Ndoki-Likouala Conservation landscape over 27,970 km2 sampled
## with 168 x 2 km transects (Stokes et al. 2010) [0.006 transects/km2
## ~ 240 x 2 km transects for 40,000 km2]166 x 2km transects for
## 28,000 km2 (stokes 2010)


px<-c(6,6,6,6,22,22,22,22)
py<-c(10.64892,26.89892,43.14892,59.39892,10.649,26.898,43.128,59.399)
dsort<-matrix(nrow=length(px),ncol=3)

distancesim<-function(input){
    xseed<-sample(seq(0,10,0.002),1)  #set random start for transects
    study.area=owin(xrange=c(xseed,xseed+2),yrange=c(0,65)) #one vertical set of transects
    xp=create.lines(study.area,nlines=4,width=.002,spacing=100,angle=180)
    label <- c(1:4)
    col1<-cbind(label,xp[,2:3]+16,xp[,4:6])
    col2<-cbind(label,col1[,2:3]+16,xp[,4:6])
    col3<-cbind(label,col2[,2:3]+16,xp[,4:6])
    xps <- rbind(xp,col1,col2,col3) #7 verticals sets
    study.area=owin(xrange=c(0,65),yrange=c(0,65))
    ls=lines_to_strips(xps,study.area) #converted to strips
    plot(ls$lines,lty=1)
    label <- c(1:nrow(ls$lines$end))
    dsort[,1]<-px
    dsort[,2]<-py
    dsort[,3]<-rep(999,length(px))
    for (j in 1:length(px))
    {
        for (i in 1:16){
            if(
               (px[j] <= ls$transects[i][[1]]$x[1]) & (px[j] >= ls$transects[i][[1]]$x[3]) & (py[j] <= ls$transects[i][[1]]$y[1]) & (py[j] >= ls$transects[i][[1]]$y[3])
               )
            { dsort[i,3]<-i }
        }
    }
    colnames(dsort)<-c("xvals","yvals","label")
    label <- c(1:nrow(ls$lines$end))
    linends<-data.frame(label,ls$lines$ends)
    ptnlines<-merge(dsort,linends,by="label")
}


dist2line.sho(ptnlines)
tester<-matrix(ncol=7,nrow=2)
tester[1,]<-c(0,1,1,-5,0,5,0)
tester[2,]<-c(0,1,1,-5,-5,5,5)
dist2line.sho(tester)

dist2line.sho <- function (ptnlines)
{
    x.bar = ptnlines[,2]
    y.bar = ptnlines[,3]
    x0 = ptnlines[,4]
    y0 = ptnlines[,5]
    x1 = ptnlines[,6]
    y1 = ptnlines[,7]
    distVals =  abs((x1-x0)*(y0-y.bar)-(x0-x.bar)*(y1-y0))/sqrt((x1-x0)^2+(y1-y0)^2)
    return(distance = distVals)
}





square=owin(xrange=c(0,200),yrange=c(0,200))
Dlines=create.lines(square,nlines=20,width=.0058,angle=180)
Dstrips=lines_to_strips(Dlines,square)
plot(Dstrips$lines,lty=1)
                                        #dev()
matplot(t(Dlines[,2:3]),t(Dlines[,4:5]),type='l')


                                        #colnames(DSpat.lines)
                                        #[1] "label" "x0"    "x1"    "y0"    "y1"
                                        #colnames(DSpat.obs)
                                        #[1] "label" "x"     "y"

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

xpoints<-c(0,sqrt(50), 0,0,-sqrt(50),-10,0, -sqrt(50),  0,0, sqrt(50),10,0) #IronX x-coords
ypoints<-c(0,sqrt(50),10,0, sqrt(50),  0,0, -sqrt(50),-10,0,-sqrt(50), 0,0) #IronX y-coords

plot(0,0,xlim=c(-15,15),ylim=c(-15,15),type="n")  #visualization
lines(xpoints,ypoints)
lines(xpoints+100,ypoints+100)


## Function to change number and location of ironXs Linear transformation of ironX
xvals<-c(0,50,100)
yvals<-c(0,50,100)
move_ironXs<-function (ironX0,xvals,yvals)
{
    new_ironXs<-matrix(NA,nrow=length(xvals)*8,ncol=4)
    for (i in 1:length(xvals))
    {
        new_ironXs[(1+(i-1)*8):(i*8),1]<-ironX0[[1]]+xvals[i]
        new_ironXs[(1+(i-1)*8):(i*8),2]<-ironX0[[2]]+yvals[i]
        new_ironXs[(1+(i-1)*8):(i*8),3]<-ironX0[[3]]+xvals[i]
        new_ironXs[(1+(i-1)*8):(i*8),4]<-ironX0[[4]]+yvals[i]
    }
    colnames(new_ironXs)<-c("nx1","ny1","nx2","ny2")
    new_ironXs
}
xvals<-c(0,50,100)
yvals<-c(0,50,100)
ironXs<-move_ironXs(ironX0,xvals,yvals)
plot(0,0,xlim=c(0,200),ylim=c(0,200),type="n")  ## visualization
points(ironXs[,1:2])
points(ironXs[,3:4])



## Generate random fecal sample coordinates
## Parameters and random location and prevalence distribution for fecal antibodies
random_basic<-function (n=50,fecal_prev=0.1)  # fecal sample: neg=0,pos=1
{
    indata<-matrix(NA,nrow=n,ncol=5)  #                           indata[,1]<-paste("rand_",i,sep="")
    indata[,2]<- sample(seq(0,200, by=0.001),n,replace=FALSE)   # random point x
    indata[,3]<- sample(seq(0,200, by=0.001),n,replace=FALSE)   # random point y
    indata[,4]<- sample(c(0,1), n, replace = TRUE, prob =
                        c(1-fecal_prev,fecal_prev))
    indata[,5]<-rep(fecal_prev,n)
    colnames(indata)<-c("id","px","py","pf","PF")
    indata
}

data<-random_basic()
points(data[,c(2,3)],col=data[,4]+1,pch=20)             ## visualization

## Cluster parameters
k = 20          #kappa=intensity of Poisson process of cluster centers
r = 0.04        #r=radius of clusters
mu = 50         #mumean number of points per cluster
fecal_prev=0.1
random_clusterT<-function (k,r,mu,fecal_prev)   #Thomas
{
    clust<-rThomas(k, r, mu)
    clust$x <- 200*clust$x
    clust$y <- 200*clust$y
    indata<-matrix(NA, nrow=clust$n,ncol=5)
                                        #                               indata[,1]<-paste("m",1:clust$n,"k",kappa,"r",r,"m",mu,sep="")
    indata[,2]<-clust$x
    indata[,3]<-clust$y
    indata[,4]<-sample(c(0,1),clust$n, replace = TRUE, prob =
                       c(1-fecal_prev,fecal_prev))
    indata[,5]<-rep(fecal_prev,clust$n)
    colnames(indata)<-c("id","px","py","pf","PF")
    indata
}

data<-random_clusterT(k,r,mu,fecal_prev)
plot(0,0,xlim=c(0,200),ylim=c(0,200),type="n")  #
points(data[,c(2,3)],col=data[,4]+1,pch=20)


random_clusterM<-function (k,r,mu,fecal_prev)  #Matern
{
    clust<-rMatClust(k,r,mu)
    clust$x <- 200*clust$x
    clust$y <- 200*clust$y
    indata<-matrix(NA, nrow=clust$n,ncol=5)
                                        #                              data[,1]<-paste("t",1:clust$n,"k",kappa,"r",r,"m",mu,sep="")
    indata[,2]<-clust$x
    indata[,3]<-clust$y
    indata[,4]<-sample(c(0,1),clust$n,
                       replace = TRUE, prob =
                       c(1-fecal_prev,fecal_prev))
    indata[,5]<-rep(fecal_prev,clust$n)
    colnames(indata)<-c("id","px","py","pf","PF")
    indata
}
data<-random_clusterM(k,r,mu,fecal_prev)
plot(0,0,xlim=c(0,200),ylim=c(0,200),type="n")
                                        #ironX100<-move_ironXs(ironX,x=100,y=100)
points(data[,c(2,3)],col=data[,4]+1,pch=20)


psampleDistances<-function (patterndf,sampledf)
{
    n = nrow(sampledf)
    out<-matrix(NA,nrow=n,ncol=2)   #shortest distance from point to each line segment
    for (j in 1:n)
    {
        allseg<-matrix(NA,nrow=1,ncol=8)        ## Matrix to catch values
        px<-sampledf[[2]][j]
        py<-sampledf[[3]][j]
        for (i in 1:8)
        {
            x1<-patterndf[[1]][i]
            y1<-patterndf[[2]][i]
            x2<-patterndf[[3]][i]
            y2<-patterndf[[4]][i]
            lineMagnitude <- function(x1, y1, x2, y2) sqrt((x2 - x1)^2+(y2 - y1)^2)
            ans <- NULL
            ix <- iy <- 0   # intersecting point
            lineMag <- lineMagnitude(x1, y1, x2, y2)
            if( lineMag < 0.00000001)                   ## Returns 9999 on 0 denominator conditions
            {
                warning("short segment")
                return(9999)
            }
            u <- (((px - x1) * (x2 - x1)) + ((py - y1) * (y2 - y1))) / (lineMag * lineMag)  ## slope of line
            if((u < 0.00001) || (u > 1))
            {                           ## closest point does not fall within the line segment,
                ix <- lineMagnitude(px, py, x1, y1)     ## take the shorter distance to an endpoint
                iy <- lineMagnitude(px, py, x2, y2)
                if(ix > iy)  ans <- iy
                else ans <- ix
            }
            else                                        ## Intersecting point is on the line, use the formula
            {
                ix <- x1 + u * (x2 - x1)
                iy <- y1 + u * (y2 - y1)
                ans <- lineMagnitude(px, py, ix, iy)
            }
            allseg[1,i]<-ans
        }
        out[j,1]<-min(allseg)  #distance
        out[j,2]<-round(which(allseg==min(allseg))[1])  #which segment is closest
    }
    colnames(out)<-c("d","seg")
    out
}



