### Functions for simulating ironX and DISTANCE sampling
## rm(list = ls())
library(spatstat)
library(DSpat)
## Define line segment for DISTANCE sampling Ape density 2.16 ind/km2
## in Ndoki-Likouala Conservation landscape over 27,970 km2 sampled
## with 168 x 2 km transects (Stokes et al. 2010) [0.006 transects/km2
## ~ 240 x 2 km transects for 40,000 km2]166 x 2km transects for
## 28,000 km2 (stokes 2010)

                                        #inputs<-list()
                                        #inputs[[1]]<-basic[[1]]  #from 5. stoke simulation

                                        #xseed<-sample(seq(0,10,0.002),1)  #set random start for transects
                                        #study.area=owin(xrange=c(xseed,xseed+2),yrange=c(0,65)) #one vertical set of transects
                                        #inputs[[2]]<-create.lines(study.area,nlines=4,width=.002,spacing=100,angle=180)

                                        #test<-distancesim(inputs)                                        #plot(test[[3]] $lines,lty=1)
                                        #points(test[[9]][,2:3])

dist2line.sho <- function (ptnlines) #measures distances from points
                                        #to line segment
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
distancesim<-function(inputs){
    xvals <- inputs[[1]][,2]
    yvals <- inputs[[1]][,3]
    label <- c(1:4) # number of horizontal sets = 4
    xp<-inputs[[2]]
    col1<-cbind(label,xp[,2:3]+16,xp[,4:6])
    col2<-cbind(label,col1[,2:3]+16,xp[,4:6])
    col3<-cbind(label,col2[,2:3]+16,xp[,4:6])
    linesdf <- rbind(xp,col1,col2,col3) #4 verticals sets
    study.area=owin(xrange=c(0,65),yrange=c(0,65))
    ls=lines_to_strips(linesdf,study.area) #converted to strips
    label <- c(1:nrow(ls$lines$end))
    dsort<-matrix(nrow=length(xvals),ncol=3)
    dsort[,1]<-xvals
    dsort[,2]<-yvals
    dsort[,3]<-rep(999,length(xvals))
    for (j in 1:length(xvals))
    {
        for (i in 1:16){
            if(
               (dsort[j,1] <= ls$transects[i][[1]]$x[1]) & (dsort[j,1] >= ls$transects[i][[1]]$x[3]) & (dsort[j,2] <= ls$transects[i][[1]]$y[1]) & (dsort[j,2] >= ls$transects[i][[1]]$y[3])
               )
            { dsort[j,3]<-i }
        }
    }
    colnames(dsort)<-c("xvals","yvals","label")
    dsortdf<-data.frame(dsort)
    label <- c(1:nrow(ls$lines$end))
    linesdf<-data.frame(label,ls$lines$ends)
    keep<-dsortdf[dsortdf[,3]!=999,]
    ptnlines<-merge(keep,linesdf,by="label") #alldf
    realD <-dist2line.sho(ptnlines)
    samples <-cbind(ptnlines,realD)
    out<-list(xvals,yvals,xp,ls,dsortdf,linesdf,keep,ptnlines,realD,samples)
    out
}


