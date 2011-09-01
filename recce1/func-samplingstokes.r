### Functions for simulating  sampling in a 65 km x 65 km landscape

library(spatstat)
## Generate random fecal sample coordinates
## Parameters and random location and prevalence distribution for fecal antibodies
random_basic<-function (n=50,fecal_prev=0.1)  # fecal sample: neg=0,pos=1
{
    indata<-matrix(NA,nrow=n,ncol=5)
    indata[,2]<- sample(seq(0,65, by=0.00001),n,replace=FALSE)   # random point x
    indata[,3]<- sample(seq(0,65, by=0.00001),n,replace=FALSE)   # random point y
    indata[,4]<- sample(c(0,1), n, replace = TRUE, prob =
                        c(1-fecal_prev,fecal_prev))
    indata[,5]<-rep(fecal_prev,n)
    colnames(indata)<-c("id","px","py","pf","fecal_prev")
    indata
}
                                        #data<-random_basic()                                      #points(data[,c(2,3)],col=data[,4]+1,pch=20)             ## visualization

## Cluster parameters
                                        #k = 20          #kappa=intensity of Poisson process of cluster centers
                                        #r = 0.04        #r=radius of clusters
                                        #mu = 50         #mumean number of points per cluster
                                        #fecal_prev=0.1
random_clusterT<-function (k,r,mu,fecal_prev)   #Thomas
{
    clust<-rThomas(k, r, mu)
    clust$x <- 65*clust$x
    clust$y <- 65*clust$y
    indata<-matrix(NA, nrow=clust$n,ncol=5)
    indata[,2]<-clust$x
    indata[,3]<-clust$y
    indata[,4]<-sample(c(0,1),clust$n, replace = TRUE, prob =
                       c(1-fecal_prev,fecal_prev))
    indata[,5]<-rep(fecal_prev,clust$n)
    colnames(indata)<-c("id","px","py","pf","fecal_prev")
    indata
}

                                        #dada<-data.frame(random_clusterT(k=5,r=(2.5/65),mu=100000,fecal_prev=0.1))

                                        #plot(dada[,c(2,3)],pch=19,cex=.01)
                                        #data<-random_clusterT(k,r,mu,fecal_prev)
                                        #plot(0,0,xlim=c(0,200),ylim=c(0,200),type="n")  #
                                        #points(data[,c(2,3)],col=data[,4]+1,pch=20)


random_clusterM<-function (k,r,mu,fecal_prev)  #Matern
{
    clust<-rMatClust(k,r,mu)
    clust$x <- 65*clust$x
    clust$y <- 65*clust$y
    indata<-matrix(NA, nrow=clust$n,ncol=5)
    indata[,2]<-clust$x
    indata[,3]<-clust$y
    indata[,4]<-sample(c(0,1),clust$n,
                       replace = TRUE, prob =
                       c(1-fecal_prev,fecal_prev))
    indata[,5]<-rep(fecal_prev,clust$n)
    colnames(indata)<-c("id","px","py","pf","fecal_prev")
    indata
}
                                        #data<-random_clusterM(k=3,r=(20/65),mu=167000,fecal_prev=0.1)
                                        #plot(data[,c(2,3)],pch=19,cex=.011)
                                        #plot(0,0,xlim=c(0,65),ylim=c(0,65),type="n")
                                        #ironX100<-move_ironX(ironX,x=100,y=100)



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



