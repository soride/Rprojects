                                        #Enter in a patterndf with x1y1 x2y2 points defining line segments
                                        #within the pattern.  Enter sampledf as random points.  Returns d
                                        #(distance from random point to nearest figure and segment as well as
                                        #the coordinates of the random point)

                                        #sampledf<-data.frame(matrix(c(NA,0,10,NA, -8,-4),ncol=3,byrow=TRUE))
                                        #patterndf<-data.frame(ironX0)
                                        #xvals<-c(0,5,10)
                                        #yvals<-c(0,5,10)
                                        #nsegs=8
                                        #patterndf<-data.frame(move_ironXs(ironX0,xvals,yvals,nsegs,angles))
                                        #sampledf<-data.frame(matrix(c(NA,0,0,NA,0,10,NA,10,10),nrow=3,byrow=T))
                                        #pointdistanceALL(patterndf,sampledf)

pointdistanceALL<-function (patterndf,sampledf,nsegs=8) #shortest distance from point to each
                                        #line segment, nsegs is number
                                        #of line segments in each figure
{
    n = nrow(sampledf)
    dmin<-matrix(NA,nrow=n,ncol=7)
    ans<-matrix(NA,nrow=n,ncol=nrow(patterndf))
    for (j in 1:n)
    {
        px<- sampledf[[2]][j]
        py<- sampledf[[3]][j]
        pf<- sampledf[[4]][j]
        fecal_prev<- sampledf[[5]][j]
        x1<-patterndf[[1]]
        y1<-patterndf[[2]]
        x2<-patterndf[[3]]
        y2<-patterndf[[4]]
        lineMagnitude <- function(x1, y1, x2, y2) sqrt((x2 - x1)^2+(y2
                                                                    - y1)^2)
        lineMag <- lineMagnitude(x1, y1, x2, y2) # of figure
        u<- (((px - x1) * (x2 - x1)) + ((py - y1) * (y2 - y1))) /
            (lineMag * lineMag) ## slope of the shortest line between
        ## the point and the figure line
        for (i in 1: nrow(patterndf)) {
            ix <- iy <- vector()   # intersecting point
            if((u[i] < 0.00001) | (u[i] > 1))
            {    ## closest point does not fall within the line segment,
                ix [i]<- lineMagnitude(px, py, x1[i], y1[i])
                iy [i] <- lineMagnitude(px, py, x2[i], y2[i])
                if(ix[i]  > iy[i])
                {ans[j,i] <- iy[i]}  else  {ans[j,i] <- ix[i]}
            } else      ## Intersecting point is on the line, use the formula
        {
            ix[i]  <- x1[i] + u [i] * (x2[i] - x1[i])
            iy[i]  <- y1[i] + u [i] * (y2[i] - y1[i])
            ans[j,i] <- lineMagnitude(px, py, ix[i], iy[i])
        }
            dmin[j,1] <-min(ans[j,])
            dmin[j,2]<-which((ans[j,])==min(ans[j,]))[1]  #which row
                                        #in patterndf  is
                                        #closest
            dmin[j,3]<-ceiling(which((ans[j,])==min(ans[j,]))[1]/nsegs)
            dmin[j,4]<-px
            dmin[j,5]<-py
            dmin[j,6] <- pf
            dmin[j,7] <-fecal_prev
        }
    }
    seg <-rep(1:nsegs,nrow(patterndf)/nsegs)[dmin[,2]]
    sampleid<-c(1:n)
    out<-cbind(dmin,seg,sampleid)
                                        #print(ans)
    colnames(out)<-c("d","patterndfrow","fignumber","px","py","pf","fecal_prev","segnumber","sampleid")
    out
}


