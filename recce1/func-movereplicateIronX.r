##Enter in line segments of a figure (here the ironX0) into a dataframe and then replicate and move
##to different locations given by xvals and yvals as center points of
##the figure

                                        # Define line segments for one ironX sample
ls1<-c( sqrt(50), sqrt(50),-sqrt(50),-sqrt(50), 20)     # line segment A 20 km  (#1)
ls2<-c(       0,        10,        0,      -10, 20)     # line segment B 20 km  (#2)
ls3<-c(-sqrt(50), sqrt(50), sqrt(50),-sqrt(50), 20)     # line segment C 20 km  (#3)
ls4<-c(     -10,         0,       10,        0, 20)     # line segment D 20 km  (#4)
ls5<-c( sqrt(50), sqrt(50),        0,       10, 7.653669)               # line segment E 5.18 km (#5)
ls6<-c(-sqrt(50), sqrt(50),      -10,        0, 7.653669)               # line segment F 5.18 km (#6)
ls7<-c(-sqrt(50),-sqrt(50),        0,      -10, 7.653669)               # line segment G 5.18 km (#7)
ls8<-c( sqrt(50),-sqrt(50),       10,        0, 7.653669)               # line segment H 5.18 km (#8)
ironX<-data.frame(t(data.frame(cbind(ls1,ls2,ls3,ls4,ls5,ls6,ls7,ls8))))
colnames(ironX) <- c("x1", "y1", "x2", "y2", "magnitude")
## Define center points of each line segment
centerpt<-function(ironX) {
    cx<-(ironX[[1]]+ironX[[3]])/2
    cy<-(ironX[[2]]+ironX[[4]])/2
    return (cbind(cx,cy))
}
centers<-data.frame(centerpt(ironX))
colnames(centers)<-c("cxs","cys")
ironX0<-data.frame(cbind(ironX,centers))

                                        #xvals<-3
                                        #yvals<-6
angles<-c(-pi/4,-pi/2,-3*pi/4,0,25*pi/180,-65*pi/180,25*pi/180,-65*pi/180)

move_ironXs<-function (ironX0,xvals,yvals,nsegs=8,angles)  #input figure
                                        #defined by lines between
                                        #(x1,y1) and (x2,y2) and new
                                        #center points xvals and yvals
                                        #and number of line segments in figure
{
    new_ironXs<-matrix(NA,nrow=length(xvals)*nsegs,ncol=9)
    for (i in 1:length(xvals))
    {
        new_ironXs[(1+(i-1)*nsegs):(i*nsegs),1]<-ironX0[[1]]+xvals[i]
        new_ironXs[(1+(i-1)*nsegs):(i*nsegs),2]<-ironX0[[2]]+yvals[i]
        new_ironXs[(1+(i-1)*nsegs):(i*nsegs),3]<-ironX0[[3]]+xvals[i]
        new_ironXs[(1+(i-1)*nsegs):(i*nsegs),4]<-ironX0[[4]]+yvals[i]
        new_ironXs[(1+(i-1)*nsegs):(i*nsegs),5]<-ironX0[[6]]+xvals[i]
        new_ironXs[(1+(i-1)*nsegs):(i*nsegs),6]<-ironX0[[7]]+yvals[i]
        new_ironXs[(1+(i-1)*nsegs):(i*nsegs),7]<-rep(i,nsegs) #which
                                        #set of center points
    }
    new_ironXs[,8]<-rep(angles,times=length(xvals)) #angles
                                        #to 0 for each segment
    new_ironXs[,9]<-rep(1:8,times=length(xvals)) #angles
    colnames(new_ironXs)<-c("nx1","ny1","nx2","ny2","ncxs","ncys","fignumber","angles","segnumber")
    new_ironXs
}
                                        #xvals<-c(0,50,100)
                                        #yvals<-c(0,50,100)
                                        #ironXs<-move_ironXs(ironX0,xvals,yvals,8)
                                        #plot(0,0,xlim=c(0,200),ylim=c(0,200),type="n")  ## visualization
                                        #points(ironXs[,1:2])
                                        #points(ironXs[,3:4])

