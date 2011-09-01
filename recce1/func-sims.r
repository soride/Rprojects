
###################Simulations####

sims<-function(nruns,sampledf,nironXs,ntransects,ironX0,w,yp10,ym10,yp8,ym8,i1area,darea)
{
    rsimIXR<-matrix(nrow=nruns,ncol=8)
    rsimIXT<-matrix(nrow=nruns,ncol=8)
    rsimD<-matrix(nrow=nruns,ncol=8)
    colnames(rsimIXR) <- colnames(rsimIXT) <- colnames(rsimD) <- c("run","samples","edays","egord","xseed","yseed","degord","nobjects")
    initR<-list() #starting lists
    initT <- list()
    initD <- list()
    mapIR <- list() #capture spatial location of ironX/transects
    mapIT <- list()
    mapD <- list()
    sampIR <- list () # capture samples
    sampIT <- list ()
    sampD <- list ()
    initR[[1]] <- initT[[1]] <- initD[[1]] <- sampledf #samples
    initR[[2]] <- initT[[2]] <- ironX0  #input pattern df
    initR[[3]] <- initT[[3]] <- nironXs  #count of ironXs on the landscape
    initR[[4]] <- initT[[4]] <- w  #half width of sine walk on ironX
    for (i in 1:nruns){
##################### 110.61 km x 1 RANDOM IronX
        rsimIXR[i,1] <- i
        ir1  <- simIXRstokes(initR)
        rsimIXR [i,2] <- nrow(ir1[[8]]) + nrow(ir1[[10]]) ##isamples
        rsimIXR [i,3] <- 1 * 110.61 * .25 ##iedays
        rsimIXR [i,4] <- (rsimIXR[i,2]/60)/i1area   ##iegord
        rsimIXR [i,5] <- ir1[[1]]  # ironX center point x seed(s)
        rsimIXR [i,6] <- ir1[[2]]  # ironX center point y seed(s)
        rsimIXR [i,7] <- (nrow(sampledf)*(1/60)*(1/65^2)) # real
                                        # density
        rsimIXR [i,8] <- nironXs
        mapIR   [[i]] <- ir1[[3]]
        sampIR  [[i]] <- rbind(ir1[[8]],ir1[[10]])
        print(paste("IronXR_",i,sep=""))
##################### 110.61 km x 1 TARGETED IronX,
        rsimIXT [i,1] <- i
        it1  <- simIXTstokes(initT)
        rsimIXT [i,2] <- nrow(it1[[8]]) + nrow(it1[[10]]) ##isamples
        rsimIXT [i,3] <- 1 * 110.61 * .25 ##iedays
        rsimIXT [i,4] <- (rsimIXT[i,2]/60)/i1area   ##iegord
        rsimIXT [i,5] <- it1[[1]]  # ironX center point x seed(s)
        rsimIXT [i,6] <- it1[[2]]  # ironX center point y seed(s)
        rsimIXT [i,7] <- (nrow(sampledf)*(1/60)*(1/65^2)) # real
                                        # density
        rsimIXT [i,8] <- nironXs
        mapIT   [[i]] <-  it1[[3]]
        sampIT  [[i]] <-  rbind(it1[[8]],it1[[10]])
        print(paste("IronXT_",i,sep=""))
##################### 2km x 16 DISTANCE transects ala Stokes 2010
        rsimD [i,1] <- i
        xseed<-sample(seq(0,9,0.001),1)  #set random start for transects
        study.area=owin(xrange=c(xseed,xseed+2),yrange=c(0,65)) #one vertical set of transects
        initD[[2]]<- create.lines(study.area,nlines=4,width=.002,spacing=100,angle=180)
        dr1 <-distancesim(initD)
        rsimD [i,2] <- nrow(dr1[[10]])
        rsimD [i,3] <- 12*2*(4/3) ## estimated number of days to
        ## sample
        rsimD [i,4] <- (rsimD[i,2]/60)/darea
        rsimD [i,5] <- xseed # seed X for transects initD[[2]][1,2]
        rsimD [i,6] <- initD[[2]][1,4]  # seed y for transects
        rsimD [i,7] <-  (nrow(sampledf)*(1/60)*(1/65^2)) # real
                                        # density
        rsimD [i,8] <- ntransects
        mapD  [[i]] <- dr1[[4]]
        sampD [[i]] <- dr1[[10]]
        print(paste("Distance_",i,sep=""))
    }
    out<-list(sampledf,rsimIXR,mapIR,sampIR,rsimIXT,mapIT,sampIT,rsimD,mapD,sampD)
    return (out)
}




