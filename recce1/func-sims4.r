
###################Simulations####

sims4<-function(nruns,sampledf,nironXs,ntransects,ironX0,w,yp10,ym10,yp8,ym8,yp14,ym14,y14prime2,i1area,darea,dlength,i1length)
{
    rsimIXR<-matrix(nrow=nruns,ncol=11)
    rsimIXT<-matrix(nrow=nruns,ncol=11)
    rsimD<-matrix(nrow=nruns,ncol=11)
    rsimM<-matrix(nrow=nruns,ncol=11)
    colnames(rsimIXR) <- colnames(rsimIXT) <- colnames(rsimD) <-
        colnames(rsimM) <-         c("run","samples","edays","egord","xseed","yseed","degord","nobjects","length","area","method")
    initR<-list() #starting lists
    initT <- list()
    initD <- list()
    initM <- list()
    mapIR <- list() #capture spatial location of ironX/transects
    mapIT <- list()
    mapD <- list()
    mapM <- list ()
    sampIR <- list () # capture samples
    sampIT <- list ()
    sampD <- list ()
    sampM <- list ()
    plotM <- list ()
    initR[[1]] <- initT[[1]] <- initD[[1]] <- initM[[1]] <- sampledf #samples
    initR[[2]] <- initT[[2]] <- ironX0  #input pattern df
    initR[[3]] <- initT[[3]] <- nironXs  #count of ironXs on the landscape
    initR[[4]] <- initT[[4]] <- w  #half width of sine walk on ironX
    for (i in 1:nruns){
##################### 110.61 km x 1 RANDOM IronX
        rsimIXR[i,1] <- i
        ir1  <- simIXRstokes(initR)
        rsimIXR [i,2] <- nrow(ir1[[8]]) + nrow(ir1[[10]]) ##isamples
        rsimIXR [i,3] <- i1length * .25 ##iedays
        rsimIXR [i,4] <- (rsimIXR[i,2]/5)/i1area   ##iegord
        rsimIXR [i,5] <- ir1[[1]]  # ironX center point x seed(s)
        rsimIXR [i,6] <- ir1[[2]]  # ironX center point y seed(s)
        rsimIXR [i,7] <- (nrow(sampledf)*(1/5)*(1/65^2)) # real
                                        # density
        rsimIXR [i,8] <- nironXs
        rsimIXR [i,9] <- i1length
        rsimIXR [i,10] <-  i1area
        rsimIXR [i,11] <- 1 ##rsimIXR
        mapIR   [[i]] <- ir1[[3]]
        sampIR  [[i]] <- rbind(ir1[[8]],ir1[[10]])
        print(paste("IronXR_",i,sep=""))
##################### 110.61 km x 1 TARGETED IronX,
        rsimIXT [i,1] <- i
        it1  <- simIXTstokes(initT)
        rsimIXT [i,2] <- nrow(it1[[8]]) + nrow(it1[[10]]) ##isamples
        rsimIXT [i,3] <- i1length * .25 ##iedays
        rsimIXT [i,4] <- (rsimIXT[i,2]/5)/i1area   ##iegord
        rsimIXT [i,5] <- it1[[1]]  # ironX center point x seed(s)
        rsimIXT [i,6] <- it1[[2]]  # ironX center point y seed(s)
        rsimIXT [i,7] <- (nrow(sampledf)*(1/5)*(1/65^2)) # real
                                        # density
        rsimIXT [i,8] <- nironXs
        rsimIXT [i,9] <- i1length
        rsimIXT [i,10] <-  i1area
        rsimIXT [i,11] <- 2 ##rsimIXT
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
        rsimD [i,3] <- dlength*(4/3) ## estimated number of days to
        ## sample
        rsimD [i,4] <- (rsimD[i,2]/5)/darea
        rsimD [i,5] <- xseed # seed X for transects initD[[2]][1,2]
        rsimD [i,6] <- initD[[2]][1,4]  # seed y for transects
        rsimD [i,7] <-  (nrow(sampledf)*(1/5)*(1/65^2)) # real
                                        # density
        rsimD [i,8] <- ntransects
        rsimD [i,9] <- dlength
        rsimD [i,10] <- darea
        rsimD [i,11] <- 3 ##rsimD
        mapD  [[i]] <- dr1[[4]]
        sampD [[i]] <- dr1[[10]]
        print(paste("Distance_",i,sep=""))
##################### 2km x 16 DISTANCE transects PLUS RECCES!!!
        rsimM [i,1] <- i
        xseed<-sample(seq(0,9,0.001),1)  #set random start for transects
        study.area=owin(xrange=c(xseed,xseed+2),yrange=c(0,65)) #one vertical set of transects
        initM [[2]]<- create.lines(study.area,nlines=4,width=.002,spacing=100,angle=180)
        mr1 <-mixsim(initM)
        lss<-mr1[[4]]
        V=lss$lines$end[2,2]-lss$lines$end[1,2]
        fV=4 #frequency of random walk sine curve
        AV=0.25 #amplitude of random walk in km
        ypV<-function(x) AV*sin(x*fV*pi/V) + w
        ymV<-function(x) AV*sin(x*fV*pi/V) - w
        yVprime2<-function (x) sqrt(1+(AV*(fV*pi/V)*cos(x*fV*pi/V))^2)
        curvelength<-12*(integrate(y14prime2,0,14))[[1]]+3*(integrate(yVprime2,0,V))[[1]]
        hcurvearea<-integrate(yp14,0,14)[[1]] - integrate (ym14,0,14)[[1]] ##area between
        vcurvearea<-integrate(ypV,0,V)[[1]] - integrate (ymV,0,V)[[1]] #area between V
        rsimM [i,2] <- nrow(mr1[[10]])
        rsimM [i,3] <- dlength*(4/3)+curvelength*.25 ## estimated number of days to
        ## sample
        rsimM [i,4] <- (rsimM[i,2]/5)/(darea+hcurvearea+vcurvearea)
        ##estimated density
        rsimM [i,5] <- xseed # seed X for transects initD[[2]][1,2]
        rsimM [i,6] <- initM[[2]][1,4]  # seed y for transects
        rsimM [i,7] <- (nrow(sampledf)*(1/5)*(1/65^2)) # real
                                        # density
        rsimM [i,8] <- ntransects
        rsimM [i,9] <- dlength + curvelength
        rsimM [i,10] <- darea+12*hcurvearea+3*vcurvearea
        rsimM [i,11] <- 4  ##rsimM
        mapM  [[i]] <- mr1[[4]]
        plotM [[i]] <- mr1[[6]]   ##
        sampM [[i]] <- mr1[[10]]  ##
        print(paste("Mixed_",i,sep=""))
    }
    over<-rbind(rsimIXR,rsimIXT,rsimD,rsimM)
    out<-list(sampledf,rsimIXR,mapIR,sampIR,rsimIXT,mapIT,sampIT,rsimD,mapD,sampD,rsimM,mapM,sampM,plotM,over)
    return (out)

}





