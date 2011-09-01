### Analyze ironX and DISTANCE sampling
## rm(list = ls())
setwd("/Users/solson/Rprojects/recce1/")   ##source("6-simsmerge.r")
load(file="/Users/solson/Rprojects/recce1/out-6.Rdata")
library("DSpat")
library("gplots")
library("epicalc")
library("lattice")
library("Hmisc")

## Organize data
hir<-data.frame(cbind(rbind(hk3[[2]],hk5[[2]],hk10[[2]],hk20[[2]],hk100[[2]]),rep(c(2.5,5,10,20,100),each=15)))
colnames(hir)[9]<-"kvalues"
hit<-data.frame(cbind(rbind(hk3[[5]],hk5[[5]],hk10[[5]],hk20[[5]],hk100[[5]]),rep(c(2.5,5,10,20,100),each=15)))
colnames(hit)[9]<-"kvalues"
hd<-data.frame(cbind(rbind(hk3[[8]],hk5[[8]],hk10[[8]],hk20[[8]],hk100[[8]]),rep(c(2.5,5,10,20,100),each=15)))
colnames(hd)[9]<-"kvalues"
mir<-data.frame(cbind(rbind(mk3[[2]],mk5[[2]],mk10[[2]],mk20[[2]],mk100[[2]]),rep(c(2.5,5,10,20,100),each=15)))
colnames(mir)[9]<-"kvalues"
mit<-data.frame(cbind(rbind(mk3[[5]],mk5[[5]],mk10[[5]],mk20[[5]],mk100[[5]]),rep(c(2.5,5,10,20,100),each=15)))
colnames(mit)[9]<-"kvalues"
md<-data.frame(cbind(rbind(mk3[[8]],mk5[[8]],mk10[[8]],mk20[[8]],mk100[[8]]),rep(c(2.5,5,10,20,100),each=15)))
colnames(md)[9]<-"kvalues"
lir<-data.frame(cbind(rbind(lk3[[2]],lk5[[2]],lk10[[2]],lk20[[2]],lk100[[2]]),rep(c(2.5,5,10,20,100),each=15)))
colnames(lir)[9]<-"kvalues"
lit<-data.frame(cbind(rbind(lk3[[5]],lk5[[5]],lk10[[5]],lk20[[5]],lk100[[5]]),rep(c(2.5,5,10,20,100),each=15)))
colnames(lit)[9]<-"kvalues"
ld<-data.frame(cbind(rbind(lk3[[8]],lk5[[8]],lk10[[8]],lk20[[8]],lk100[[8]]),rep(c(2.5,5,10,20,100),each=15)))
colnames(ld)[9]<-"kvalues"
allIR<-rbind(hk3[[2]],hk5[[2]],hk10[[2]],hk20[[2]],hk100[[2]],mk3[[2]],mk5[[2]],mk10[[2]],mk20[[2]],mk100[[2]],lk3[[2]],lk5[[2]],lk10[[2]],lk20[[2]],lk100[[2]])
dev<-allIR[,7]-allIR[,4]
type<-rep("IXR",225)
k<-rep(c(2.5,5,10,20,100),each=15, times=3)
IR<-data.frame(cbind(allIR,dev),type,k)
allIT<-rbind(hk3[[5]],hk5[[5]],hk10[[5]],hk20[[5]],hk100[[5]],mk3[[5]],mk5[[5]],mk10[[5]],mk20[[5]],mk100[[5]],lk3[[5]],lk5[[5]],lk10[[5]],lk20[[5]],lk100[[5]])
dev<-allIT[,7]-allIT[,4]
type<-rep("IXT",225)
k<-rep(c(2.5,5,10,20,100),each=15, times=3)
IT<-data.frame(cbind(allIT,dev),type,k)
allD<-rbind(hk3[[8]],hk5[[8]],hk10[[8]],hk20[[8]],hk100[[8]],mk3[[8]],mk5[[8]],mk10[[8]],mk20[[8]],mk100[[8]],lk3[[8]],lk5[[8]],lk10[[8]],lk20[[8]],lk100[[8]])
dev<-allD[,7]-allD[,4]
type<-rep("D",225)
k<-rep(c(2.5,5,10,20,100),each=15, times=3)
D<-data.frame(cbind(allD,dev),type,k)
i1length<-112.1621
all<-rbind(IR,IT,D)
sampleff<-all$samples/all$edays
km <-ifelse(all$type=="D",i1length,16*2)
bias<-all[,9]*-1
all2<-cbind(all,km,bias)
write.csv(file="all.csv",all)
write.csv(file="all2.csv",all2)

## Visually check runs

## Plot the mixed samples
plot(1,1,type='n',xlim=c(0,65),ylim=c(0,65))
matplot(t(test[[6]][,c(2,4)]),t(test[[6]][,c(3,5)]),type='l',add=TRUE,col="blue",lty=1,lwd=2,cex=0.5)
points(test[[1]],test[[2]],pch=20,cex=0.5)
points(test[[10]][,2:3],cex=3,col="blue",pch=19)
##targeted IronXn green


mapall<-function(run,rep){
    plot(c(0,65),c(0,65),type="n",xlab="km",ylab="km")
    points(run[[1]][,2:3],pch=19,cex=0.005)
    points(run[[4]][[rep]][,5:6],pch=19,col="#0000CD",cex=0.5)
    ##sampled points
    matplot(t(run[[3]][[rep]][,c(1,3)]),t(run[[3]][[rep]][,c(2,4)]),type='l',add=TRUE,col="#0000CD",lty=2,cex=0.5) #random IronX blue
    points(run[[7]][[rep]][,5:6],pch=19,col="#006400",cex=0.5)
    ##sampled points
    matplot(t(run[[6]][[rep]][,c(1,3)]),t(run[[6]][[rep]][,c(2,4)]),type='l',add=TRUE,col="#006400",lty=2,cex=0.5) #targeted IronXn green
    plot(run[[9]][[rep]] $lines,lty=1.5, add=TRUE,col="#000000")
    points(run[[10]][[rep]][,2:3],pch=19,col="#000000",cex=0.5)
}


maprun<-function(run,rep){
    plot(c(0,65),c(0,65),type="n",xlab="",ylab="")
    points(run[[1]][,2:3],pch=19,cex=0.005,col="black")
}

mapIXR<-function(run,rep){
    matplot(t(run[[3]][[rep]][,c(1,3)]),t(run[[3]][[rep]][,c(2,4)]),type='l',add=TRUE,col="#0000CD",lty=2,lwd=2,cex=0.5) ##random IronX blue
    points(run[[4]][[rep]][,5:6],pch=19,col="#0000CD",cex=1) ##sampled points
}

mapIXT<-function(run,rep){
    matplot(t(run[[6]][[rep]][,c(1,3)]),t(run[[6]][[rep]][,c(2,4)]),type='l',add=TRUE,col="#006400",lty=2,lwd=2,cex=0.5) #targeted IronXn green
    points(run[[7]][[rep]][,5:6],pch=19,col="#006400",cex=1) ##sampled points
}

mapD<-function (run,rep){
    plot(run[[9]][[rep]]$lines,lty=1.5,lwd=2, add=TRUE,col="#000000")
    points(run[[10]][[rep]][,2:3],pch=19,col="#000000",cex=1)
}


pdf(file="6-landscapesblack.pdf",height=6,width=10)
par(mfrow=c(3,5),mar=c(1,1,1,1),xaxt="n",yaxt="n")
maprun(hk3,1)
maprun(hk5,1)
maprun(hk10,1)
maprun(hk20,1)
maprun(hk100,1)
maprun(mk3,1)
maprun(mk5,1)
maprun(mk10,1)
maprun(mk20,1)
maprun(mk100,1)
maprun(lk3,1)
maprun(lk5,1)
maprun(lk10,1)
maprun(lk20,1)
maprun(lk100,1)
dev.off()


pdf(file="6-exsamplingmk5.pdf",height=3,width=9)
par(mfrow=c(1,3),mar=c(2,2,1,1))
maprun(mk5,1)
mapIXR(mk5,1)
maprun(mk5,1)
mapIXT(mk5,1)
maprun(mk5,1)
mapD(mk5,1)
dev.off()

par(reset=TRUE)
##Density Figure


##part A density =2.16 ind/km2
pdf(file="6-densityboxplots.pdf")
par(mfrow=c(3,1))
boxplot(egord~kvalues,data=hir,boxwex=0.1,at=(1:5-0.2),
        col="#0000CD",main="Simulation of 2.16 ind km-2 gorilla density",
        xlab="K value",
        ylab="Inds km-2",ylim=c(0,10),names=c("","","","",""))
boxplot(egord~kvalues,data=hit, add=TRUE,boxwex=0.1, at=1:5,
        col="#006400")
boxplot(egord~kvalues,data=hd, add=TRUE,boxwex=0.1, at=1:5+0.2,
        col="#606060",names=c("","","","",""))
abline(h=hir[1,7],lty=3)
legend(4.5, 9, c("Random IronX", "Targeted IronX","Distance"),
       fill = c("#0000CD", "#006400","#606060"))
##part B density = 1.08 ind/km2
boxplot(egord~kvalues,data=mir,boxwex=0.1,at=(1:5-0.2),
        col="#0000CD",main="Simulation of 1.08 ind km-2 gorilla density",
        xlab="K value",
        ylab="Inds km-2",ylim=c(0,10),names=c("","","","",""))
boxplot(egord~kvalues,data=mit, add=TRUE,boxwex=0.1, at=1:5,
        col="#006400")
boxplot(egord~kvalues,data=md, add=TRUE,boxwex=0.1, at=1:5+0.2,
        col="#606060",names=c("","","","",""))
abline(h=mir[1,7],lty=3)
legend(4.5, 9, c("Random IronX", "Targeted IronX","Distance"),
       fill = c("#0000CD", "#006400","#606060"))
##part C density = 0.54 ind/km2
boxplot(egord~kvalues,data=lir,boxwex=0.1,at=(1:5-0.2),
        col="#0000CD",main="Simulation of 0.54 ind km-2 gorilla density",
        xlab="K value",
        ylab="Inds km-2",ylim=c(0,10),names=c("","","","",""))
boxplot(egord~kvalues,data=lit, add=TRUE,boxwex=0.1, at=1:5,
        col="#006400")
boxplot(egord~kvalues,data=ld, add=TRUE,boxwex=0.1, at=1:5+0.2,
        col="#606060",names=c("","","","",""))
abline(h=lir[1,7],lty=3)
legend(4.5, 9, c("Random IronX", "Targeted IronX","Distance"),
       fill = c("#0000CD", "#006400","#606060"))
dev.off()


## Deviance by density for each sampling type
pdf(file="6-devboxes.pdf")
par(mfrow=c(2,3))
boxplot(dev~round(degord,2),data=all[all$type=="IXR",],
        xlab="Density ind -km2",
        ylab="Deviance",
        main="Random IronX Deviance")
abline(h=0,lty=3)
boxplot(dev~round(degord,2),data=all[all$type=="IXT",],
        xlab="Density ind -km2",
        ylab="Deviance",
        main="Targeted IronX Deviance")
abline(h=0,lty=3)
boxplot(dev~round(degord,2),data=all[all$type=="D",],
        xlab="Density ind -km2",
        ylab="Deviance",
        main="Distance Transect Deviance")
abline(h=0,lty=3)
## Deviance by clustering for each sampling type
boxplot(dev~k,data=all[all$type=="IXR",],
        xlab="K value",
        ylab="Deviance",
        main="Random IronX Deviance")
abline(h=0,lty=3)
boxplot(dev~k,data=all[all$type=="IXT",],
        xlab="K value",
        ylab="Deviance",
        main="Targeted IronX Deviance")
abline(h=0,lty=3)
boxplot(dev~k,data=all[all$type=="D",],
        xlab="K value",
        ylab="Deviance",
        main="Distance Transect Deviance")
abline(h=0,lty=3)
dev.off()

## No. of samples  by density and sampling type
quartz()
par(mfrow=c(2,3))
boxplot(samples~round(degord,2),data=all[all$type=="IXR",],
        xlab="Density ind -km2",
        ylab="No. samples",
        main="Random IronX Samples")
abline(h=0,lty=3)
boxplot(samples~round(degord,2),data=all[all$type=="IXT",],
        xlab="Density ind -km2",
        ylab="No. samples",
        main="Targeted IronX Samples")
abline(h=0,lty=3)
boxplot(samples~round(degord,2),data=all[all$type=="D",],
        xlab="Density ind -km2",
        ylab="No. samples",
        main="Distance Transect Samples")
abline(h=0,lty=3)
## No. of samples  by density and sampling type
boxplot(samples~k,data=all[all$type=="IXR",],
        xlab="K value",
        ylab="No. samples",
        main="Random IronX Samples")
abline(h=0,lty=3)
boxplot(samples~k,data=all[all$type=="IXT",],
        xlab="K value",
        ylab="No. samples",
        main="Targeted IronX Samples")
abline(h=0,lty=3)
boxplot(samples~k,data=all[all$type=="D",],
        xlab="K value",
        ylab="No. samples",
        main="Distance Transect Samples")
abline(h=0,lty=3)

## No. of samples  by density and sampling type adjusted for area
pdf(file="6-barplotadjarea.pdf")
boxplot(samples/i1length~k,data=all[all$type=="IXR",],
        xlab="K value",
        ylab="No. samples -km",
        main="Random IronX Samples")
abline(h=0,lty=3)
boxplot(samples/i1length~k,data=all[all$type=="IXT",],
        xlab="K value",
        ylab="No. samples -km",
        main="Targeted IronX Samples")
abline(h=0,lty=3)
boxplot(samples/32~k,data=all[all$type=="D",],
        xlab="K value",
        ylab="No. samples -km",
        main="Distance Transect Samples")
abline(h=0,lty=3)
##tapply(all$samples,list(all$type,all$k),sd)
##tapply(all$samples,list(all$type,all$k),mean)
dev.off()

## Barplots with 1 sd confidence intervals for no. of samples and
## sampling efficiency by clustering and sampling type
## par(mfrow=c(2,1))
pdf(file="6-barplotssamples.pdf")
barplot2(tapply(all$samples,list(all$type,all$k),mean),beside=TRUE,
         col= c("lightblue","mistyrose","lightcyan"),
         legend=(c("Random IronX","Targeted IronX","Distance")),
         ylim=c(0,100),
         plot.ci=TRUE,
         ci.l=-1*(tapply(all$samples,list(all$type,all$k),sd)) +tapply(all$samples,list(all$type,all$k),mean),
         ci.u=1*(tapply(all$samples,list(all$type,all$k),sd)) +tapply(all$samples,list(all$type,all$k),mean),
         cex.names=1,plot.grid=TRUE,col.sub="gray20"
         )
mtext("Clustering (k)",1,cex=1.5,line=2.5)
mtext("No. of samples",2,cex=1.5,line=2.5)
mtext("No. of samples",3,cex=1.5,line=1.5)
mtext("[bars indicate  +/- 1 sd]",3,line=-1.5)
barplot2(tapply(sampleff,list(all$type,all$k),mean),beside=TRUE,
         col= c("lightblue","mistyrose","lightcyan"),
         legend=(c("Random IronX","Targeted IronX","Distance")),
         ylim=c(0,4),
         plot.ci=TRUE, ci.l=-1*(tapply(sampleff,list(all$type,all$k),sd))+tapply(sampleff,list(all$type,all$k),mean),        ci.u=1*(tapply(sampleff,list(all$type,all$k),sd))+tapply(sampleff,list(all$type,all$k),mean),
         cex.names=1,plot.grid=TRUE,col.sub="gray20"
         )
mtext("Clustering (k)",1,cex=1.5,line=2.5)
mtext("Sampling efficiency -day",2,cex=1.5,line=2.5)
mtext("Sampling Efficiency",3,cex=1.5,line=1.5)
mtext("[bars indicate  +/- 1 sd]",3,line=-1.5)
barplot2(tapply(all2$samples/all2$km,list(all2$type,all2$k),mean),beside=TRUE,
         col= c("lightblue","mistyrose","lightcyan"),
         legend=(c("Random IronX","Targeted IronX","Distance")),
         ylim=c(0,4),
         plot.ci=TRUE, ci.l=-1*(tapply(all2$samples/all2$km,list(all2$type,all2$k),sd))+tapply(all2$samples/all2$km,list(all2$type,all2$k),mean),        ci.u=1*(tapply(all2$samples/all2$km,list(all2$type,all2$k),sd))+tapply(all2$samples/all2$km,list(all2$type,all2$k),mean),
         cex.names=1,plot.grid=TRUE,col.sub="gray20"
         )
mtext("Clustering (k)",1,cex=1.5,line=2.5)
mtext("Samples -km",2,cex=1.5,line=2.5)
mtext("Samples -km",3,cex=1.5,line=1.5)
mtext("[bars indicate  +/- 1 sd]",3,line=-1.5)
dev.off()

boxplot(spday~type,data=all,
        xlab="Sampling method",
        ylab="No. samples -day",
        main="Sampling -day")



##Samples -km for all densities stratified by type and cluster
pdf(file="6-sampleskm.pdf")
boxplot(samples/km~k,data=all2[all2$type=="IXR",],boxwex=0.1,at=(1:5-0.2),
        col="lightblue",main="Simulation of samples -km",
        xlab="K value",
        ylab="Samples -km",ylim=c(0,5),names=c("","","","",""))
boxplot(samples/km~k,data=all2[all2$type=="IXT",], add=TRUE,boxwex=0.1, at=1:5,
        col="mistyrose")
boxplot(samples/km~k,data=all2[all2$type=="D",], add=TRUE,boxwex=0.1, at=1:5+0.2,
        col="lightcyan",names=c("","","","",""))
legend(4, 5, c("Random IronX", "Targeted IronX","Distance"),
       fill = c("lightblue", "mistyrose","lightcyan"))
dev.off()



## Deviance by density and Kvalues plotted lines for each sampling type
pdf(file="6-devklines.pdf",height=3,width=8)
par(mfrow=c(1,3))
ixrbias<-tapply(all2[all2$type=="IXR",13],list(all2[all2$type=="IXR",7],all2[all2$type=="IXR",11]),mean)
colors<-c("blue","violet","red","orange","green")
plot(c(0.54,1.08,2.167),ixrbias[,1],type='l',col=colors[1], lwd=2,
     ylim=c(-1,5),xlab="Ind km-2",ylab="Bias",lty=1,main="Random IronX")
abline(h=0,col="grey",lwd=2)
lines(c(0.54,1.08,2.167),ixrbias[,2],type='l',col=colors[2],lty=2,lwd=2)
lines(c(0.54,1.08,2.167),ixrbias[,3],type='l',col=colors[3],lty=3,lwd=2)
lines(c(0.54,1.08,2.167),ixrbias[,4],type='l',col=colors[4],lty=4,lwd=2)
lines(c(0.54,1.08,2.167),ixrbias[,5],type='l',col=colors[5],lty=5,lwd=2)
legend(1, 5, c("k=2.5", "k=5","k=10","k=20","k=40"),col=colors,lty=c(1:5),bty='n')
ixtbias<-tapply(all2[all2$type=="IXT",13],list(all2[all2$type=="IXT",7],all2[all2$type=="IXT",11]),mean)
plot(c(0.54,1.08,2.167),ixtbias[,1],type='l',col=colors[1], lwd=2,
     ylim=c(-1,5),xlab="Ind km-2",ylab="Bias",lty=1,main="Targeted IronX")
abline(h=0,col="grey",lwd=2)
lines(c(0.54,1.08,2.167),ixtbias[,2],type='l',col=colors[2],lty=2,lwd=2)
lines(c(0.54,1.08,2.167),ixtbias[,3],type='l',col=colors[3],lty=3,lwd=2)
lines(c(0.54,1.08,2.167),ixtbias[,4],type='l',col=colors[4],lty=4,lwd=2)
lines(c(0.54,1.08,2.167),ixtbias[,5],type='l',col=colors[5],lty=5,lwd=2)
                                        #legend(1.75, 5, c("k=2.5", "k=5","k=10","k=20","k=40"),col=colors,lty=c(1:5),bty='n')
dbias<-tapply(all2[all2$type=="D",13],list(all2[all2$type=="D",7],all2[all2$type=="D",11]),mean)
colors<-c("blue","violet","red","orange","green")
plot(c(0.54,1.08,2.167),dbias[,1],type='l',col=colors[1], lwd=2,
     ylim=c(-1,5),xlab="Ind km-2",ylab="Bias",lty=1,main="Distance transects")
abline(h=0,col="grey",lwd=2)
lines(c(0.54,1.08,2.167),dbias[,2],type='l',col=colors[2],lty=2,lwd=2)
lines(c(0.54,1.08,2.167),dbias[,3],type='l',col=colors[3],lty=3,lwd=2)
lines(c(0.54,1.08,2.167),dbias[,4],type='l',col=colors[4],lty=4,lwd=2)
lines(c(0.54,1.08,2.167),dbias[,5],type='l',col=colors[5],lty=5,lwd=2)
legend(1, 5, c("k=2.5", "k=5","k=10","k=20","k=40"),col=colors,lty=c(1:5),bty='n')
dev.off()




boxplot(dev~round(degord,2),data=all[all$type=="IXT",],
        xlab="Density ind -km2",
        ylab="Deviance",
        main="Targeted IronX Deviance")
abline(h=0,lty=3)
boxplot(dev~round(degord,2),data=all[all$type=="D",],
        xlab="Density ind -km2",
        ylab="Deviance",
        main="Distance Transect Deviance")
abline(h=0,lty=3)


devall<-tapply(all2$dev,list(all2$k,all2$degor),mean)

allIXR<-all2[all2$type=="IXR",]
allIXT <- all2[all2$type=="IXT",]
allD <- all2[all2$type=="D",]
biascolors<-c(cm.colors(10)[c(9,7)],"lightblue",heat.colors(10)[8:4])

pdf(file="6-deviancecolorplots.pdf")
levelplot(tapply(allIXR$bias,list(allIXR$k,round(allIXR$degor,2)),median),
          scales=list(cex=0.7), aspect="iso",
          col.regions=biascolors,
          colorkey=TRUE, at=seq(-2.5,5.5,by=1),
          pretty=F,xlab='K level',ylab='Ind -km2',main="Median bias for IXR")
levelplot(tapply(allIXT$bias,list(allIXT$k,round(allIXT$degor,2)),median),
          scales=list(cex=0.7), aspect="iso",
          col.regions=biascolors,
          colorkey=TRUE, at=seq(-2.5,5.5,by=1),
          pretty=F,xlab='K level',ylab='Ind -km2',main="Median bias for IXT")
levelplot(tapply(allD$bias,list(allD$k,round(allD$degor,2)),median),
          scales=list(cex=0.7), aspect="iso",
          col.regions=biascolors,
          colorkey=TRUE, at=seq(-2.5,5.5,by=1),
          pretty=F,xlab='K level',ylab='Ind -km2',main="Median bias for D")
dev.off()

biascolors<-c(cm.colors(30)[30:22],"lightblue","lightblue",heat.colors(30)[21:1])

pdf(file="6-deviancecolorplots2.pdf")
levelplot(tapply(allIXR$bias,list(allIXR$k,round(allIXR$degor,2)),median),
          scales=list(cex=0.7), aspect="iso",
          col.regions=biascolors,
          colorkey=TRUE, at=seq(-2.5,5.5,by=.25),
          pretty=F,xlab='K level',ylab='Ind -km2',main="Median bias for IXR")

levelplot(tapply(allIXT$bias,list(allIXT$k,round(allIXT$degor,2)),median),
          scales=list(cex=0.7), aspect="iso",
          col.regions=biascolors,
          colorkey=TRUE, at=seq(-2.5,5.5,by=.25),
          pretty=F,xlab='K level',ylab='Ind -km2',main="Median bias for IXT")
levelplot(tapply(allD$bias,list(allD$k,round(allD$degor,2)),median),
          scales=list(cex=0.7), aspect="iso",
          col.regions=biascolors,
          colorkey=TRUE, at=seq(-2.5,5.5,by=.25),
          pretty=F,xlab='K level',ylab='Ind -km2',main="Median bias for D")
dev.off()





pdf(file="6-densityboxplots.pdf")
par(mfrow=c(3,1))
boxplot(egord~kvalues,data=hir,boxwex=0.1,at=(1:5-0.2),
        col="#0000CD",main="Simulation of 2.16 ind km-2 gorilla density",
        xlab="K value",
        ylab="Inds km-2",ylim=c(0,10),names=c("","","","",""))
boxplot(egord~kvalues,data=hit, add=TRUE,boxwex=0.1, at=1:5,
        col="#006400")
boxplot(egord~kvalues,data=hd, add=TRUE,boxwex=0.1, at=1:5+0.2,
        col="#606060",names=c("","","","",""))
abline(h=hir[1,7],lty=3)
legend(4.5, 9, c("Random IronX", "Targeted IronX","Distance"),
       fill = c("#0000CD", "#006400","#606060"))


pdf(file="6-densityboxplot2.pdf")
boxplot(egord~kvalues,data=hir,boxwex=0.1,at=(1:5-0.2),
        col="lightblue",ylim=c(0,10),names=c("","","","",""))
boxplot(egord~kvalues,data=hit, add=TRUE,boxwex=0.1, at=1:5,
        col="mistyrose")
boxplot(egord~kvalues,data=hd, add=TRUE,boxwex=0.1, at=1:5+0.2,
        col="lightcyan",names=c("","","","",""))
abline(h=hir[1,7],lty=3)
legend(4, 10, c("Random IronX", "Targeted IronX","Distance"),
       fill = c("lightblue", "mistyrose","lightcyan"))
mtext("Clustering radius (km)",1,cex=1.5,line=2.5)
mtext("Gorilla density (Ind km-2)",2,cex=1.5,line=2.5)
mtext("Gorilla density estimates at 2.16 ind km-2",3,cex=1.5,line=1.5)
dev.off()






## Barplots with 1 sd confidence intervals for no. of samples and
## sampling efficiency by clustering and sampling type
## par(mfrow=c(2,1))
pdf(file="6-barplotadjarea2.pdf")
barplot2(tapply(all2$samples/all2$km,list(all2$type,all2$k),mean),beside=TRUE,
         col= c("lightblue","mistyrose","lightcyan"),
         legend=(c("Random IronX","Targeted IronX","Distance")),
         ylim=c(0,3),
         plot.ci=TRUE,
         ci.l=-1*(tapply(all2$samples/all2$km,list(all2$type,all2$k),sd)) +tapply(all2$samples/all2$km,list(all2$type,all2$k),mean),
         ci.u=1*(tapply(all2$samples/all2$km,list(all2$type,all2$k),sd)) +tapply(all2$samples/all2$km,list(all2$type,all2$k),mean),
         cex.names=1,plot.grid=TRUE,col.sub="gray20"
         )
mtext("Clustering (k)",1,cex=1.5,line=2.5)
mtext("No. of samples/km",2,cex=1.5,line=2.5)
mtext("No. of samples/km",3,cex=1.5,line=1.5)
mtext("[bars indicate  +/- 1 sd]",3,line=-1.5)
dev.off()
