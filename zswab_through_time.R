library(ggplot2)
library(plyr) 
library(dplyr)

setwd("/Users/Sofia/Dropbox/SFSU/Ensatina Museum Project/back to Bd/lab trial/")
zswab<-read.csv("simplified_eex_zswab.csv",header=T)
head(zswab)
#zswab<-as.matrix(zswab,row.names=T)
head(zswab)
pos<-zswab[zswab$treatment=="Bd.pos",]
ctrl<-zswab[zswab$treatment=="control",]
head(pos)

ztran<-read.csv("simplified_eex_zswab_transposed.csv",header=T)
head(ztran)

plot(ztran$ID,ztran$EEX01,type="l")
points(ztran$ID,ztran$EEX02)

plot.new()
plot.window(xlim=c(0,11),ylim=c(0,200000))

for (i in 3:33) (
  points(ztran$ID,ztran[,i]) +
    lines(ztran$ID,ztran[,i])+
  points(ztran[])
  
)


pos<-read.csv("bd_pos.csv",header=T)
ctrl_full<-read.csv("bd_ctrl.csv",header=T)
ctrl<-ctrl_full[1:10]



##log transforming data
time<-c(0:10)
logpos<-log(pos[,2:20]+1)
logpos<-cbind(time,logpos)
logctrl<-log(ctrl[,2:10]+1)
logctrl<-cbind(time,logctrl)
head(logctrl)

##plotting log-transformed data

plot.new()
plot.window(xlim=c(0,11),ylim=c(0,14))
for (i in 2:20) (
  #points(logpos$time,(logpos[,i]),col="red",pch=20) +
    lines(logpos$time,(logpos[,i]),col="red")
)

for (i in 2:10) (
  #points(logctrl$time,(ctrl[,i]),col="black",pch=20)+
    lines(logctrl$time,(ctrl[,i]),col="black")
)

# ##log transforming the data - plotting
# for (i in 2:20) (
#   points(pos$time,log10(pos[,i]+1),col="red",pch=20) +
#     lines(pos$time,log10(pos[,i]+1),col="red")
# )
# 
# for (i in 2:10) (
#   points(ctrl$time,log10(ctrl[,i]+1),col="black",pch=20)+
#     lines(ctrl$time,log10(ctrl[,i]+1),col="black")
# )

axis(1,pos=0)
axis(2,pos=0)
clip(0,10,0,10)
abline(h=log10(10000),lty=2)

title(main="Zoospore Load through Time",xlab="Week",ylab="Zoospore Load (log scale)")
legend("topright","Treatment Type",c("Bd+","Bd-"),col=c("red","black"),pch=20)

dev.off()


#without log transofrmation
plot.new()
plot.window(xlim=c(-0,11),ylim=c(0,250000))

for (i in 2:20) (
  points(pos$time,pos[,i],col="red") +
    lines(pos$time,pos[,i],col="red")
)


for (i in 2:10) (
  points(ctrl$time,ctrl[,i],col="black")+
    lines(ctrl$time,ctrl[,i],col="black")
)

axis(1,pos=0)
axis(2,pos=0)
abline(h=10000,lty=2)


##plot survival curve
png("survcurv.png")
pdg("survcurv.pdf")
plot.new()
plot.window(xlim=c(0,12),ylim=c(0,1.2))
points(pos$time,pos$prop.surviving,col="red",pch=20)
lines(pos$time,pos$prop.surviving,col="red",type="s")
points(ctrl$time,ctrl$prop.surviving,pch=20)
lines(ctrl$time,ctrl$prop.surviving,type="s",lty=2)
axis(2,pos=0)
axis(1,pos=0)
title(main="Survival Curve",xlab="Week",ylab="Proportion Surviving")
legend("right","Treatment Type",c("Bd+","Bd-"),col=c("red","black"),pch=20)
dev.off()



?legend


##correlate zswab with time to death
dtd<-read.csv("EEX susc trial lab data_dtd_deadonly.csv",head=T)

fit<-lm(dtd$daystodeath~dtd$zswab)
fitlog<-lm(dtd$daystodeath~dtd$log10swab)

fit<-lm(dtd$zswab~dtd$daystodeath)
fitlog<-lm(dtd$log10swab~dtd$daystodeath)

summary.lm(fit)
summary.lm(fitlog)

cor(dtd$zswab,dtd$daystodeath)

png("zswab_daystodeath.png")
pdf("zswab_daystodeath.pdf")
plot(dtd$daystodeath,dtd$log10swab,xlim=rev(range(dtd$daystodeath)),ylab = "Zoospore Load (log10 scale)",xlab="Days to Death",pch=20)
dev.off()

plot(dtd$daystodeath,dtd$zswab)

abline(fitlog)
abline(fit)
