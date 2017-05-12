library(survival)

setwd("Masters Thesis/bd/")
aml

plot(aml$time,aml$status)

with(aml,plot(time,type="h"))
aml.survfit=survfit(Surv(time,status==1)~1, data=aml)
plot(aml.survfit,xlab="time",ylab="prop surviving",conf.int=FALSE)

aml

sals<-read.csv("survival_over_time.csv",head=T)
head(sals)

pos<-subset(sals[which(sals$treatment=="Bd.pos"),])
ctr<-subset(sals[which(sals$treatment=="control"),])
poswk<-colSums(pos[3:13])
ctrwk<-colSums(ctr[3:13])
plot(ctrwk)
plot(poswk)
time<-c(0:10)
time

require(reshape2)

possurv<-as.data.frame(possurv<-cbind(time,poswk))
ctrsurv<-as.data.frame(ctrsurv<-cbind(time,ctrwk))

pos.survfit<-survfit(Surv(time,poswk>0)~1,data=possurv)
plot(pos.survfit,xlab = "time",ylab="prop")

plot(poswk,type="s")
lines(ctrwk,type="s")


sals2<-read.csv("simplified_eex_zswab_transposed.csv")



###TAKE TWO
#http://www.sthda.com/english/wiki/survival-analysis-basics

survdata<-read.csv("for_surv_analysis.csv",head=T)
survdata<-survdata[which(survdata$Bd.treatment!=""),]
head(survdata)

fit<-survfit(Surv(time,status)~Bd.treatment,data=survdata)
print(fit)

surv_bd<-survdiff(Surv(time,status)~Bd.treatment,data=survdata)
surv_bd

surv_age<-survdiff(Surv(time,status)~age,data=survdata)
surv_age

pos<-survdata[which(survdata$Bd.treatment=="+"),]
surv_age_pos<-survdiff(Surv(time,status)~age,data=pos)
surv_age_pos


#summary of results:
# Bd positive had significantly lower probability of survival
# juveniles and adults had similar probabilities
# all Bd positives reached 10K+ at some point - only three were able to recover
# one control reached high levels, but quickly fell back down - could this be an error?
