#############################################
### This file generates the Figure on the Cover
### Mean Log Wage as a Function of Education
#############################################
### Uses data file cps09mar.txt
#########################################

dat <- read.table("cps09mar.txt")
lnwage <- as.matrix(log(dat[,5]/(dat[,6]*dat[,7])))
wm <- (dat[,11]==1)&(dat[,2]==0)
wf <- (dat[,11]==1)&(dat[,2]==1)

# take log hourly wage for each group
wm_lnwage <- lnwage[wm]
wf_lnwage <- lnwage[wf]
wm_edu <- dat[wm,4]
wf_edu <- dat[wf,4]

edu_dot <- c(4,6,8,9,10,11,12,13,14,16,18,20)

################################################
###  Conditional Mean  
################################################

wm_mean <- vector()
wf_mean <- vector()

for (i in 1:length(edu_dot)){
  wm_mean[i] <- mean(wm_lnwage[wm_edu == edu_dot[i]])
  wf_mean[i] <- mean(wf_lnwage[wf_edu == edu_dot[i]])
}

xL <- 3.4
yL <- 1.95

pdf("HANSEN-cover.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(edu_dot,wm_mean,type="p",cex=.8,pch=19,xlim=c(4,20),ylim=c(2,4),
     xlab="",ylab="",yaxt="n",xaxt="n",bty="n",cex.lab=.75)
points(edu_dot,wf_mean,cex=.8, pch=17)
lines(edu_dot,wm_mean)
lines(edu_dot,wf_mean)
lines(c(xL,xL),c(yL,4))
lines(c(xL,22),c(yL,yL))
dev.off()

postscript("HANSEN-cover.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(edu_dot,wm_mean,type="p",cex=.8,pch=19,xlim=c(4,20),ylim=c(2,4),
     xlab="",ylab="",yaxt="n",xaxt="n",bty="n",cex.lab=.75)
points(edu_dot,wf_mean,cex=.8, pch=17)
lines(edu_dot,wm_mean)
lines(edu_dot,wf_mean)
lines(c(xL,xL),c(yL,4))
lines(c(xL,22),c(yL,yL))
dev.off()