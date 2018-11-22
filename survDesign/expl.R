data$s.ob <- Surv(data$st,data$OS_cen)
data$cut199 <- cut(data$PostOpCA199,c(-Inf,90,Inf))
data$dum <- factor(data$cut199):factor(data$Arm)

plot(survfit(data$s.ob~data$dum),lwd=6,col=c(2,2,4,4),lty=c(1,2))



data[1:3,]

data$H2 <- scale(data$rnaHENT1)*scale(data$Hscore)
data$H2c <- cut(data$H2,c(-Inf,-.4,Inf))
data$dum <- factor(data$H2c):factor(data$Arm) 

survfit(data$s.ob ~ data$dum)
plot(survfit(data$s.ob ~ data$dum),lwd=4,col=c(2,2,4,4),lty=c(1,2))
coxph(data$s.ob~ LymphN + ResecM + data$H2c/data$Arm,data=data)



data[1:3,]
names(data)


bio <- data[,c(23:29)]


### Looking at differences

bio[1:3,]

bio[,6] <- log(bio[,6]+.5)
bio[,7] <- log(bio[,7]+.5)
diff <- NULL
id <- NULL
for(i in 1:6){
	for(j in (i+1):7){
		diff <- cbind(diff,bio[,j]-bio[,i])
		id <- c(id,paste(i,j))
	}
}

hist(diff[,4])
miss.id <- unique(which(is.na(diff),arr.ind=T)[,1])
diff <- diff[-miss.id,]

hr <- NULL
for(i in 1:ncol(diff)){
	hr <- cbind(hr,coef(coxph(data$s.ob[-miss.id]~data$Arm[-miss.id]/diff[,i])))
}
hr



distDiff <- dist(scale(diff))
hca <- hclust(distDiff,method="ward.D")

survfit(data$s.ob[-miss.id]~cutree(hca,k=3))
coxph(data$s.ob[-miss.id]~factor(cutree(hca,k=3)))



plot(hr[2,-4],hr[3,-4])
identify(hr[2,-4],hr[3,-4])
abline(a=0,b=1)

plot(abs(hr))

which(abs(hr)>0.15)
hist(diff[,13])
c <- cut(diff[,13],c(-Inf,-0.5,Inf),labels=c("Low","High"))
ca <- factor(c):factor(data$Arm[-miss.id])
survfit(data$s.ob[-miss.id]~ca)
plot(survfit(data$s.ob[-miss.id]~ca),col=c(2,2,4,4),lty=c(1,2),xlim=c(0,60),lwd=4)
cm <- coxph(data$s.ob[-miss.id]~data$LymphN[-miss.id] +data$Diff_status[-miss.id] + data$Arm[-miss.id]*c)

anova(cm)




c <- cut(diff[,19],c(-Inf,3,Inf))
ca <- factor(c):factor(data$Arm[-miss.id])
survfit(data$s.ob[-miss.id]~ca)
plot(survfit(data$s.ob[-miss.id]~ca),col=c(2,2,4,4),lty=c(1,2))
coxph(data$s.ob[-miss.id]~c*data$Arm[-miss.id])



hcf <- function(x){
	hclust(x,method="ward.D")
	
}

hm <- heatmap(as.matrix(scale(diff)),hclustfun=hcf)
cutree(hm,k=3)

