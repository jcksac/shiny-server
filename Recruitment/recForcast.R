### Function for estimating the recruitment rate of a future trial



#nSite <- 20
#rpm <- 1
#openRate <- 2
#maxTime <- 24
#penal <- 0.5
#plot <- TRUE
#detail <- TRUE
#rec.forcast(nSite,rpm,openRate,maxTime)

rec.forcast <- function(nSite,rpm,openRate,maxTime,penal=0.5,plot=TRUE,detail=TRUE,...){ 

## Getting the number of open sites per month
openSite <-seq(1, nSite,by= openRate)

if(max(openSite)!= nSite) openSite <- c(openSite, nSite)

if(length(openSite)<maxTime){
openSite <-c(openSite,rep(nSite,maxTime-length(openSite))) 
} else {
openSite <- openSite[1:maxTime]
warning("Not enough time to open all sites!")
}
 
### Basic average rate per site approach
monthRate<-openSite*rpm
 
## penalisng monthly recruitment (recruits 1/2 as much in first month)
penalty <- diff(c(0, monthRate))*penal
monthRate <- monthRate-penalty

cumRec<-round(cumsum(monthRate)) 
monthRate <- diff(c(0, cumRec))

rec<-data.frame("Monthly Rec"=monthRate,"Cumualtive Rec."= cumRec) 


if(plot) {
	ymax <- max(rec[,2])*1.1
	par(mar=c(5,5,1,1))
	plot(cumRec,typ="l",xlab="Time (Months)",ylab="Cumulative Recruitment",font.lab=3,ylim=c(0,ymax)) 
	
	hline <- seq(0,ymax,by=500)
	if(ymax<2001) hline <- seq(0,ymax,by=250)
	if(ymax<1001) hline <- seq(0,ymax,by=100)
	if(ymax<501) hline <- seq(0,ymax,by=50)
	if(ymax<201) hline <- seq(0,ymax,by=25)
	if(ymax<51) hline <- seq(0,ymax,by=10)

	
	vline <- seq(0,maxTime,by=24)
	if(maxTime < 73) vline <- seq(0,maxTime,by=12)
	if(maxTime < 37) vline <- seq(0,maxTime,by=6)
	if(maxTime < 12) vline <- seq(0,maxTime,by=3)
	
	abline(h=hline,v=vline,lty=2,col="gray")
	
	if(detail){
		det.id <- round(seq(0,nrow(rec),length=11)[-1])
		if(det.id[length(det.id)]!=nrow(rec)) det.id[length(det.id)] <- nrow(rec)
		text(det.id,rec[det.id,2]+ymax/25,rec[det.id,2],col=2,font=2)
	}
}

return(rec) 

} 

