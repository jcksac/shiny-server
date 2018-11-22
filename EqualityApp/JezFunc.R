
jez <- function(nAgent,nTrans,maxTV,alpha,pow,trace=5){

	### Creating 1000 agents
	agent <- c(1:nAgent)
	gdp <- rep(0,nAgent)
	i<-1
	
	
	if(trace){
		res <- matrix(NA,nAgent,nTrans)
	}
	
	col.count <- 1
	col.id <- seq(0,nTrans,by=trace)
	res <- matrix(NA,nAgent,length(col.id))
	res[,1] <- 0
	
	for(i in 1:nTrans){
	
		###### Simulation
		sim.ag <- sample(agent,2)
		
		### weighting	
		rat <- (gdp[sim.ag[2]]+5)/(gdp[sim.ag[1]]+5)
		rat <- (rat^pow);rat
	
		### Simulating transaction value	
		beta <- alpha/rat
		sim.tv <- maxTV*rbeta(1,alpha,beta)
			
		## Standard Approach
		gdp[sim.ag[1]] <- gdp[sim.ag[1]] + maxTV-sim.tv
		gdp[sim.ag[2]] <- gdp[sim.ag[2]] + sim.tv
	
	
		if(i%in%col.id) {
			col.count <- col.count+1	
			res[,col.count] <- gdp
			}
	
	}
	
	#res <- cbind(0,res)
	ret <- gdp
	if(trace) ret <- res
	ret
	
}



mobPlot <- function(gdp,pos){
	
	par(bg="lightgray")
	rank.co <- rep(NA,pos)
	rank.co[1] <- 0
	for(i in 2:pos){
		
		rank.co[i] <- sqrt(sum((rank(gdp[,i]) - rank(gdp[,(i-1)]))^2)/nrow(gdp))
		
	}
	
	plot(1:length(rank.co),rank.co,typ="o",col="royalblue",lwd=6,ylab="",xlab="Number of Transactions",xaxt="n")
	axis(side=1,c(0:pos),c(0:pos)*500)
	mtext(side=2,padj=-4,adj=0.8,"Greater mobility ->")
	mtext(side=2,padj=-4,adj=0.2,"<- Lesser mobility")

}
