### Bens functions



#########################
#########################
demSelect<-function(dat){
nm <- names(dat)
dat <- dat[,which(substr(nm,1,2)=="d.")]
names(dat) <-substr(names(dat),3,nchar(names(dat)))
dat
}

outSelect<-function(dat){
	nm <- names(dat)
	dat <- dat[,which(substr(nm,1,2)=="o.")]
	names(dat) <-substr(names(dat),3,nchar(names(dat)))
	dat
}

corTable <- function(demo,out,flex=T,plot=F, results =T){
	
	## Correlations
	LMR <- NULL
	P <- NULL
	
	for(i in 1:ncol(demo)){
		
		### Error trapping
		cl <- class(demo[,i])
		st <- 0
		message <- ""
		
		if(cl=="factor"){
			
			tb <- table(demo[,i])
			
			if(min(tb)<4) {
				nm <- names(tb)[which(tb<4)]
				demo[which(demo[,i]%in%nm),i] <- NA
				tb <- table(demo[,i])
			}
			
			#if(min(tb)<=(4)|max(tb)>=(nrow(demo-4))) {
			#	st <- 1
			#	message <- paste(names(demo)[i],"has too many records in the same category")
			#}
		}
	
			### Calculating Correlations
			lmr <- rep("",5)
			p <- rep("",5)
			if(st==0){
				for(j in 1:ncol(out)){
					lin.mod <- try(lm(out[,j]~demo[,i]),silent=T)
					lmr[j] <- round(sqrt(summary(lin.mod)$r.squared),2)
					p[j] <-	round(anova(lin.mod)[1,5],3)
			
				}
			}	
		
			LMR <- c(LMR,lmr)
			P <- c(P,p)
	}
	
	
	### Sorting our results
	res <- cbind(names(demo)[gl(ncol(demo),ncol(out))],rep(names(out),ncol(demo)),LMR,P)
	
	if(plot){
			
		par(mar=c(6,3,1,1))
		barplot(as.numeric(res[,3]),ylim=c(0,1),names=res[,2],space=0)
		abline(h=c(0.6,0.8,1),lty=2)
		abline(v=seq(0,35,by=7),lty=3)
		axis(side=1,c(3.5),c("Gender"),padj=3)
		axis(side=1,c(10.5),c("Age"),padj=3)
		axis(side=1,c(17.5),c("Duration"),padj=3)
		axis(side=1,c(24.5),c("Cohort"),padj=3)
		axis(side=1,c(31.5),c("Cust. Face."),padj=3)
	
	}
	
	## Adding in names
	colnames(res) <- c("Demographic","Outcome","Correlation","P-value")
	
	### Add in some summary stats
	ret <- res
	
	if(flex) {
	
		### Formatting Table
		rft <- flextable(data.frame(res))
		rft <- merge_v(rft,1)
		
		## Increasing size of font	
		rtf <- fontsize(rft,size=12)
		rtf <- fontsize(rft,size=14,part="header")
		
		### Shading rows
		id1 <- gl(ncol(demo),ncol(out))
		row.id <- which(as.numeric(id1)%%2==0)
		rft <- bg(rft,row.id,NULL,"lightblue")
		
		### shading important correlations
		or.id <- which(as.numeric(res[,4])<0.1)
		gr.id <- which(as.numeric(res[,4])<0.05)
		
		rft <- bg(rft,or.id,4,bg="orange")
		rft <- bg(rft,gr.id,4,bg="green")
	
		## Aligning cells
		rft <- align(rft,i=NULL,j=2:4,align="center")
		rft <- align(rft,i=NULL,j=1,align="left")
		rft <- align(rft,part="header",align="center")
	
		### make header bold
		rft <- bold(rft,part="header")
	
		ret <- rft
	}
	if(results){
		return(ret)
	}

}

corPlot <- function(x,y,namDem,namOut,demo,outcome){


	x.id <- which(namDem==x)
	y.id <- which(namOut==y)

	#x.id <- x
	#y.id <- y

	par(bty="l",mar=c(5,5,1,1))
	boxplot(outcome[,y.id]~demo[,x.id],col=c("hotpink","royalblue"),ylab=names(outcome)[y.id],xlab=names(demo)[x.id],cex.axis=1,cex.lab=1.3)
	


}












heatMapRJ <- function(x,meth="ward.D2",ident=T,plot=F,ret=T){
	
	s <- as.matrix(x)
	d1 <- dist(s)
	d2 <- dist(t(s))
	hca1 <- hclust(d1,method=meth)
	hca2 <- hclust(d2,method=meth)

	### Selecting groups
	h <- hca1
	out1 <- boxplot(h$height,plot="F")$out

	if(length(out1)>=1){
		#ct <- cutree(h,k=length(out1)+1)
		ct <- cutree(h,k=2)
	}


	### getting 'cut-points'
	cutP <- which(diff(ct[h$order])!=0)
	
	## creating matrix
	s2 <- s[hca1$order,hca2$order]
	
	### plot
	if(plot){
		#quartz(width=3,height=3)
		par(mar=c(0,0,0,0))
		layout(matrix(c(1,2,3,4),2,2),height=c(0.28,0.72),width=c(0.28,0.72))
		plot(0,0,col=0,bty="n",xaxt="n",yaxt="n")
		par(mar=c(2,3,0,0))
		plot(as.dendrogram(hca1),horiz=T,bty="n",xaxt="n",yaxt="n",leaflab="none")
		axis(side=2,nrow(s)/2,"Demographics",hadj=-0,tick=F,font=2,cex.axis=1.3)
		
		
		par(mar=c(0,0,3,2))
		plot(as.dendrogram(hca2),horiz=F,bty="n",xaxt="n",yaxt="n",leaflab="none")
		axis(side=3,ncol(s)/2,"Outcomes",tick=F,font=2,cex.axis=1.3)
		
		sx <- as.numeric(gl(ncol(s2),nrow(s2)))
		sy <- rep(nrow(s2):1,ncol(s2))
		
		s3 <- c(s2)
		un <- unique(s3)
		un.col <- heat.colors(length(un))
		
		
		for(i in 1:length(un)){
			s3[which(s3==un[i])] <- un.col[order(un)[i]]
		}
		
		
		par(mar=c(2,0,0,2))
		plot(sx,sy,xlim=c(0.5,ncol(s)+0.5),ylim=c(0.5,nrow(s)+0.5),yaxt="n",xaxt="n",col=0,bty="n")
		rect(sx-0.5,sy-0.5,sx+0.5,sy+0.5,col=rev(s3),border=rev(s3))
		
		axis(side=4,1:nrow(s),h$order,tick="F",las=2,hadj=1)
		axis(side=1,1:ncol(s),names(x),tick="F",padj=-1)
		
	
		if(ident==T){
	
			rect(0.5,0.5,ncol(x)+0.5,cutP+0.4,lwd=6,border="royalblue")
			rect(0.5,cutP+0.6,ncol(x)+0.5,nrow(x)+0.5,lwd=6,border="royalblue")
	
			mp <- c(cutP,nrow(x))
			mp <- cumsum(mp)/2 + 0.5
	
			text(ncol(x)/2+0.5,rev(mp),col="royalblue",cex=4,font=2)
		}
	}
	
	if(ret) return(ct)

}



corTable <- function(demo,out,flex=T,plot=F, results =T){
	
	## Correlations
	LMR <- NULL
	P <- NULL
	
	for(i in 1:ncol(demo)){
		
		### Error trapping
		cl <- class(demo[,i])
		st <- 0
		message <- ""
		
		if(cl=="factor"){
			
			tb <- table(demo[,i])
			
			if(min(tb)<4) {
				nm <- names(tb)[which(tb<4)]
				demo[which(demo[,i]%in%nm),i] <- NA
				tb <- table(demo[,i])
			}
			
			#if(min(tb)<=(4)|max(tb)>=(nrow(demo-4))) {
			#	st <- 1
			#	message <- paste(names(demo)[i],"has too many records in the same category")
			#}
		}
	
			### Calculating Correlations
			lmr <- rep("",5)
			p <- rep("",5)
			if(st==0){
				for(j in 1:ncol(out)){
					lin.mod <- try(lm(out[,j]~demo[,i]),silent=T)
					lmr[j] <- round(sqrt(summary(lin.mod)$r.squared),2)
					p[j] <-	round(anova(lin.mod)[1,5],3)
			
				}
			}	
		
			LMR <- c(LMR,lmr)
			P <- c(P,p)
	}
	
	
	### Sorting our results
	res <- cbind(names(demo)[gl(ncol(demo),ncol(out))],rep(names(out),ncol(demo)),LMR,P)
	
	if(plot){
			
		par(mar=c(6,3,1,1))
		barplot(as.numeric(res[,3]),ylim=c(0,1),names=res[,2],space=0)
		abline(h=c(0.6,0.8,1),lty=2)
		abline(v=seq(0,35,by=7),lty=3)
		axis(side=1,c(3.5),c("Gender"),padj=3)
		axis(side=1,c(10.5),c("Age"),padj=3)
		axis(side=1,c(17.5),c("Duration"),padj=3)
		axis(side=1,c(24.5),c("Cohort"),padj=3)
		axis(side=1,c(31.5),c("Cust. Face."),padj=3)
	
	}
	
	## Adding in names
	colnames(res) <- c("Demographic","Outcome","Correlation","P-value")
	
	### Add in some summary stats
	ret <- res
	
	if(flex) {
	
		### Formatting Table
		rft <- flextable(data.frame(res))
		rft <- merge_v(rft,1)
		
		## Increasing size of font	
		rtf <- fontsize(rft,size=12)
		rtf <- fontsize(rft,size=14,part="header")
		
		### Shading rows
		id1 <- gl(ncol(demo),ncol(out))
		row.id <- which(as.numeric(id1)%%2==0)
		rft <- bg(rft,row.id,NULL,"lightblue")
		
		### shading important correlations
		or.id <- which(as.numeric(res[,4])<0.1)
		gr.id <- which(as.numeric(res[,4])<0.05)
		
		rft <- bg(rft,or.id,4,bg="orange")
		rft <- bg(rft,gr.id,4,bg="green")
	
		## Aligning cells
		rft <- align(rft,i=NULL,j=2:4,align="center")
		rft <- align(rft,i=NULL,j=1,align="left")
		rft <- align(rft,part="header",align="center")
	
		### make header bold
		rft <- bold(rft,part="header")
	
		ret <- rft
	}
	if(results){
		return(ret)
	}

}





corClust <- function(x,ct,namDem,demo){


	x.id <- which(namDem==x)
	X <- demo[,x.id]
	if(class(X)!="numeric"){
	
		barplot(table(X,ct),main=x,xlab="Groups")
		}

	if(class(X)=="numeric"){
	
		boxplot(X,ct,main=x)
		}


}





summaryTable <- function(x,by="none",cont.sum="med",se=TRUE,perc=TRUE,missing=TRUE,row=TRUE,flex=FALSE,nam=colnames(x)){
if(by[1]=="none") stop("No 'by'. I will do this bit later - Rich")
    if(class(by)!="factor") by <- as.factor(by)
    if(class(x)=="matrix") x <- as.data.frame(x)
	missing.by <- length(which(is.na(by)))>0
    lev <- levels(by)
	#nam <- names(cov)
    TAB<-table(by,useNA="always")
    TAB<-c("Total","",TAB,sum(TAB))
    for(i in 1:ncol(x)){
		## reclassifyign character covariates
		if(class(x[,i])=="character"){
			fac.int <- length(unique(x[,i]))/length(x[,i])
			if(fac.int>0.1) {
				x[,i] <- as.numeric(x[,i])
				warning(paste(nam[i],"converted to numeric"))
				}
			if(fac.int<=0.1) {
				x[,i] <- as.factor(x[,i])
				warning(paste(nam[i],"converted to factor"))
				}
		}
        ## Factor variables
        if(class(x[,i])=="factor"){
            x[,i]<-as.factor(as.character(x[,i]))
            tab<- summ.fac(x[,i],by=by,row=row,perc=perc)
			tab <- cbind(rep(nam[i],nrow(tab)),c(levels(x[,i]),"Missing"),tab)  
			if(!missing) tab <- tab[-nrow(tab),]
	        TAB<-rbind(TAB,tab)
        }
        ## Continuous Variables
        if(class(x[,i])=="integer"|class(x[,i])=="numeric"){
            if(cont.sum=="med") {
                tab<-tapply(x[,i],by,summ.med)
                tab<-c(nam[i],"median (IQR)",tab,NA, summ.med(x[,i]))
            }
            if(cont.sum=="mean") {
                tab<-tapply(x[,i],by,summ.mean,se=se)
                tab<-c(nam[i],"mean (se)",tab,NA, summ.med(x[,i]))
            }
			if(missing){
	            na.tab<-table((is.na(x[,i])|x[,i]=="-Inf"|x[,i]=="Inf"),by)
	            na.id<-which(row.names(na.tab)=="TRUE")		
				### only add missing column if any observed
	            if(length(na.id)>0)	{
					na.tab<-na.tab[na.id,]
	            	na.tab<-c(na.tab,0,sum(na.tab))
					na.tab <- c(nam[i],"Missing",na.tab)
					tab<-rbind(tab,na.tab)	
				}            		
			}
            TAB<-rbind(TAB,tab)
        }
        ## Neither Factor or Continuous
        if(class(x[,i])!="integer"&class(x[,i])!="numeric"&class(x[,i])!="factor"&class(x[,i])!="character"){ 
			warning(paste(nam[i],"not included due to uncompatible class"))
        }
    }
	### Adding in column names
	colnames(TAB) <- c("Covariate","Level", lev ,"Missing","Total")
	### Removing missing column (if required)
	if(!missing|!missing.by) TAB <- TAB[,-which(colnames(TAB)=="Missing")]
	### Setting return object
	ret <- TAB
    ### Formatted flex table (if required)
	if(flex){
		flexTAB <- FlexTable(TAB,...)	
		### merging column for factors
		nam.tab <-which(table(TAB[,1])>1)	
		for(m in names(nam.tab)){
			row.id <- which(TAB[,1]==m)
			flexTAB <- spanFlexTableRows(flexTAB,1,min(row.id),max(row.id))
		}
		ret <- flexTAB
	}
	ret
}





