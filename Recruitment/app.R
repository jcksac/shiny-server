
#install.packages("shiny")
library(shiny)
#source("recForcast.R")


#####################################################
############# Function 

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
	
	if(length(openSite)<maxTime){openSite <- c(openSite,rep(nSite,maxTime-length(openSite)))}else{openSite <- openSite[1:maxTime]}
	 
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


#############################
#############################



# Define UI for dataset viewer app ----
ui <- fluidPage(

	#includeCSS("bootstrap.css"),

  	titlePanel("Recruitment Estimates"),
  	
  	h4("This page gives a basic funtion for estimating recruitment forecasts for clinical trials.  Inputs required are: The number of sites available, average rate of recruitment, the rate of opening sites to recruitment and the length of time available"),

  sidebarLayout(

    sidebarPanel(
		sliderInput("nSite", "Number of Sites:",  
		                  min = 1, max = 150, value = 5),
		
		sliderInput("rpm", "Average Monthly Recruitment",  
		                  min = 0.1, max = 10, value = 1,step=0.1),
		
		sliderInput("openRate", "Rate of Opening sites (per month):",  
		                  min = 1, max = 5, value = 2,step=0.5),
		
		sliderInput("maxTime", "Length of Recruitment (months):",  
		                  min = 1, max = 120, value = 12)

		# Add a stop button for development	        
      	#actionButton("close",label="stop")
                  
                  
    ),

    mainPanel(
      plotOutput("recPlot"),
      textOutput("protLang")
    )
    
    
  )
)
	
	
	
	
############
server <- function(input, output) {

	#rec <- eventReactive(input$go,{ 
	#	rec.forcast(input$nSite,input$rpm,input$openRate,input$maxTime)
	#	})
	
	
	## Total
	output$totalOutput <- renderText({
		max(rec.forcast(input$nSite,input$rpm,input$openRate,input$maxTime,plot=F)[,2])
	})
	
	## Plot
	output$recPlot <- renderPlot({	
rec.forcast(input$nSite,input$rpm,input$openRate,input$maxTime,cex.axis=1.2,cex.lab=1.3,col="lightblue",lwd=6)
		})


	### Protocol Language
	output$protLang <- renderText({	
		rec <- rec.forcast(input$nSite,input$rpm,input$openRate,input$maxTime,plot=F)
		paste("With recruitment taking place over",input$nSite,"sites recruiting at an average monthly rate of",input$rpm,"patients/site/month, then a total of",max(rec[,2]),"patients are anticipated over a period of",input$maxTime,"months with sites themselves being opened at a rate of",input$openRate,"sites per month")
	})
	
	
	### Stopping App
    # observe({
    #    if (input$close > 0) stopApp()                             # stop shiny
    # })

}


shinyApp(ui, server)









