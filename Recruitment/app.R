
#install.packages("shiny")
library(shiny)
#source("recForcast.R")


rec.forcast<-function(N.site,rpm,open.rate,Max.Time,penal=0.5,plot=TRUE,...){ 

## Getting the number of open sites per month
open.site<-seq(1,N.site,by=open.rate)
if(max(open.site)!=N.site) open.site <- c(open.site,N.site)
open.site<-c(open.site,rep(N.site,Max.Time-length(open.site)))
### Basic average rate per site approach
month.rate<-open.site*rpm

## penalisng monthly recruitment (recruits 1/2 as much in first month)
penalty <- diff(c(0,month.rate))*penal
month.rate <- month.rate-penalty

cum.rec<-round(cumsum(month.rate))
month.rate <- diff(c(0,cum.rec))

rec<-data.frame("Monthly Rec"=month.rate,"Cumualtive Rec."=cum.rec)


if(plot) {
	
	plot(cum.rec,typ="l",xlab="Time (Months)",ylab="Cumulative Recruitment",font.lab=3,...)
	npat <- max(rec[,2])
	nmon <- nrow(rec)
	pat.by <- 25*max(round(npat/125),1);pat.by
	mon.by <- 3*max(round(nmon/18),1);mon.by	abline(h=seq(0,npat*2,by=pat.by),v=seq(0,nmon*2,by=mon.by),lty=2,col="lightgray",lwd=3)
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
                  min = 0.1, max = 10, value = 1),

      sliderInput("openRate", "Rate of Opening sites (per month):",  
                  min = 1, max = 5, value = 2),

      sliderInput("maxTime", "Length of Recruitment (months):",  
                  min = 1, max = 120, value = 12),
          
          
		## Add a stop button for development	        
      actionButton("close",label="stop")
                  
                  
    ),

    mainPanel(
      plotOutput("recPlot")
    )
    
    
  )
)
	
	
	
	
############
server <- function(input, output) {

	rec <- eventReactive(input$go,{ 
		rec.forcast(input$nSite,input$rpm,input$openRate,input$maxTime)
		})
	
	
	## Plot
	output$recPlot <- renderPlot({	
rec.forcast(input$nSite,input$rpm,input$openRate,input$maxTime,cex.axis=1.2,cex.lab=1.3,col="lightblue",lwd=6)
		
		})


	### Stopping App
   observe({
      if (input$close > 0) stopApp()                             # stop shiny
    })

}


shinyApp(ui, server)









