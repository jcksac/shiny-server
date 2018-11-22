
#install.packages("shiny")
library(shiny)
source("recForcast.R")



#############################
#############################



# Define UI for dataset viewer app ----
ui <- fluidPage(

  titlePanel("Recruitment Estiamtes"),

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
		abline(h=seq(0,1000,50),v=seq(0,120,6),lty=2,col="lightgray",lwd=3)
		})


	### Stopping App
   observe({
      if (input$close > 0) stopApp()                             # stop shiny
    })

}


shinyApp(ui, server)









