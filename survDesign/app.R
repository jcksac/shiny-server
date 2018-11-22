
#install.packages("shiny")
library(shiny)





#############################
#############################



# Define UI for dataset viewer app ----
ui <- fluidPage(

	## Loading CSS
	#includeCSS("bootstrap.css"),

	br(),
  	
  	# App title ----
  	h1("Design of Trials with a Time-to-Event Endpoint"),

	### Setting up panel structures
	tabsetPanel(type = "tabs",       



		#### Panel 1 - Events
		tabPanel("Number of Events",
			
			br(),
			
			fluidRow(
					
				column(5,offset=1,
					selectInput("alpha","Alpha Level",c(0.01,0.05,0.1))
					),
				column(5,offset=1,
					selectInput("tails","Tails",c("One-sided","Two-sided"))
				)
			),
			
			fluidRow(
					
				column(5,offset=1,
					selectInput("power","Power",c(0.8,0.85,0.9))
					),
				column(5,offset=1,
					sliderInput("hr","Hazard Ratio",0.5,2,0.75,0.01)
				)
			),
			
			br(),
			
			fluidRow(
				plotOutput(outputId="sdPlot1")
			)
		),


		#### Panel 1 - Survival Function		
		tabPanel("Survival Function",
			
			br(),
			
			fluidRow(
				column(5,offset=1,
					selectInput("survFunc","Survival Function",c("Exponential","Weibull","Log Logistic","Log Normal"))
					)
			),
			
			fluidRow(
					
				column(5,offset=1,
					sliderInput("scale","Scale",0.5,2,0.75,0.01)
					),
				column(5,offset=1,
					sliderInput("shape","Shape",0.5,2,0.75,0.01)
				)
			),
			
			br(),
			
			fluidRow(
				plotOutput(outputId="sdPlot2")
			)
		),
	
		#### Panel 1 - Recruitment	
		tabPanel("Recruitment",
			fluidRow(),
			fluidRow(
				plotOutput(outputId="sdPlot3")
			)
		),
		
		#### Panel 1 - Design	
		tabPanel("Design",
			fluidRow(),
			fluidRow(
				plotOutput(outputId="sdPlot4")
			)
		)
	)
)
	
	
	
	
	
	
	
############
server <- function(input, output) {

	output$sdPlot1 <- renderPlot((hist(rnorm(100))))

}


shinyApp(ui, server)









