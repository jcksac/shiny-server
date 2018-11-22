
#install.packages("shiny")
library(shiny)
source("benFunc.R")
data <- read.csv("data/CJdata.csv")

demo  <- data[,2:6]
outcome <- data[,7:13]
namDem <- c("Gender","Age","Duration","Cohort","Cust. Face.")
namOut <- c("A","B","C","D","E","F","G")

ct <- heatMapRJ(outcome,ident=F,plot=F,ret=T)

#############################
#############################


# Define UI for dataset viewer app ----
ui <- fluidPage(

	## Loading CSS
	includeCSS("bootstrap.css"),

	br(),
  # App title ----
  h1("Social Values - Clusters and Patterns"),

	h4("This website will let you see your data and the patterns they create!"),

   				selectInput(inputId = "dataset",
	                  label = "Choose a dataset:",
	                  choices = c("SJdata","Other datasets...")),
	                  
	tabsetPanel(type = "tabs",       

		#### Panel 1
		tabPanel("Data",
	
		br(),
		h4("This section provides a first look at the data you have collected!"),
		br(),
	
			fluidRow(
			
				column(4,offset=1,
				selectInput("dataType", "Choose your data Type:",
                  choices = c("demographics","outcome"))
                  )
			),
      
      	br(),
      
      	fluidRow(
				column(4,offset=1,
					tableOutput(outputId="demTable")
					)
				)
    	
    		),
    		
    			#### Panel 2
    	tabPanel("Correlations (1)",
	
		br(),
		h4("Here we have a chance to look at an overview of all the correlations between the deomgraphic and outcome data you have collected!"),
		br(),
	
			fluidRow(
				column(10,offset=1,
					plotOutput(outputId="corPlotAll")
					)
			)
    	
    		),
	
   	
    	
    		#### Panel 2
			tabPanel("Correlations (2)",
			
		br(),
		h4("Here you can have a closer look at the connections between each demographice and outcome data, one variable at a time!"),
		br(),
	

			fluidRow(
				column(10,offset=1,
					plotOutput(outputId="corPlotInd",height="600px")
				)
			
			),
			
			br(),
			
			fluidRow(
			
				column(4,offset=1,
					selectInput("demCov","Demographic Covariate",choices=c("Gender","Age","Duration","Cohort","Cust. Face."))
				),
				
				column(4,offset=1,
					selectInput("outCov","Outcome Covariate",choices=c("A","B","C","D","E","F","G"))
				)
			
			)
			
			),
    		
    		
    		#### Panel 3
			tabPanel("Cluster Analysis",
				
			br(),
			h4("Heirachical Cluster analysis is a way of grouping together your outcome data into naturally occuring clusters.  In the heat graph below, we order the subjects included in your data and the outcome to show you the groups that are formed"),
			br(),

			fluidRow(
				column(10,offset=1,
					plotOutput(outputId="heatPlot",height="600px")
				)
			
			),
			
			br(),
			
			## Action Button
			actionButton(inputId="go", label="THE BUTTON!")
			
			),
    		
    		#### Panel 4
			tabPanel("Cluster Comparisions",
			
				
		br(),
		h4("Now we have found our clusters, it's time to find out what makes are groups different from one another!"),
		br(),
	

			fluidRow(
				column(10,offset=1,
					plotOutput(outputId="corPlotClust")
				)
			
			),
			
			br(),
			
			fluidRow(
			
				column(4,offset=1,
					selectInput("demClustCov","Demographic Covariate",choices=c("Gender","Age","Duration","Cohort","Cust. Face."))
				)
				
			
			)

			
			)


	)
)

 

    	
    	
############
server <- function(input, output) {

	#ct <- eventReactive(input$go,{ 
	#	heatMapRJ(outcome,ident=F,plot=F,ret=T)
	#	 })
		
	output$demTable <- renderTable(outcome)
	output$corTable <- renderTable(data.frame(corTable(demo,outcome,flex=F)))
	output$corPlotAll <- renderPlot(corTable(demo,outcome,results=F,flex=F,plot=T))
	output$corPlotInd <- renderPlot(corPlot(input$demCov,input$outCov,namDem,namOut,demo,outcome))
	output$heatPlot <- renderPlot(heatMapRJ(outcome,ident=T,plot=T,ret=F))
	output$corPlotClust <- renderPlot(corClust(input$demClustCov,ct,namDem,demo))
	
}


shinyApp(ui, server)