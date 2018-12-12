#setwd("/Users/richardjackson/Dropbox/Jackson SAC/Projects/Rshiny/BenSV")
#install.packages("shiny")
library(shiny)
#library(knitr)
source("benFunc.R")
#source("/Users/richardjackson/Dropbox/Documents/R Utils/statsTools.R")
data <- read.csv("data/CJdata.csv")

#ct <- heatMapRJ(outcome,ident=F,plot=F,ret=T)

#############################
#############################

### Bens functions

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
	                  choices = dir("data")),
	                  
	             actionButton("close",label="stop"),


	                  
	tabsetPanel(type = "tabs",       



    		
    		#######################
			#### Panel 1
			tabPanel("Data",
		
			br(),
			h4("This section provides a first look at the data you have collected!"),
			br(),
		
				#fluidRow(
				#	column(4,offset=1,
				#	selectInput("dataType", "Choose your data Type:",
	            #      choices = c("Demographics","Outcome"))
	            #      ),
				#),
	      
	      
	      	fluidRow(
				column(4,offset=1,tableOutput(outputId="demTable")),
				column(4,offset=1,tableOutput(outputId="outTable"))
				)
    	
    		),



    		
    		#######################
    		#### Panel 2
    		tabPanel("Correlations (1)",
		
			br(),
			h4("Here we have a chance to look at an overview of all the correlations between the deomgraphic and outcome data you have collected!"),
			br(),
	
			fluidRow(
				column(10,offset=1,plotOutput(outputId="corPlotAll"))
			)
    	
    		),
	
   	
    	
    		#######################
    		#### Panel 3
			tabPanel("Correlations (2)",
			
			br(),
			h4("Here you can have a closer look at the connections between each demographice and outcome data, one variable at a time!"),
			br(),
	

			fluidRow(
				column(10,offset=1,plotOutput(outputId="corPlotInd",height="600px"))	
			),
			
			br(),
			
			fluidRow(
				column(4,offset=1,uiOutput("selectDemo")),
				column(4,offset=1,uiOutput("selectOutput"))
				)
			
			),
    		
    		
    		#######################
    		#### Panel 4
			tabPanel("Cluster Analysis",
				
			br(),
			h4("Heirachical Cluster analysis is a way of grouping together your outcome data into naturally occuring clusters.  In the heat graph below, we order the subjects included in your data and the outcome to show you the groups that are formed"),
			br(),

			fluidRow(
				column(10,offset=1,plotOutput(outputId="heatPlot",height="600px"))		
			)
			
			),
    		
    		#######################
    		#### Panel 5
			tabPanel("Cluster Comparisions",
			
					
			br(),
			h4("Now we have found our clusters, it's time to find out what makes are groups different from one another!"),
			br(),
		

			fluidRow(
				column(10,offset=1,plotOutput(outputId="corPlotClust"))
			),
			
			br(),
			
			fluidRow(
				column(4,offset=1,uiOutput("selectDemCor"))
			)
				
			
			)


	)
)

 

    	    	
############
server <- function(input, output) {


	dat <- reactive({
		file <- paste("data/",input$dataset,sep="")
		dat <- read.csv(file)
		demo <- demSelect(dat)
		outcome <- outSelect(dat)
		ret <- list(dat,demo,outcome)
		})
		

	ct <- reactive({
		heatMapRJ(dat()[[3]],ident=F,plot=F,ret=T)
		})
		

	
	##########
	### Tab 1 - summary of data	
	output$demTable <- renderTable(
		summaryTable(dat()[[2]],by=round(runif(nrow(dat()[[2]]))))[,-c(3,4)])


	output$outTable <- renderTable(
		summaryTable(dat()[[3]],by=round(runif(nrow(dat()[[3]]))))[,-c(3,4)])	
	
	##########
	### Tab 2 - 
	output$corTable <- renderTable(data.frame(corTable(dat()[[3]],outcome,flex=F)))
	
	##########
	### Tab 3	
	output$selectDemo <- renderUI({
		selectInput(inputId = "demoCor2",
	    label = "Demographic Factors",
	    choices = names(dat()[[2]]))
	})


	output$selectOutput <- renderUI({
		selectInput(inputId = "outCor2",
	    label = "Outcomes",
	    choices = names(dat()[[3]]))
	})

	output$corPlotInd <- renderPlot(corPlot(input$demoCor2,input$outCor2,names(dat()[[2]]),names(dat()[[3]]),dat()[[2]],dat()[[3]]))
	
	
	#######
	
	output$corPlotAll <- renderPlot(corTable(dat()[[2]],dat()[[3]],results=F,flex=F,plot=T))
	
	##########
	### Tab 4
	output$heatPlot <- renderPlot(heatMapRJ(dat()[[3]],ident=T,plot=T,ret=F))
	
	##########
	### Tab 5	
	output$selectDemCor <- renderUI({
		selectInput(inputId = "demoHeatPlot",
	    label = "Demographic Factors",
	    choices = names(dat()[[2]]))
	})

	output$corPlotClust <- renderPlot(corClust(input$demoHeatPlot,ct(),names(dat()[[2]]),dat()[[2]]))

	observe({
	 	if(input$close > 0) stopApp()  # stop shiny
	})
	
}

shinyApp(ui, server)
<<<<<<< HEAD


		demo <- demSelect(data)
		out <- demSelect(data)
=======
>>>>>>> 11246f9f4e90f415c406804dba6dbead7ac6af63
