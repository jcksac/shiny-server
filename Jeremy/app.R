#install.packages("BH")
library(shiny)


### getting SV functions
source("JezFunc.R")


### Getting data


## nAgent 
## nTrans
## maxTV
## alpha
## pow,trace=F


# Define UI for dataset viewer app ----
ui <- fluidPage(

includeCSS("bootstrap.css"),

   # App title
  h1("Inequality"),

h4("Inequality is increasing as is recognition that this is a bad thing. It’s one thing though to recognise the problem. It’s another one to fix it."),
h4("The simple model below is designed to show how inequality will increase within our current economic system even in a situation where everyone starts with equal opportunities."),
h4("Please download the model, improve it and send it back. We suspect that most improvements that make it more realistic will only speed the rate at which inequality arises. What would be interesting would be improvements, linked to policy changes, that slow or stop the rate."),
h4("In this very simple starting point there are people in an economy that trade with each other. There is a value to both parties of trading which is a fixed amount. The value represents both the benefit from selling a service and the benefit from using it. This benefit is not however shared equally but depends on a beta distribution (see below)."),
h4("Currently you can select the number of people, the value of a trade and the power balance between the people trading. The power balance represents the extent to which a person with more accumulated value than another person can increase their share of the benefit of a trade. You can then ‘run’ the model by clicking Run Transactions. Each click represents X trades."),
h3("The model"),
tags$ul(
    tags$li("Does not differentiate between roles"), 
    tags$li("People start with the same level of accumulated benefit (zero)"), 
    tags$li("The size of the benefit is fixed"),
    tags$li("Both parties always gain some value")
),
h4("This could be made much more complex and become a closer model of the real world."),
h3("The model has two outputs."),
h4("A graph over time with the peoples accumulated value and the distribution of that value between people."),
h4("A graph over time showing the extent to which people move up or down the distribution, a measure of social mobility. The model compares the position in the distribution of every person in our simulation and calculates the average change in ranks between two time-points in our simulation. Low mobility is when people stay in roughly the same place."),
h4("The model reveals some simple dynamic issues around inequality"),
h4("With equal power between people a level of inequality does still result, but it remains constant. Mobility is initially high but falls off quickly and stabilises. As benefit accumulates the fixed benefit of each trade becomes a small proportion of the total and so has less effect over time on a persons position in the distribution. Introducing some leakage of benefit and allowing the size of the benefit to vary would increase mobility although it would still decline. For example a distribution of size of benefit and a random chance of benefiting would mean that people are lower end of distribution would occasionally be involved in a high value trade and move up in the distribution. This would have a positive occasional effect on mobility."),
h4("A more realistic representation would skew the likelihood of high value trades to those with higher accumulated benefit."),
h4("Because mobility of one person upwards requires another to go down any more realistic representation would also need to allow for people’s actions to protect their position and reduce the change of dropping down, further reducing mobility."),
h4("The important point is that even in a simple model driven by chance with no power imbalance there will be some inequality and some social mobility. Introducing power imbalances (which are independent of peoples abilities) inequality rapidly increases and mobility falls."), 
h4("We suspect that introducing tax will act as a lag in the system, delaying changes in inequality, but will not have an effect on the tendency of the system to result in inequality. A more accurate model with people starting with different levels of wealth would increase inequality and this could be reduced by taxation, especially inheritance tax, but not change the underlying tendency."),
h2("Beta distribution"),
h4("The amount of each transaction that is given to each agent in the transaction is determined using a Beta distribution. This randomly selects the amount of each value to distribute and when the trade is 'fair' then each agent has an equal chance of getting the most value (or the least!). To take this further, lets call the two agents A_1 and A_2. We can bias the distribution of value in each transaction using the following equations R = (A_1/A_2)^p. Here 'p' is our parameter which lets us control the level of bias. When 'p = 0' then 'R=1' and this represents a fair trade. When 'p > 0' we start to bias the distribution of wealth towards the agent with the highest value in the trade "),


	
	
	h2("The results"),
	
	h4("Use the model below to see what happens when the model parameters are chaged."),
	

  # Output: Plot with the rsults of the simulation
  plotOutput("sims"),

  # Sidebar layout with a input and output definitions
	br(),

	fluidRow(
		
		column(5,offset=1,
			h4("Increase/Decrease the number of transactions:")
		),
	
		column(3,
			actionButton(inputId="decTrans", "<", width=25),
			actionButton(inputId="incTrans", ">", width=25)
		)
	),

	br(),

	fluidRow(
	column(3,offset=1,
     	
	    # Input: Selector for choosing nAgents
	 		sliderInput(inputId="nAgent","Number of Agents",10,500,100)
	 	
    ),
   
	column(3,

	 	# Input: Selector for choosing maxTV
	 	sliderInput(inputId="maxTV", "Transaction Value", 1, 100, 10)
    ),
   
    column(3,
	 		
	 	# Input: Selector for choosing pow
	 	sliderInput(inputId="pow", "Power", 1, 10, 1, step=0.25)
	 ),
	 
	 column(1,
		## Action Button
		actionButton(inputId="go", label="Click here to run 10,000 transactions.")	
    )
    
 ),
 
 br(),
 
 h2("Mobility"),
 
 h4("When we're talking about mobility, we want to know more than just the difference between the top and the bottom, but also the ability of agents to move up and down.  To monitor this we look at a simple measure of mobility by comparing the ranks of every agent in our simulation.  To do this we look at the average change in ranks between two time-points in our simulation.  Low numbers occur when every agent stays in roughly the same place and higher numbers occure when there is more movement!"),
 
 plotOutput("mob")


)



############
server <- function(input, output) {


		index <- reactiveVal(1)

  # Return the requested dataset ----
	data <- eventReactive(input$go,{ 
		jez((input$nAgent),10000,(input$maxTV),5,(log(input$pow)),trace=500)
		})

	
	# 
	observeEvent(input$go,{
		newIndex <- 1
		index(newIndex)
	})
	
	observeEvent(input$incTrans,{
		newIndex <- min(index() + 1,20)
		index(newIndex)
	})

	observeEvent(input$decTrans,{
		newIndex <- max(0,index() - 1)
		index(newIndex)
	})

  output$sims <- renderPlot({	
  	par(bg="lightgray")
  	xmax <- mean(data()[,ncol(data())])*2
	plot(ecdf(data()[,index()]),lwd=5,col="royalblue",yaxt="n",ylab="Position", xlab="value",bty="l",main="",font.lab=2,cex.lab=1.4,xlog="T",xlim=c(0,xmax))

	if(index()>1){
	quint <- cut(data()[,index()],c("-Inf",quantile(data()[,index()],p=c(0.2,0.4,0.6,0.8)),"Inf"))
	val <- round(tapply(data()[,index()],quint,mean))
	totalVal <- paste("Total Value:",sum(data()[,index()]))
		
	abline(h=c(0,.2,.4,.6,.8,1),lty=3)
		
	text(1900,.70,"Quintile values",cex=1.2)
	text(1900,.6,paste("5.",val[5]))
	text(1900,.53,paste("4.",val[4]))
	text(1900,.46,paste("3.",val[3]))
	text(1900,.39,paste("2.",val[2]))
	text(1900,.32,paste("1.",val[1]))
	
		
	text(1900,.20,"% Diff between highest and lowest")
	text(1900,.13,paste(round(100*(val[5]-val[1])/val[1])))
	
	}
	})
	
	
	output$mob <- renderPlot({	
		if(index()>1){
			mobPlot(data(),index())
			abline(h=c(2,5,10),lty=2)
		}
		
		})
	


}



shinyApp(ui, server)







