
#install.packages("shiny")
library(shiny)


#####################################################
############# Function 

survFunction <- function(time,parm,dist="Exponential"){
	
	S<- NULL
	if(dist=="Exponential"){
		S <- exp(-parm[1]*time)
	}
		
	if(dist=="Weibull"){
		S <- exp(-parm[1]*time^parm[2])
	}
	
	if(dist=="Lognormal"){
		S <- 1-pnorm((log(time) - parm[1])/parm[2])
	}

	if(dist=="Loglogistic"){
		S <- 1 / (1 + (parm[1]*time)^exp(parm[2]))
	}
	
	
	S
}

#####################################################


#############################
#############################



# Define UI for dataset viewer app ----
ui <- fluidPage(

	#includeCSS("bootstrap.css"),

  	titlePanel("Survival Functions"),
  	
  	h4("This page shows the shape and behaviour of basic survival functions."),

  sidebarLayout(

	#### Survival Function
    sidebarPanel(
		selectInput("survFunc", "Distribution:",  
		                  c("Exponential","Weibull","Loglogistic","Lognormal")),
		
		uiOutput("parm1"),
		uiOutput("parm2"),
		uiOutput("parm3"),
		uiOutput("parm4"),
		uiOutput("parm5"),


		#### Survival Function
     	#h3("Add points to figure"),
      	#sliderInput(inputId = "survAP",label = "survival Time",0,1,step=0.01,value=0.5),
      	#uiOutput("timeAP"),
      	
      	### Add button to add points
		#actionButton("AP",label="Add point"),
		

		## Add a stop button for development	        
      	actionButton("close",label="stop")
                  
                  
    ),

    mainPanel(
      	plotOutput("survPlot"),

      	#### Maximum Time
      	sliderInput(inputId = "maxTime",label = "Max. Time",0,120,step=1,value=24)


    )
    
    
  )
)
	
	
	
	
############
server <- function(input, output) {


			
	output$parm1 <- renderUI({
		lab <- "Hazard Rate"
		val <- 0.05
		min <- 0
		max <- 0.5
		step <- 0.005
		
		if(input$survFunc=="Peicewise Exponential"){
			lab <- "Hazard Rate 1"	
		}

		if(input$survFunc=="Lognormal"){
			lab <- "Mean"	
			max <- 5
			step <- 0.05
		}

		if(input$survFunc=="Loglogistic"){
			lab <- "scale"	
		}
		sliderInput(inputId = "parm1",label = lab,min,max,step,value=val)
	})


	output$parm2 <- renderUI({
		if(input$survFunc!="Exponential"){
			lab <- "Shape"
			val <- 0			
			min <- -1
			max <- 1
			step <- 0.05
		
			if(input$survFunc=="Peicewise Exponential"){
				lab <- "Hazard Rate 2"	
			}
			if(input$survFunc=="Lognormal"){
				lab <- "Variance"	
			}		
			sliderInput(inputId = "parm2",label = lab,min,max,step,value=val)
		}
	})


	output$parm3 <- renderUI({
		if(input$survFunc=="Peicewise Exponential"){		
			sliderInput(inputId = "parm3",label = "Hazard Rate 3",-0.5,0.5,step=0.005,value=0.05)
		}
	})

	output$parm4 <- renderUI({
		if(input$survFunc=="Peicewise Exponential"){		
			sliderInput(inputId = "parm4",label = "Hazard Rate 4",-0.5,0.5,step=0.005,value=0.05)
		}
	})

	output$parm5 <- renderUI({
		if(input$survFunc=="Peicewise Exponential"){		
			sliderInput(inputId = "parm5",label = "Hazard Rate 5",-0.5,0.5,step=0.005,value=0.05)
		}
	})
	
	output$timeAP <- renderUI({
		sliderInput(inputId = "timeAP",label = "Timepoint",0,input$maxTime,step=1,value=0)
	})

	


	


	## Plot
	output$survPlot <- renderPlot({	
		
		if(input$survFunc=="Exponential"){
			PARM <- input$parm1
		}

		if(input$survFunc%in%c("Weibull","Lognormal","Loglogistic")){
			PARM <- c(input$parm1,exp(input$parm2))
		}
		
		time <- seq(0,input$maxTime,by=0.25)
		surv <- survFunction(time,PARM,dist=input$survFunc)
		plot(time,surv,typ="l",col="hotpink",ylim=c(0,1),,xlim=c(0,input$maxTime),lwd=6,ylab="Survival Proportion",xlab="Time")
		abline(h=seq(0,1,by=0.2),v=seq(0,input$maxTime,by=6),lty=2,col="gray")
	})


	## Add point to graph
    #observe({
    #		if (input$AP > 0) points(input$timeAP,input$survAP,col="royalblue",pch=20,cex=2)                           	# stop shiny
    #})

	## Stopping App
    observe({
    		if (input$close > 0) stopApp()                             # stop shiny
    })

}


shinyApp(ui, server)









