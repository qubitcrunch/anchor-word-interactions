shinyUI(
			conditionalPanel(condition="output.loggedOut=='0'",
			fluidPage(
			hr(),
			conditionalPanel(condition="input.annotator=='select'||output.display=='0'",
  			sidebarPanel(uiOutput("annotators"))),
  			##
  			conditionalPanel(condition="input.annotator!='select'",
  			conditionalPanel(condition="input.dataset=='select'",hr(),uiOutput("dataset")),
  			conditionalPanel(condition="input.dataset!='select'",
			conditionalPanel(condition="output.load=='0'",
			sidebarPanel(actionButton("load",label="Load Previous Work",icon=icon("download")))
			),
			conditionalPanel(condition="output.display=='1'",
  			conditionalPanel(condition="output.load=='1'",
  							#selectInput("task",NULL,c("Select task"="select","Training"="training","Evaluation"="evaluation")),
  							#conditionalPanel(condition="output.task1=='0'&&(output.task2=='0')",
			tabsetPanel(
  	 		tabPanel("Feedback",
                fluidRow(
            	headerPanel("Documents"),
            		column(
    			wellPanel(
    			uiOutput("outDoc1")
    			,style = "overflow-y:scroll; max-height: 300px"),
    			width=6)
    				,
    				
            	column(
    			wellPanel(
    			htmlOutput("outDoc2")
    			,style = "overflow-y:scroll; max-height: 300px"),
    			width=6)
    			),
    			#uiOutput("Buckets"),
    			# fluidRow(
    						# selectInput("numBuckets","# of Buckets",choices=1:(K),selected=1),
    						# column(4,
  								# h2(div(icon("hand-o-down"),"Drop Words")),
  								# uiOutput({"feedback"})
 								# ),
  								# h2(icon("trash")),
  								# uiOutput("trashBucket")
  								# )
  				
    			
				fluidRow(
				column(selectInput("numBuckets","# of Buckets",choices=1:(10),selected=1),width=1),
				uiOutput("feedback")),
				fluidRow(
				#h3(div(icon("trash"),"Trash Bucket")),
				#uiOutput("trashBucket")
				
    			),
    			hr(),
    			fluidRow(
 				column(2,actionButton("nextPair",label="Next",icon=icon("arrow-circle-right"))),
   				#column(2,actionButton("trash",label="Trash Words",icon=icon("trash"))),
  				column(2,actionButton("add",label="Add Constraints")),
  				column(4,actionButton("clear",label=icon("refresh",lib="glyphicon"),
   										style="color: black; background-color: silver; border-color: #silver;width: 40px;
    									height: 40px;
    									border-radius: 350%;"
    									)
    									)
  				#column(2,actionButton("logOut",label="Save & Log out",icon=icon("save & log out")))
  					)    			
    			   ),  	 		
    			tabPanel("Topics",
  	 			titlePanel("Vanilla LDA"),
  	 					DT::dataTableOutput("vanillaTopics"),
  	 					titlePanel("Interactive LDA"),
  	 					DT::dataTableOutput("interactiveTopics")
  	 			),
  	 			tabPanel("Log-Likelihood",
  	 					plotOutput("vanillaLogLik"),
  	 					plotOutput("interactiveLogLik")
  	 				),
  	 			tabPanel("Coherence",
  	 					plotOutput("vanillaCoherence"),
  	 					plotOutput("interactiveCoherence")
  	 				),
  	 		tabPanel("Debugging", 
  	 			DT::dataTableOutput('foo1'),
  	 			textOutput("foo")
  	 		)
  	 		)
    	#)
    	,
    	conditionalPanel("output.task1=='1'",
    	mainPanel(
    	   uiOutput("evaluation.topic"),
    	   h3(icon("hand-o-down"),"Drop intruding word"),
	    	uiOutput("evaluation.bucket"),
	    	actionButton("clear.eval","Clear"),
	    	actionButton("done.eval.topic","Submit"),
	    	textOutput("foo2")
    	)
    	),
    	conditionalPanel("output.task2=='1'&&(output.task1=='0')",
    	mainPanel(
    	   uiOutput("evaluation.document"),
    	   actionButton("done.eval.document","Submit")
    	)
    	)
    	)))))
    	)
    	)


 