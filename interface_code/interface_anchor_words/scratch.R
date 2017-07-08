	#observeEvent(
    	#input$add,{
    		
    		
    		#cannotLinkList	<-	values$feedbackMatrix
    		#print(values$feedbackMatrix)
    		#if(!is.null(cannotLinkList[[1]])){
    		#must				<-		do.call("rbind.data.frame",values$constraintMatrix)
    		#rownames(must)	<-		1:nrow(must)
    		#cannot			<-		getCannotLinkConstraints(cannotLinkList)
    		#values$constraint.matrix		<-	rbind.data.frame(must, cannot[,c("doc_1","doc_2","word_1", "word_2","link")]
			#)
    		#values$int.maps	<-	add.constraints(values$int.maps$component.assignments, values$constraint.matrix, K, values$int.maps, edge.control=TRUE)
    				
    				#mymodel  <-	interactive.lda.collapsed.gibbs.sampler.hard(ldaCorpus$documents, K, ldaCorpus$vocab, 10, alpha=50/K, eta=.01, int.maps.edge.control)
    				#values$model$topic<- mymodel$topics
    		#}			
    	
    		#})



    # observeEvent({
   	 # input[[paste0("drop",max(values$numBuckets))]]
   	 # isolate({
   	# values$numBuckets	<-	values$numBuckets+1
   	# output$moreBuckets	<-	renderUI({
							# #numFun	<-	reactive({input$numBuckets})
							# num	<-	as.numeric(values$numBuckets)   							
   							# out 		<- foreach(i=1:num)%do%{
   							# column(list(dropUI(paste0("drop",i),row_n = 10, col_n = 1)),width=3)
   								# } 
  							# out
  							# })
  							# })
  							# })
  							
 
if(FALSE){
shinyServer(function(input, output,session) {
    
    				


    
    
    values				<-	reactiveValues(docNum1=sample(length(rawX),1),
    									   docNum2=sample(length(rawX),1),
    									   j_1=1,j_2=1,
    									   proc_1=NULL,proc_2=NULL,
    									   outHtml_1=NULL,outHtml_2=NULL,numBuckets=1)  
     
    	
    observeEvent(input[["drop1"]],{
    					output$foo 	<- renderText(input[["drop1"]])
    			})    

    observeEvent(input[["drop2"]],{
    output$foo1 <- renderText(input[["drop2"]])
    })
    
   output$moreBuckets	<-	renderUI({
							numFun	<-	reactive({input$numBuckets})
							num	<-	as.numeric(numFun())   							
   							

   							#num		<-	values$numBuckets
   							out 		<- foreach(i=1:num)%do%{
   								column(list(dropUI(paste0("drop",i),row_n = 4, col_n = 1)),width=3)
   								} 
  							out 								
  							})
                                         		                  
                        					   	
   	
    		
   	    #observeEvent(input$drop1,{
   	    	
   	    #	num		<- values$numBuckets+1 
   	     # 	output$moreBuckets<- renderUI({   	     		
   		#	out <- foreach(i=1:num)%do%{
   		#						column(list(dropUI(paste0("drop",i),row_n = 4, col_n = 1)),width=3)
   		#						} 
  		#			out      		
   		#			})
   		 #  })
 		 	
   
   
   
# #   	observeEvent(input$clear,{
   			   # values$feedback_flag		<- TRUE  				
  			   # updateSelectInput(session,c("drag1","drag2","moreBuckets"))
			   # })
			   
   	
   	# output$clear		<-	renderText({
									 # if(!values$feedback_flag){
										 # '0'
									# }
									 # else{
										 # '1'
									 # }
								 # })
	
	# outputOptions(output, 'clear', suspendWhenHidden=TRUE)
	
   
   			observeEvent(input$nextPair,{
    					values$docNum1	<-	sample(length(rawX),1)
    					values$docNum2	<-	sample(length(rawX),1)
	                    
	                    docNum1			<-	values$docNum1
	                    	docNum2			<-	values$docNum2
	                    	print(c(values$docNum1, values$docNum2))

                        j_1				<-	values$j_1
						j_2				<-	values$j_2    					
												
						range_1	<-	seq(1,length(strsplit(rawX[docNum1],"\\.")[[1]]))
                        range_2	<-	seq(1,length(strsplit(rawX[docNum2],"\\.")[[1]]))
						
						proc_1	<-	colorString(values$docNum1,range_1[c(j_1,j_1+1)],topicMod)$feedbackList[,c("word","color")]
						proc_2	<-	colorString(values$docNum2,range_2[c(j_2,j_2+1)],topicMod)$feedbackList[,c("word","color")]
						
						
						
						observeEvent(input$more1,{
										output$feedbackWords		<-	renderUI({
													fluidRow(dragSetUINew(proc_1,"drag"),
															 dragSetUINew(proc_2,"drag")
															)})
						})
						
						output$outDoc1	<-	renderUI({
   							colorString(values$docNum1,range_1[c(values$j_1,(values$j_1)+1)],topicMod)$html
   						})
   					
   						output$outDoc2	<-	renderUI({
   						   	colorString(values$docNum2,range_2[c(values$j_2,(values$j_2)+1)],topicMod)$html
   						})
						})
												

						
						
						
    					
   						
   						   
   

   
   output$top_words		<-	DT::renderDataTable({
									
							    top.topic.words(values$model$topics,10)
									
								})
   

 
 })
 
  # renderUI({
			# fluidRow(
			# dragSetUINew(list1,"drag"),
			# dragSetUINew(list1,"drag")
			# )}
    		# )
    		}
