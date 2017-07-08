shinyServer(function(input, output,session) {
						
						
						###All our reactive values are here
						values	<-	reactiveValues(
										annotator="stefanos",
										dataset="sentence",
										display=TRUE,
										load=FALSE,
										loggedOut=FALSE,
										interactionRound=NULL,
										trainingRound=NULL,
										data=NULL,
										docNum=NULL,
										model=NULL,
										currTopics=NULL,
										out1=list(),
										out2=list(),
										#currTopics=currTopics,
										#out1=displayDoc(currTopics,1,corpus,topicMod),
										#out2=displayDoc(currTopics,2,corpus,topicMod),
										trashIn="",
										corpus=NULL,
										#corpus=corpus,
										int.maps=NULL,
										#int.maps=int.maps,
										constraint.matrix=NULL,
										task1=FALSE,
										task2=FALSE,
										#logLik=loglikelihood(topicMod$topics,topicMod$document_sums,topicMod$topic_sums,50/ncol(topicMod$topics),.01),
										#coherence=mean(coherence(topicMod,dtm,10)),
										logLik=NULL,
										coherence=NULL,
										eval.topic=NULL,
										doc.intrusion=NULL,
										selected_intruder=NULL,
										display_doc=NULL,
										docIndex=NULL,
										intruder=NULL,
										counter=1
										)
						
						##These are the starting panels
						output$annotators	<-	 renderUI({
							selectInput("annotator",NULL,c("Select your name"="select","Chris"="chris","Sanjoy"="sanjoy","Stefanos"="stefanos","Michael"="michael"))
						})
						
						output$dataset	<-	 renderUI({
							#selectInput("dataset",NULL,c("Select dataset"="select","Fox News"="foxnews","Cnn"="cnn","Espn"="espn"))
							selectInput("dataset",NULL,c("Select dataset"="select","Cnn"="cnn"))
						})
						
						##Load dataset and start things up
						observeEvent({	
							input$load
							},{
							values$annotator		<-	input$annotator
							values$display			<-	TRUE
							values$dataset			<-	input$dataset
							files					<-	list.files(paste0("../../annotations/",values$dataset,"/objects/"))
							if(length(grep(values$annotator,files))>0){
								files	<-	files[grep(values$annotator,files)]	
								string	<-	files[order(do.call("c",lapply(strsplit(files,"::"),function(x)
								as.POSIXlt(paste(as.Date(x[2],"%a %b %d"),strsplit(x[2]," ")[[1]][4]))
								)),decreasing=TRUE)[1]]
								file					<-	paste0("../../annotations/",values$dataset,"/objects/",string)							
							}else{
								file	<-	paste0("../../annotations/",values$dataset,"/objects/",files[grep("dataObj",files)])
								}
							##print(file)		
							#if(values$annotator%in%list.files(paste0("../annotatorData/",values$dataset))){		
								##
								dataObj				<<-	get(load(file))
									##print(paste("Loaded",values$annotator,"'s file"))
								
								topicMod			<<-	dataObj$vanillaModel
								K					<<-	nrow(topicMod$topics)
								tokenX				<<-	dataObj$tokenX
								curr_lines			<<-	dataObj$rawX
									##print("Computing some stuff. Please wait!")
									
								#withProgress(message="Computing Log-likelihood.", value = 0, {
								#	logLik_vanilla  <<- loglikelihood(topicMod$topics,topicMod$document_sums,topicMod$topic_sums,50/ncol(topicMod$topics),.01, display_progress=TRUE)
								#})
						
								#if(!("logLik"%in%names(dataObj))){
								#	values$logLik			<-	logLik_vanilla
								#}else{values$logLik			<-	dataObj$logLik}
								
								values$corpus			<-	dataObj$corpus
								## Compute coherence new way:
								#coherence_vanilla		<<-	mean(coherence(topicMod,values$corpus$documents,10))
								
								#print(coherence_vanilla)
								#if(!("coherence"%in%names(dataObj))){
								#values$coherence		<-	coherence_vanilla
								#}else{values$coherence	<-	dataObj$coherence}
								
								if(("eval.topic"%in%names(dataObj))){
								values$eval.topic		<-	dataObj$eval.topic		
								}
								
								if(("doc.intrusion"%in%names(dataObj))){
								values$doc.intrusion		<-	dataObj$doc.intrusion		
								}
								
								if(("selected_intruder"%in%names(dataObj))){
								values$selected_intruder		<-	dataObj$selected_intruder		
								}
														
								##print("Should be ok now")
								
								if("interactiveModel"%in%names(dataObj)){
								values$model			<-	dataObj$interactiveModel
								values$int.maps			<-	dataObj$int.maps
								values$interactionRound	<-	dataObj$interactionRound
								}else{
									values$model<-	dataObj$vanillaModel
									if("interactionRound"%in%names(dataObj)){
										values$interactionRound	<-	dataObj$interactionRound
									}else{values$interactionRound<-1}
									
									if("trainingRound"%in%names(dataObj)){
										values$trainingRound	<-	dataObj$trainingRound
									}else{values$trainingRound<-1}
									
									if("counter"%in%names(dataObj)){
										values$counter	<-	dataObj$counter
									}else{values$counter<-1}
									
									}
								
								if("int.maps"%in%names(dataObj)){
								values$int.maps			<-	dataObj$int.maps	
								}else{values$int.maps	<-	initialize.interactive.maps(topicMod$assignments)}
								
								pair					<-	sample(length(values$corpus$documents),2)
								currTopics				<-	unique(c(document.assignments(values$model, pair[1]),document.assignments(values$model,pair[2])))
								valTopicColors			<-	sapply(seq(0.1,1,len=length(currTopics)),function(x)hsv(h=x,s=.7,v=.9))
								names(currTopics)		<-	valTopicColors
								values$currTopics		<-	currTopics
								values$docNum[1]		<-	pair[1]
								values$docNum[2]		<-	pair[2]
								out1					<-	displayDoc(values$currTopics,values$docNum[1],values$corpus,values$int.maps,tokenX)
								out2					<-	displayDoc(values$currTopics,values$docNum[2],values$corpus,values$int.maps,tokenX)
								values$out1				<-	out1
								values$out2				<-	out2
								values$load				<-	TRUE
							})
							
						output$load	<-	renderText({
							if(!values$load){
								'0'
							}
							else{
								'1'
							}							
						})
						
						outputOptions(output,c("load"),suspendWhenHidden=FALSE)	

						
						##Log out and save things out	
						observeEvent({
									input$logOut
									},{
							##saveData
								dataObj$corpus			<-	values$corpus
								dataObj$model			<-	values$model
								dataObj$int.maps			<-	values$int.maps
								dataObj$interactionRound	<-	values$interactionRound
								dataObj$trainingRound		<-	values$trainingRound
								dataObj$counter				<-	values$counter
								dataObj$data				<-	values$data
								#dataObj$coherence			<-	values$coherence
								dataObj$logLik				<-	values$logLik
								dataObj$eval.topic			<-	values$eval.topic
								dataObj $doc.intrusion		<-	values$doc.intrusion	
								dataObj$selected_intruder	<-	values$selected_intruder
														
								
								curr_date					<-	date()
								file						<-	paste0("../../annotations/",values$dataset,"/objects/",values$annotator,"::",curr_date)		
									##print(paste("Saving",values$annotator,"'s file"))
								save(dataObj,file=file)
								values$display	<-	FALSE
								values$load		<-	FALSE
								values$loggedOut<-	TRUE
							})
							
							
							
						output$loggedOut	<-	renderText({
							if(!values$loggedOut){
								'0'
							}
							else{
								'1'
							}							
						})
						
						outputOptions(output,c("loggedOut"),suspendWhenHidden=FALSE)		
						
						##This is needed to move the to feedback panel
						output$display	<-	renderText({
							if(!values$display){
								'0'
							}
							else{
								'1'
							}							
						})
						outputOptions(output,c("display"),suspendWhenHidden=FALSE)	
   						
   						##This is how we display documents
     					output$outDoc1	<-	renderUI({
   											  do.call("div",unname(values$out1))
   											 })
   						
   						output$outDoc2	<-	renderUI({
   											do.call("div",unname(values$out2))
   											})
   						
   						##This is all the trash stuff
   						 	output$trashBucket		<-	renderUI({dropUI("trashBucket",row_n = 10, col_n = 10)})

 							#update document function when things are dropped into trash 
   						update_Doc_trash		<-	eventReactive(input$trashBucket,{
													ids		<-	getDropElements(input$trashBucket,0)
													#ids		<-	sapply(ids,function(x)strsplit(x,";")[[1]][1],USE.NAMES=FALSE)
													#print(sum(!is.null(ids)))
													#print(ids)
													if(sum(!is.null(ids))>0&&(length(na.omit(ids))>0)){
													updated		<-	update_displayDoc(ids,
													values$out1,values$out2,values$docNum[1],values$docNum[2])
													out			<-	list()
													out$slots	<-	updated
													out$ids		<-	ids
													}else{out<-NULL}
													out
													})
														
										observe({
													input$clear
														obj	<-	update_Doc_trash()
														values$out1[names(obj$slots[["1"]])]<- obj$slots[["1"]]
														do.call("div",unname(values$out1))
														values$out2[names(obj$slots[["2"]])]<- obj$slots[["2"]]
  														do.call("div",unname(values$out2))
  														values$trashIn	<-	getDropElements(input$trashBucket,0)	
													})
													
   						##This is what is happening when we hit "trash" 
    					observeEvent(input$trash,{
    							ids		<-	values$trashIn
    							docVec		<-	as.numeric(
    							do.call("rbind",lapply(strsplit(ids,"\\|"),function(x)x[1]))[,1]
								)
								mywords		<-	unique(sapply(1:length(docVec), function(i){
													obj			<-	displayDoc(values$currTopics,docVec[i],values$corpus,values$int.maps,tokenX)
													mynames		<-	na.omit(names(obj))
													values$corpus$vocab[values$corpus$documents[[docVec[i]]][1,match(ids[i],mynames)]+1]
													}))
								##print(mywords)
    							obj					<-	remove.stop.words(values$corpus,mywords,values$int.maps)
    							values$corpus		<-	obj$corpus				
    							values$int.maps		<-	obj$interactive.maps
								out1				<-	displayDoc(values$currTopics,values$docNum[1],values$corpus,values$int.maps,tokenX)
								out2				<-	displayDoc(values$currTopics,values$docNum[2],values$corpus,values$int.maps,tokenX)
								values$out1			<-	out1
								values$out2			<-	out2	
								output$trashBucket	<-	renderUI({dropUI("trashBucket",row_n = 10, col_n = 10)})
								
								obj	<-	list()
								#obj$constraint.matrix	<-	values$constraint.matrix
								obj$trashed				<-	values$trashIn
								save(obj,file=paste0("../annotatorData_clean/",values$dataset,"/post_processing_objects/","feedback_List_",values$interactionRound,"_",values$trainingRound,"_",values$annotator,"_",Sys.time())) 
									
    						})
    						
   						##Here is what happens when we hit next, after some pairs of documents we train
   						observeEvent(input$nextPair,{
   									#print(c(values$counter,values$trainingRound,values$interactionRound))
   									##Get a sample of pairs from the current batch
   										##print(paste("Interaction Round",values$interactionRound))
   									values$counter			<-	values$counter+1
   									pair					<-	sample(length(values$corpus$documents),2)
   									if(values$counter<=steps_btw_training){
										#print(values$counter)

   										values$docNum[1]		<-	pair[1]
   										values$docNum[2]		<-	pair[2]
   									
   									##Get topic colors
   									values$currTopics	<-	unique(c(unname(document.assignments(values$model,values$docNum[1])),unname(document.assignments(values$model,values$docNum[2]))))
									valTopicColors		<-	sapply(seq(0.1,1,len=length(values$currTopics)),function(x)hsv(h=x,s=.7,v=.9))
									names(values$currTopics)<- valTopicColors
										#print(paste("Current topic colors:",values$currTopics))
									##Update document views
									out1				<-	displayDoc(values$currTopics,values$docNum[1],values$corpus,values$int.maps,tokenX)
									out2				<-	displayDoc(values$currTopics,values$docNum[2],values$corpus,values$int.maps,tokenX)
									values$out1			<-	out1
									values$out2			<-	out2	
									
  									values$interactionRound	<- values$interactionRound +1
  										}else{
											#print("Batch done; training model with feedback")
											 withProgress(message="Running Gibbs sampler.", value = 0, {
												int.model	<-	interactive.lda.collapsed.gibbs.sampler.hard(values$corpus$documents,K,values$corpus$vocab,gibbs_iterations,alpha=50/K, eta=.01,values$int.maps, display_progress=TRUE)
											})
											values$model				<- int.model  	
											values$trainingRound		<- 	values$trainingRound+1									
											#values$task1				<-	TRUE
											values$outPairs				<-	NULL
											##print("Computing Likelihood")
											withProgress(message="Computing Log-likelihood.", value = 0, {
												interactive.loglikelihood <- loglikelihood(values$model$topics,values$model$document_sums,matrix(values$model$topic_sums),50/K,.01, display_progress=TRUE)
											})
											
											
											values$logLik	<-	c(values$logLik, interactive.loglikelihood)
											#values$logLik				<-	c(values$logLik,loglikelihood(values$model$topics,values$model$document_sums,matrix(values$model$topic_sums),50/K,.01))
											
											##print("Computing Coherence")
											## Compute coherence old way:
											# values$coherence			<-	c(values$coherence,mean(coherence(values$model,dtm,10)))
											
											## Compute coherence new way:
											#values$coherence			<-	c(values$coherence,mean(coherence(values$model,values$corpus$documents,10)))
											values$counter				<-	1										
										}
									
									##Update Buckets, 	
  					output$feedback		<-	renderUI({
  											foreach(i=1:input$numBuckets)%do%{
  											#dropUI(paste("feedback",i),row_n = 10, col_n = 10)})
  											column(
  											h3(div(icon("hand-o-down"),paste("Feedback Bucket",i))),
  											list(dropUI(paste0("feedback",i),row_n = 10, col_n = 10)),width=3)
  											}
  											})
   							})
   					
   					##This puts out our buckets 					
  					output$feedback		<-	renderUI({
  											foreach(i=1:input$numBuckets)%do%{
  											#dropUI(paste("feedback",i),row_n = 10, col_n = 10)})
  											column(
  											h3(div(icon("hand-o-down"),paste("Feedback Bucket",i))),
  											list(dropUI(paste0("feedback",i),row_n = 10, col_n = 10)),width=3)
  											}
  											}) 	
  					
  					##This clears our buckets
  					
   						observeEvent(input$clear,{
   									# ##print(input$clear)
							# output$outDoc1	<-	renderUI({
								   			  values$out1		<- displayDoc(values$currTopics,values$docNum[1],values$corpus,values$int.maps,tokenX)
								   			 do.call("div",unname(values$out1))
   											 # })
   						
   							# output$outDoc2	<-	renderUI({
   								   			 values$out2		<- displayDoc(values$currTopics,values$docNum[2],values$corpus,values$int.maps,tokenX)
   											do.call("div",unname(values$out2))
   											# })					

  							output$feedback		<-	renderUI({
  											 foreach(i=1:input$numBuckets)%do%{
  											 #dropUI(paste("feedback",i),row_n = 10, col_n = 10)})
  											 column(
  											 h3(div(icon("hand-o-down"),paste("Feedback Bucket",i))),
  											 list(dropUI(paste0("feedback",i),row_n = 10, col_n = 10)),width=3)
  											 }
  											 }) 							
  							output$trashBucket	<-	renderUI({dropUI("trashBucket",row_n = 10, col_n = 10)})
 						})
					
					##This is all our constraint stuff			
					update_Doc				<-	list()
					update_Doc[[1]]			<-	eventReactive(input[["feedback1"]],{
													ids	<-	getDropElements(input$feedback1,0)
													#print(sum(!is.null(ids)))
													if(sum(!is.null(ids))>0){
														#print(ids)
													updated		<-	update_displayDoc(ids,
													values$out1,values$out2,values$docNum[1],values$docNum[2])
													out			<-	list()
													out$slots	<-	updated
													out$ids		<-	ids
													}else{out<-NULL}
													out
													})
					update_Doc[[2]]			<-	eventReactive(input[["feedback2"]],{
													ids	<-	getDropElements(input$feedback2,0)
													#print(sum(!is.null(ids)))
													if(sum(!is.null(ids))>0){
														#print(ids)
													updated		<-	update_displayDoc(ids,
													values$out1,values$out2,values$docNum[1],values$docNum[2])
													out			<-	list()
													out$slots	<-	updated
													out$ids		<-	ids
													}else{out<-NULL}
													out
													})					
					update_Doc[[3]]			<-	eventReactive(input[["feedback3"]],{
													ids	<-	getDropElements(input$feedback3,0)
													#print(sum(!is.null(ids)))
													if(sum(!is.null(ids))>0){
														#print(ids)
													updated		<-	update_displayDoc(ids,
													values$out1,values$out2,values$docNum[1],values$docNum[2])
													out			<-	list()
													out$slots	<-	updated
													out$ids		<-	ids
													}else{out<-NULL}
													out
													})
					update_Doc[[4]]			<-	eventReactive(input[["feedback4"]],{
													ids	<-	getDropElements(input$feedback4,0)
													#print(sum(!is.null(ids)))
													if(sum(!is.null(ids))>0){
														#print(ids)
													updated		<-	update_displayDoc(ids,
													values$out1,values$out2,values$docNum[1],values$docNum[2])
													out			<-	list()
													out$slots	<-	updated
													out$ids		<-	ids
													}else{out<-NULL}
													out
													})
													
					fn_constraint			<-	list()
					fn_constraint[[1]]		<-	eventReactive(input[["feedback1"]],{
													ids		<-	getDropElements(input$feedback1,0)
													#print(ids)
													#print(sum(!is.null(ids)))
													if(sum(!is.null(ids))>0){
														feedbackMatrix 	<- getFeedbackMatrix(ids,values$corpus)
														out				<-	getMustLinkConstraints(feedbackMatrix)
													}else{
														out<-NULL
														feedbackMatrix<-NULL
														}
													list(mustMatrix=out,feedbackMatrix=feedbackMatrix)
													})
					fn_constraint[[2]]		<-	eventReactive(input[["feedback2"]],{
													ids		<-	getDropElements(input$feedback2,0)
													#print(ids)
													#print(sum(!is.null(ids)))
													if(sum(!is.null(ids))>0){
														feedbackMatrix 	<- getFeedbackMatrix(ids,values$corpus)
														out				<-	getMustLinkConstraints(feedbackMatrix)
													}else{
														out<-NULL
														feedbackMatrix<-NULL
														}
													list(mustMatrix=out,feedbackMatrix=feedbackMatrix)
													})
					fn_constraint[[3]]		<-	eventReactive(input[["feedback3"]],{
													ids		<-	getDropElements(input$feedback3,0)
													#print(ids)
													#print(sum(!is.null(ids)))
													if(sum(!is.null(ids))>0){
														feedbackMatrix 	<- getFeedbackMatrix(ids,values$corpus)
														out				<-	getMustLinkConstraints(feedbackMatrix)
													}else{
														out<-NULL
														feedbackMatrix<-NULL
														}
													list(mustMatrix=out,feedbackMatrix=feedbackMatrix)
													})
					fn_constraint[[4]]		<-	eventReactive(input[["feedback4"]],{
													ids		<-	getDropElements(input$feedback4,0)
													#print(ids)
													#print(sum(!is.null(ids)))
													if(sum(!is.null(ids))>0){
														feedbackMatrix 	<- getFeedbackMatrix(ids,values$corpus)
														out				<-	getMustLinkConstraints(feedbackMatrix)
													}else{
														out<-NULL
														feedbackMatrix<-NULL
														}
													list(mustMatrix=out,feedbackMatrix=feedbackMatrix)
													})
													
											observe({
														for(i in 1:input$numBuckets){
															 obj	<-	update_Doc[[i]]()
															 values$out1[names(obj$slots[["1"]])]<- obj$slots[["1"]]
															 values$out2[names(obj$slots[["2"]])]<- obj$slots[["2"]]
														}
  														do.call("div",unname(values$out1))
  														do.call("div",unname(values$out2))
													})
					
					# output$outDoc1		<-		renderUI({
												# for(i in 1:input$numBuckets){
												# obj	<-	update_Doc[[i]]()
												# values$out1[names(obj$slots[["1"]])]<- obj$slots[["1"]]
												# }
  												# do.call("div",unname(values$out1))
   											# })
					
					# output$outDoc2	<-	renderUI({
												# for(i in 1:input$numBuckets){
												# obj	<-	update_Doc[[i]]()
												# values$out2[names(obj$slots[["2"]])]<- obj$slots[["2"]]
												# }
  												# do.call("div",unname(values$out2))
   											# })
   					
				observeEvent(input$add,{	
					##print(values$constraint.matrix)
					obj	<-	list()
					obj$constraint.matrix	<-	values$constraint.matrix
					#obj$trashed				<-	values$trashIn
					save(obj,file=paste0("../../annotations/",values$dataset,"/post_processing_objects/","feedback_List_",values$interactionRound,"_",values$trainingRound,"_",values$annotator,"_",Sys.time())) 	

    										values$int.maps		<-	add.constraints(values$int.maps$component.assignments,values$constraint.matrix,K,values$int.maps,edge.control=TRUE)
    										output$feedback		<-	renderUI({
  											foreach(i=1:input$numBuckets)%do%{
  											#dropUI(paste("feedback",i),row_n = 10, col_n = 10)})
  											column(
  											h3(div(icon("hand-o-down"),paste("Feedback Bucket",i))),
  											list(dropUI(paste0("feedback",i),row_n = 10, col_n = 10)),width=3)
  											}
  											})
							    	})
						
						observe({
									buckets		<-	list()
									mustMatrices	<-	list()	
									for(i in 1:input$numBuckets){
										obj					<-	fn_constraint[[i]]()
										buckets[[i]]		<-	obj$feedbackMatrix
										mustMatrices[[i]]	<-	obj$mustMatrix
									}
									cannot					<-	getCannotLinkConstraints(buckets)
									#print(cannot)
									must							<-	do.call("rbind", mustMatrices)
									interm.matrix	<-	rbind.data.frame(must,cannot)
									values$constraint.matrix	<-	interm.matrix[,-match(c("z_1","z_2"),names(interm.matrix))]
									})

					##These are all our resuls and plots
   					output$vanillaTopics		<-	DT::renderDataTable({
											top.topic.words(topicMod$topics,20)
   											})
   					
   					output$interactiveTopics	<-	DT::renderDataTable({
											model	<-	values$model
											top.topic.words(model$topics,20)
   											})
   					
   					output$vanillaLogLik			<-	renderPlot({
														plot(1:values$trainingRound,rep(logLik_vanilla,values$trainingRound),main="Vanilla LogLikelihood",type="b",ylab="Log-Likelihood",xlab="Rounds of Interaction")
   					})
   					
   					output$interactiveLogLik		<-	renderPlot({
														plot(1:values$trainingRound,values$logLik,main="Interactive LogLikelihood",type="b",ylab="Log-Likelihood",
														xlab="Rounds of Interaction")
   					})
   					
   					output$vanillaCoherence		<-	renderPlot({
   									plot(1:values$trainingRound,rep(coherence_vanilla,values$trainingRound),main="Vanilla Coherence",type="b",ylab="Coherence", xlab="Rounds of Interaction")
																							   
												})
   					
   					output$interactiveCoherence	<-	renderPlot({
												plot(1:values$trainingRound,values$coherence,main="Interactive Coherence",type="b",ylab="Coherence",xlab="Rounds of Interaction") 									
   												})
   					
					##This is the evaluation panel	
					#need this to go to word intrusion panel
					output$task1	<-	renderText({
							if(!values$task1){
								'0'
							}
							else{
								'1'
								}							
						})
							
					outputOptions(output,"task1",suspendWhenHidden=FALSE)											
					
					output$evaluation.topic			<-	renderUI({
													if(!is.null(values$model)){
														#display.eval.task.1(topicMod)
														display.eval.task.1(values$model)
																}
																})
					output$evaluation.bucket		<-	renderUI({
														dropUI("eval.bucket",row_n=10,col_n=10)																																							})
					observeEvent(input$clear.eval,{
						output$evaluation.topic	<-	renderUI({
														#display.eval.task.1(topicMod)
														display.eval.task.1(values$model)
																})
						output$evaluation.bucket		<-	renderUI({
														dropUI("eval.bucket",,row_n=10,col_n=10)
														})
					})
					
					
					observeEvent(input$eval.bucket,{
						          output$foo2	<-	renderText({input$eval.bucket})
					})
					
					output$evaluation.topic			<-	renderUI({
													if(!is.null(values$model)){
														#display.eval.task.1(topicMod)
														display.eval.task.1(values$model)
																}
																})
					observeEvent(input$done.eval.topic
									,{
								
								out					<-	str_trim(str_replace_all(input$eval.bucket,"[\r\n]"," "),side="both")
								split_out			<-	strsplit(out," ")[[1]]
								values$eval.topic	<-	append(values$eval.topic,length(which(split_out=="intruder")))
								##print(values$eval.topic)
								
								output$evaluation.bucket		<-	renderUI({
														dropUI("eval.bucket",,row_n=10,col_n=10)
														})
								
   								values$task1		<-	FALSE
   								values$task2		<-	TRUE
   								})
	
						#need this to go to topic intrusion in document panel
						output$task2	<-	renderText({
							if(!values$task2){
								'0'
							}
							else{
								'1'
								}							
						})
							
						outputOptions(output,"task2",suspendWhenHidden=FALSE)	
   									
   						observeEvent(input$done.eval.document,{
   							
   								values$doc.intrusion	<-	append(values$doc.intrusion,paste(paste0("docIndex:",values$docIndex),paste0("intr_topic:",values$intruding),sep="|"))
								##print(values$doc.intrusion)														
   								M	<-	match(input$eval.document,values$display_doc)
   								##print(M)
   								values$selected_intruder	<-	append(values$selected_intruder,ifelse(names(values$display_doc)[M]=="intruding_topic",1,0))
   								##print(values$selected_intruder)
   								
   							output$evaluation.document		<-	renderUI({
															##sample
															if(!is.null(values$model)){
															docIndex	<-	sample(length(curr_lines),1)
															line		<-	try(fromJSON(curr_lines[docIndex])$html,silent=TRUE)
															myurl		<-	try(fromJSON(curr_lines[docIndex])$url,silent=TRUE)

															while(class(line)=="try-error"){
																docIndex	<-	sample(length(curr_lines),1)
																line		<-	try(fromJSON(curr_lines[docIndex])$html,silent=TRUE)
																myurl		<-	try(fromJSON(curr_lines[docIndex])$url,silent=TRUE)
																}
															mat				<-	document.intruder(docIndex,values$model,50/K,3,8)
															to_display		<-	apply(mat,2,function(x)paste(x,collapse=", "))
															intruding		<-	which(names(to_display)=="intruding_topic")
															values$display_doc	<-	to_display
															values$docIndex		<-	docIndex
															values$intruding		<-	intruding
															##print(to_display)
															urlTag			<-	tags$a(href=myurl,myurl,target="_blank")
															div(urlTag,wellPanel(line,style = "overflow-y:scroll; max-height: 100px"),
															radioButtons("eval.document","Check intruding topic",unname(to_display))
															)
															}
															})
								values$task2		<-	FALSE

   								
   								
   								})	
   											
					output$evaluation.document		<-	renderUI({
															##sample
															if(!is.null(values$model)){
															docIndex	<-	sample(length(curr_lines),1)
															line		<-	try(fromJSON(curr_lines[docIndex])$html,silent=TRUE)
															myurl		<-	try(fromJSON(curr_lines[docIndex])$url,silent=TRUE)

															while(class(line)=="try-error"){
																docIndex	<-	sample(length(curr_lines),1)
																line		<-	try(fromJSON(curr_lines[docIndex])$html,silent=TRUE)
																myurl		<-	try(fromJSON(curr_lines[docIndex])$url,silent=TRUE)
																}
															mat				<-	document.intruder(docIndex,values$model,50/K,3,8)
															to_display		<-	apply(mat,2,function(x)paste(x,collapse=", "))
															intruding		<-	which(names(to_display)=="intruding_topic")
															values$docIndex		<-	docIndex
															values$intruding		<-	intruding
															values$display_doc	<-	to_display
															##print(to_display)
															urlTag			<-	tags$a(href=myurl,myurl,target="_blank")
															div(urlTag,wellPanel(line,style = "overflow-y:scroll; max-height: 100px"),
															radioButtons("eval.document","Check intruding topic",unname(to_display))
															)
															}
															})
															
					
					##This is debugging	
					fn_list					<-		list()
					i<-1
					fn_list[[i]]			<-	eventReactive(input[[paste0("feedback",i)]],{paste(input[[paste0("feedback",i)]])})
					fn_list[[2]]			<-	eventReactive(input[["feedback2"]],{paste(input$feedback2)})
					fn_list[[3]]			<-	eventReactive(input[["feedback3"]],{paste(input$feedback3)})
					fn_list[[4]]			<-	eventReactive(input[["feedback4"]],{paste(input$feedback4)})
					fn_list[[5]]			<-	eventReactive(input[["feedback5"]],{paste(input$feedback5)})   											
					output$foo	<-	renderText({
									unlist(sapply(1:input$numBuckets,function(i){
									fn_list[[i]]()
									}))
									})
								
					output$foo1	<-	DT::renderDataTable({
					
									buckets		<-	list()
									mustMatrices	<-	list()	
									for(i in 1:input$numBuckets){
										obj					<-	fn_constraint[[i]]()
										buckets[[i]]		<-	obj$feedbackMatrix
										mustMatrices[[i]]	<-	obj$mustMatrix
									}
									cannot		<-	getCannotLinkConstraints(buckets)
				
									must							<-	do.call("rbind", mustMatrices)
									interm.matrix	<-	rbind.data.frame(must,cannot)
									values$constraint.matrix	<-	interm.matrix[,-match(c("z_1","z_2"),names(interm.matrix))]
									# mat			<-  do.call("rbind",buckets)
									# docs		<-	unique(mat[,"doc"])
									# #print(sapply(docs,function(doc){
									 			# mywords	<-	mat[,"word"][which(mat[,"doc"]==doc)]
									 			# as.character(values$corpus$vocab[values$corpus$documents[[doc]][1,mywords[1]+1]])
										
									# }))
									#cbind.data.frame(values$corpus$documents[[values$constraint.matrix$doc_1]][1,1], values$corpus$documents[[values$constraint.matrix$doc_2]][1,2])
									
									
									
									})
									
					})	