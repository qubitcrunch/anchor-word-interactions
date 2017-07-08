
					fn_list					<-		list()
					i<-1
					fn_list[[i]]			<-	eventReactive(input[[paste0("feedback",i)]],{paste(input[[paste0("feedback",i)]])})
					fn_list[[2]]			<-	eventReactive(input[["feedback2"]],{paste(input$feedback2)})
					fn_list[[3]]			<-	eventReactive(input[["feedback3"]],{paste(input$feedback3)})
					fn_list[[4]]			<-	eventReactive(input[["feedback4"]],{paste(input$feedback4)})
					fn_list[[5]]			<-	eventReactive(input[["feedback5"]],{paste(input$feedback5)})
					
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
														feedbackMatrix 	<- getFeedbackMatrix(ids)
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
														feedbackMatrix 	<- getFeedbackMatrix(ids)
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
														feedbackMatrix 	<- getFeedbackMatrix(ids)
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
														feedbackMatrix 	<- getFeedbackMatrix(ids)
														out				<-	getMustLinkConstraints(feedbackMatrix)
													}else{
														out<-NULL
														feedbackMatrix<-NULL
														}
													list(mustMatrix=out,feedbackMatrix=feedbackMatrix)
													})