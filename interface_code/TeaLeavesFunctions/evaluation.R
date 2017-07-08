
document.intruder	<-	function(document_index, topic_model,alpha,num_topics,num_words){
	##Input: A document index, a topic model, alpha, num of most probable topics to show, num of words under each topic 
	##Output: a list of the 4 most prevalent topics in the document plus an intruder 
					
	theta_mat	<-	function(topic_model,alpha){
		t(apply(topic_model$document_sums, MARGIN=2, function(x) (x + alpha)/(sum(x) + K*alpha)))	
	}
	theta_vectors		<-	theta_mat(topic_model,alpha)
	theta_of_interest	<-	theta_vectors[document_index,]
	true_topics			<-	top.topic.words(topic_model$topics,num_words)[1:num_words,order(theta_of_interest,decreasing=TRUE)[1:num_topics]]
	intruding_topic		<-	top.topic.words(topic_model$topics,num_words)[1:num_words,sample(order(theta_of_interest,decreasing=FALSE)[1:3],1)]
					
	out	<-	cbind(true_topics[,1:num_topics], intruding_topic)[,sample(num_topics+1)]
	#colnames(out)	<-	rep("",num_topics+1)
	out
}
						
					
#document.intruder(10,topicMod,50/K,4,8)



topic.intruder	<-	function(topic_model,num.topic.words=5, relevance.percentage=0.1, relevance.cutoff=10){
	## topic_model: a topic model (either from lda or interactive.lda)
	## num.topic.words: the number of top words to take from the topic model to present the user (default is 5)
	## relevance.percentage: the percentage of top words that are relevant to a particular topic
	## relevance.cutoff: the number of top words to consider from other topics (default is 10)
	require(lda)
	
	K	<-	nrow(topic_model$topics)
	V	<-	ncol(topic_model$topics)
	
	## V x K character matrix
	top.word.matrix	<-	top.topic.words(topic_model$topics, num.words=V)
	
	num.nonintruder.candidates	<-	floor(V * relevance.percentage)
	
	
	## Each column is a vector of words with low probability in that topic.
	intruder.candidates		<-	top.word.matrix[-c(1: num.nonintruder.candidates),]
	
	num.intruder.candidates	<-	nrow(intruder.candidates)
	
	top.intruders		<-	lapply(1:K, function(i){
											intruders	<-	c()
											cutoff		<-	relevance.cutoff
											while(length(intruders) == 0){
												other.top.words	<-	unlist(lapply(setdiff(c(1:K), i), function(j) top.word.matrix[c(1:cutoff),j]), use.names=FALSE)
												intruders 	<-	intersect(intruder.candidates[,i], other.top.words)
												cutoff		<-	min(2*cutoff, num.intruder.candidates)
												}
												return(intruders)
											})
	
	intruder.vector		<-	unlist(lapply(top.intruders, function(x) sample(x, 1)), use.names=FALSE)
	
	name.vector			<-	c(rep("true", num.topic.words), "intruder")
	
	result				<-	lapply(1:K, function(i){
											vec			<-	c(top.word.matrix[c(1: num.topic.words),i], intruder.vector[i])
											names(vec)	<-	name.vector
											sample(vec, length(vec))
										})
	
	return(result)
}


display.eval.task.1	<-	function(model){

							to_display	<-	topic.intruder(model,5,.1,10)			
									out	<-	foreach(i=1:nrow(model$topics))%do%{
																	fluidRow(h4(paste0("Topic ",i)),
																		do.call("div",
																			foreach(j=1:length(to_display[[i]]))%do%{
																			id		<-	paste0("word_",i,"_",j)
																			drag	<-	dragUI(id,to_display[[i]]																		[j],style=paste0("float:left;","width:",150,"px"))
																	drag$children[[2]]<-	span(style="visibility:hidden",names(to_display[[i]])[j])
																	#print(names(to_display[[i]])[i])
																	drag
																	}
																	)
																	,column=10)
																	}
																return(out)
																}
