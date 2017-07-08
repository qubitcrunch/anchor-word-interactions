
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
