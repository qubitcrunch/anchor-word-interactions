##	Input: 
##		topicModel: a topic model with a $topics field
##		documents: a list of documents as produced by lexicalize or lexicalize2		
##	Output: 
##		coherenceVector: a vector of coherences
coherence	<-	function(topicModel, documents, M=15){
	topWordIndices	<-	t(apply(topicModel$topics, 1, function(x) order(x, decreasing=T)[1:M]))
	
	codocument.frequency	<-	function(word1, word2){
		return(sum(sapply(documents, function(x) (word1 %in% x) && (word2 %in% x), USE.NAMES=FALSE)))
	}
	
	document.frequency	<-	function(word){
		return(sum(sapply(documents, function(x) word %in% x, USE.NAMES=FALSE)))
	}
	
	coherenceVector	<-	apply(topWordIndices, 1, function(x) 
							sum(unlist(sapply(2:M, function(m) 
									unlist(sapply(1:(m-1), function(l) log((codocument.frequency(x[m], x[l]) + 1.0)/document.frequency(x[l]))),use.names=FALSE)), use.names=FALSE))
							)
	
	
	return(coherenceVector)
}
