## Input:
##		model: either a model trained via LDA or interactive LDA.
##		documents: the documents that we want to infer the labels of.
##		
## Output:
##		doc.model
infer.topics		<-	function(model, documents, K, vocab, steps, alpha, eta){
	require(lda)
	if(!(is.null(model$topics) || is.null(model$topic_sums))){
		##	In this case, we simply use the collapsed Gibbs sampler.
		
		
		
		doc.model	<-	lda.collapsed.gibbs.sampler(documents, K, vocab, steps, alpha, eta, initial=list(topics=model$topics,topic_sums=model$topic_sums), freeze.topics=TRUE)
		return(doc.model)
	}else{
		stop("Error: model is missing topics field or topic_sums field.")
	}
}
