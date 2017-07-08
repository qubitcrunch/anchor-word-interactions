require(Matrix)
require(plyr)
## Input:
##		documents:			same format as for lda.collapsed.gibbs.sampler
##		K:					same format as for lda.collapsed.gibbs.sampler
##		vocab:				same format as for lda.collapsed.gibbs.sampler
##		num.iterations:		same format as for lda.collapsed.gibbs.sampler
##		alpha:				same format as for lda.collapsed.gibbs.sampler
##		eta:					same format as for lda.collapsed.gibbs.sampler
##		interactive.maps:	produced by a call to add.constraints.and.create.initialization.
## Output:
##		model:			same format as for lda.collapsed.gibbs.sampler


interactive.lda.collapsed.gibbs.sampler.hard		<-	function(documents, K, vocab, num.iterations, alpha, eta, interactive.maps, display_progress=FALSE){
	library(Matrix)
	D	<- length(documents)
	V	<- length(vocab)
	
	must.link.components	<-	interactive.maps$must.link.components
	cannot.link.map			<-	interactive.maps$cannot.link.map
	component.assignments	<-	interactive.maps$component.assignments
	
	document.vector		<-	unlist(lapply(1:D, function(j) documents[[j]][1,]), use.names=FALSE)
	document.rep.vector	<-	unlist(lapply(1:D, function(j) rep.int(j, length(documents[[j]][1,]))), use.names=FALSE)
	assignment.vector	<-	component.assignments[must.link.components$membership]
	
	membership.vector	<-	unname(interactive.maps$must.link.components$membership)
	
	##	topics is a K x V matrix
	topic_matrix 	<- Matrix::sparseMatrix(assignment.vector + 1, document.vector + 1, x=1, dims=c(K, V))
	topics			<- as.matrix(topic_matrix)
	
	##	topic_sums is a length K vector
	topic_sums			<-	rowSums(topics)

	##	document_sums is a K x D matrix
	document_sum_matrix	<-	Matrix::sparseMatrix(assignment.vector + 1, document.rep.vector, x=1, dims=c(K, D))
	document_sums		<-	as.matrix(document_sum_matrix)

	##	component.doc.matrix is a sparse D x must.link.components$no matrix
	component.doc.matrix	<-	Matrix::sparseMatrix(document.rep.vector, membership.vector, x=1, dims=c(D, must.link.components$no))

	##	component.doc.matrix is a sparse V x must.link.components$no matrix
	component.word.matrix	<-	Matrix::sparseMatrix(document.vector+1, membership.vector, x=1, dims=c(V, must.link.components$no))

	component.sizes		<-	must.link.components$csize	
	
	component.neighbor.vectors	<-	as(cannot.link.map, "dgCMatrix")
	
	##print("About to run C++ code.")
	## Run the Gibbs sampler.
	component.model	<- runGibbsCpp(component.assignments, component.doc.matrix, component.word.matrix, component.sizes, component.neighbor.vectors, topics, topic_sums, document_sums, alpha, eta, num.iterations, display_progress)
	##print("Finished running C++ code.")
	colnames(component.model$topics)	<- 	vocab
	
	
	storage.mode(component.model$topics) <- "integer"
	
	component.model$topic_sums <- matrix(as.integer(component.model$topic_sums), ncol=1)
	
	
	component.model$must.link.components	<-	must.link.components
	return (component.model)
}