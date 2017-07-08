## Input:
##		model: either a model trained via LDA or interactive LDA
##		doc: the document index we want the assignments for
## Output:
##		doc.assignments: the assignment vector for the words in this document.
document.assignments		<-	function(model, doc){
	require(plyr)
	##	Check if it is an interactive LDA model
	if(!is.null(model$must.link.components)){
		
		membership		<-	model$must.link.components$membership
		num				<-	model$must.link.components$no
	
		##	Grab the components that belong to the document's words
		doc.components	<-	membership[unlist(lapply(strsplit(names(membership), split="[.]"), function(x) x[1] == as.character(doc)), use.names=FALSE)]

		##	Get the assignments for the words.
		doc.assignments	<-	plyr::mapvalues(doc.components, c(1:num), model$component.assignments, warn_missing=FALSE)

		return(doc.assignments)
	} ## Check if it is an LDA model
	else if(!is.null(model$assignments)){
		return(model$assignments[[doc]])
	}
	else{
		stop("Error: model is not of the appropriate type.")
	}
}
