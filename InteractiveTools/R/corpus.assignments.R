## Input:
##		membership: the membership vector from must.link.components
##		component.assignments: the component.assignments list
##		num.docs: the number of documents in the corpus
## Output:
##		assignments: a list of assignment vectors
corpus.assignments		<-	function(must.link.components, component.assignments){
	require(plyr)
	membership		<-	must.link.components$membership
	num				<-	must.link.components$no

	##	Get the assignments for the words.
	assignments	<-	plyr::mapvalues(membership, c(1:num), component.assignments, warn_missing=FALSE)
	
	return(assignments)
}
