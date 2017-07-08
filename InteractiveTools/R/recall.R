##	Input:
##		assignment: assignment list in same format as output by lda.collapsed.gibbs.sampler
##		true.assignment: assignment list in same format as output by lda.collapsed.gibbs.sampler
##	Output:
##		recall: # of clustering
recall	<-	function(assignment, true.assignment){
	avec			<-	unlist(assignment, use.names=FALSE)
	avec_ref		<-	unlist(true.assignment, use.names=FALSE)
	
	n		<-	length(avec)
	
	mat		<-	as.matrix(unclass(unname(table(avec, avec_ref))))
	
	K_row	<-	nrow(mat)
	K_col	<-	ncol(mat)
	
	# false_pos	<-	sum(sapply(1:K_row, function(k) sum(mat[k,])^2 - sum((mat[k,])^2)))/2
	false_neg	<-	sum(sapply(1:K_col, function(k) sum(mat[,k])^2 - sum((mat[,k])^2)))/2
	
	true_pos		<-	sum(apply(mat,2, function(v) sum(ifelse(v > 0, choose(v,2), 0))))
	# true_neg		<-	choose(n,2) - true_pos - false_pos - false_neg
	
	return (true_pos/(true_pos + false_neg))
}