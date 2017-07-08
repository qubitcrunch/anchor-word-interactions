## Input:
##		assignments: assignment list in same format as output by lda.collapsed.gibbs.sampler.
##		N: number of constraints we wish to sample.
##		doc_lengths: a vector corresponding to the lengths of documents of assignments. Default value is null.
## Output:
##		constraint.matrix: a constraint matrix of N uniformly sampled constraints induced by assignments

uniform.sample.constraints		<-	function(assignments, N, doc_lengths=NULL){
	
	if(is.null(doc_lengths)){
		doc_lengths		<-	unlist(lapply(assignments, function(x) length(x)))
	}
	
	corpus.size			<-	sum(doc_lengths)
	
	doc_distribution	<-	doc_lengths/corpus.size
	
	sample.row			<-	function(){
		roll_doc_1				<- 		rmultinom(1,1, doc_distribution)
		doc_1					<-		which(roll_doc_1==1)
		new_lengths				<-		doc_lengths
		new_lengths[doc_1]		<-		doc_lengths[doc_1] - 1
		new_distribution			<-		new_lengths/(corpus.size - 1)
		roll_doc_2				<- 		rmultinom(1,1, new_distribution)
		doc_2					<-		which(roll_doc_2==1)
		out						<-		list()	
		
		if(doc_1 > doc_2){
			doc					<-		doc_1
			doc_1				<-		doc_2
			doc_2				<-		doc
		}
		
		out$doc_1				<-		doc_1
		out$doc_2				<-		doc_2
		if(doc_1 == doc_2){
			indices 				<- 		sample(1:doc_lengths[doc_1], 2, replace=FALSE)
			out$word_1			<-		indices[1]
			out$word_2			<-		indices[2]
			if(out$word_1 > out$word_2){
				word				<-		out$word_1
				out$word_1		<-		out$word_2
				out$word_2		<-		word
			}
		}
		else{
			index_1 				<- 		sample(1:doc_lengths[doc_1], 1, replace=FALSE)
			index_2 				<- 		sample(1:doc_lengths[doc_2], 1, replace=FALSE)
			out$word_1			<-		index_1
			out$word_2			<-		index_2
		}
		out$z_1					<-		assignments[[doc_1]][out$word_1]
		out$z_2					<-		assignments[[doc_2]][out$word_2]
		out$link				<-		ifelse(out$z_1 == out$z_2, "must", "cannot")
		return(out)
	}
	
	
	constraint.list					<-	lapply(1:N, function(x) sample.row())

	constraint.matrix				<-	list()
	constraint.matrix$doc_1			<-	unlist(lapply(constraint.list, function(x) x$doc_1))
	constraint.matrix$doc_2			<-	unlist(lapply(constraint.list, function(x) x$doc_2))
	constraint.matrix$word_1			<-	unlist(lapply(constraint.list, function(x) x$word_1))
	constraint.matrix$word_2			<-	unlist(lapply(constraint.list, function(x) x$word_2))
	constraint.matrix$z_1			<-	unlist(lapply(constraint.list, function(x) x$z_1))
	constraint.matrix$z_2			<-	unlist(lapply(constraint.list, function(x) x$z_2))
	constraint.matrix$link			<-	unlist(lapply(constraint.list, function(x) x$link))
	
	constraint.matrix				<-	unique(as.data.frame(constraint.matrix))
	
	return(constraint.matrix)
}
