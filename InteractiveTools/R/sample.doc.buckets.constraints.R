## Input:
##		assignments: assignment list in same format as output by lda.collapsed.gibbs.sampler.
##		num.words: number of words we wish to sample to get all pairwise constraints. Total number of resulting 
##			constraints will be (num.words choose 2).
##		doc_lengths: a vector corresponding to the lengths of documents of assignments. Default value is null.
## Output:
##		constraint.matrix: a constraint matrix of N constraints induced by assignments created by sampling two
##				random documents, and then sampling N uniform constraints.

sample.doc.buckets.constraints		<-	function(assignments, num.words, doc_lengths=NULL){
	if(is.null(doc_lengths)){
		doc_lengths		<-	unlist(lapply(assignments, function(x) length(x)))
	}
	
	num.docs				<-	length(doc_lengths)
	corpus.size				<-	sum(doc_lengths)
	
	doc_distribution			<-	doc_lengths/corpus.size
	
	if(num.docs == 1){
		doc = docs[1]
		sample.single.row		<-	function(){
			out					<-		list()	
			out$doc_1			<-		doc
			out$doc_2			<-		doc
			if(doc_lengths[doc] > 1){
				indices 			<- 		sample(1:doc_lengths[doc_1], 2, replace=FALSE)
				out$word_1			<-		indices[1]
				out$word_2			<-		indices[2]
			}
			if(out$word_1 > out$word_2){
				word			<-		out$word_1
				out$word_1		<-		out$word_2
				out$word_2		<-		word
			}
		
		out$z_1					<-		assignments[[doc_1]][out$word_1]
		out$z_2					<-		assignments[[doc_2]][out$word_2]
		out$link					<-		ifelse(out$z_1 == out$z_2, "must", "cannot")
		return(out)
		}
		constraint.list					<-	lapply(1:num.words, function(x) sample.single.row())
		constraint.matrix				<-	list()
		constraint.matrix$doc_1			<-	unlist(lapply(constraint.list, function(x) x$doc_1))
		constraint.matrix$doc_2			<-	unlist(lapply(constraint.list, function(x) x$doc_2))
		constraint.matrix$word_1			<-	unlist(lapply(constraint.list, function(x) x$word_1))
		constraint.matrix$word_2			<-	unlist(lapply(constraint.list, function(x) x$word_2))
		constraint.matrix$z_1			<-	unlist(lapply(constraint.list, function(x) x$z_1))
		constraint.matrix$z_2			<-	unlist(lapply(constraint.list, function(x) x$z_2))
		constraint.matrix$link			<-	unlist(lapply(constraint.list, function(x) x$link))
		constraint.matrix				<-	unique(as.data.frame(constraint.matrix))
	}
	else{
		sample.docs		<-	sort(sample(num.docs, size=2, replace=FALSE, prob=doc_distribution))
		d.1				<-	sample.docs[1]
		d.2				<-	sample.docs[2]
		doc_1_length		<-	length(assignments[[d.1]])
		doc_2_length		<-	length(assignments[[d.2]])
		
		total.size		<-	doc_1_length + doc_2_length
		
		num_1_words		<-	as.integer(num.words * (doc_1_length/total.size))
		num_2_words		<-	as.integer(num.words - num_1_words)
		
		doc_1_words		<-	c(1:doc_1_length)
		
		if(num_1_words < doc_1_length){
			doc_1_words			<-	sort(sample(doc_1_length, size=num_1_words, replace=FALSE))
		}
		num_1_words		<-	length(doc_1_words)
		
		
		doc_2_words		<-	c(1:doc_2_length)
		if(num_2_words < doc_2_length){
			doc_2_words			<-	sort(sample(doc_2_length, size=num_2_words, replace=FALSE))
		}
		num_2_words		<-	length(doc_2_words)

		
		pair						<-	list()
		pair$indices				<-	c(d.1, d.2)
		pair$strings[[1]]		<-	as.character(assignments[[d.1]][doc_1_words])
		pair$strings[[2]]		<-	as.character(assignments[[d.2]][doc_2_words])
		pair$assignments[[1]]	<-	assignments[[d.1]][doc_1_words]
		pair$assignments[[2]]	<-	assignments[[d.2]][doc_2_words]
	
		constraint.matrix	<-	get.doc.pair.constraints(pair)
		constraint.matrix	<-	constraint.matrix[,c("doc_1","doc_2","word_1","word_2","z_1","z_2","link")]
		
		constraint.matrix$word_1	 <- ifelse(constraint.matrix$doc_1 == d.1, mapvalues(constraint.matrix$word_1, c(1:num_1_words), doc_1_words, warn_missing=FALSE),
																		   mapvalues(constraint.matrix$word_1, c(1:num_2_words), doc_2_words, warn_missing=FALSE))
		constraint.matrix$word_2 <- ifelse(constraint.matrix$doc_2 == d.1, mapvalues(constraint.matrix$word_2, c(1:num_1_words), doc_1_words, warn_missing=FALSE),
																		   mapvalues(constraint.matrix$word_2, c(1:num_2_words), doc_2_words, warn_missing=FALSE))
	}
	return(constraint.matrix)
}