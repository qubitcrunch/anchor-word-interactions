##	Input:
##		tokenX: a list of character vectors, where each element is a document.
##		stop_words: 
##	Output:
##		out: 
##			$vocab:
##			$documents:
##			$active.indices:
lexicalize2	<-	function(tokenX, stop_words, stemming=FALSE){
	require(tm)
	require(stringr)
	require(plyr)
	require(SnowballC)
		
	## Introduce space after '.' 
	# spacedX		<-	lapply(rawX, function(x) paste(strsplit(x,"\\.")[[1]],collapse=". "))
	
	## Tokenize the corpus	
	# tokenX		<-	lapply(spacedX, function(x) strsplit(x," ")[[1]])
	
	## Remove all the 's from the corpus
	tokenX  			<-  lapply(tokenX, function(x) gsub("'s", "", x, fixed = TRUE))
		
	## Delete all non-alphanumeric characters and make everything lower-case
	lowerX				<-	lapply(tokenX, function(x) tolower(str_replace_all(x, "[^[:alnum:]]", " ")))
	
	lower_stop_words		<-	sapply(stop_words, function(x) tolower(str_replace_all(x, "[^[:alnum:]]", " ")), USE.NAMES=FALSE)
	
	## Remove numbers
	lowerX		<-	lapply(lowerX,removeNumbers)
	
	lower_stop_words		<-	sapply(lower_stop_words, removeNumbers, USE.NAMES=FALSE)
	
	## Remove 
	
	
	##	Remove white space
	procTokenX	<-	lapply(lowerX, function(x) sapply(x, function(w) gsub("^\\s+|\\s+$", "", w) ,USE.NAMES=FALSE))
	
	proc_stop_words	<-	sapply(lower_stop_words, function(x) sapply(x, function(w) gsub("^\\s+|\\s+$", "", w) ,USE.NAMES=FALSE), USE.NAMES=FALSE)
	
	##	Remove stop words
	finalX		<-	lapply(procTokenX, function(x) plyr::mapvalues(x, 
																   from=proc_stop_words,
																   to=rep("", length(stop_words)),
																   warn_missing=FALSE))
	##	If stemming is true, use SnowballC's function wordStem
	if(stemming){
		finalX	<-	lapply(finalX, function(x) SnowballC::wordStem(x))
	}
	
	active.indices	<-	lapply(finalX, function(x) which(x!=""))
	
	
	compressedX	<-	lapply(finalX, function(x) x[x!=""])
	
# # corpus		<-	Corpus(VectorSource(procX))
	# corpus		<-	tm_map(corpus,removePunctuation)
	# corpus		<-	tm_map(corpus,removeWords, stop_words)
	# corpus		<-	tm_map(corpus,stripWhitespace)
	# textLines	<-	str_trim(unlist(lapply(corpus,function(x)x$content)),side="both")
	# splitLines	<-	sapply(textLines, function(x) strsplit(x, " "), USE.NAMES=FALSE)
	
	vocab		<-	unique(unlist(sapply(compressedX, function(x) unique(x), USE.NAMES=FALSE), use.names=FALSE))
	v_size		<-	length(vocab)
	
	documents	<-	lapply(compressedX, function(x) matrix(data=as.integer(c(plyr::mapvalues(x,
																				from=vocab,
																				to=c(0:(v_size-1)),
																				warn_missing=FALSE),
													  	  				   rep(1, length(x)))),
													  	  nrow=2,
													  	  byrow=TRUE)) 
	
	
	
	corpus					<-	list()
	corpus$vocab			<-	vocab
	corpus$documents		<-	documents
	corpus$active.indices	<-	active.indices
	return (corpus)
}