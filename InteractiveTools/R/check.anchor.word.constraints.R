check.anchor.word.constraints	<-	function(line,token_x){
	
					doc_tokens_1					<-	token_x[[line$doc_1]]
					doc_tokens_2					<-	token_x[[line$doc_2]]
	
					doc_tokens_lower_1			<-	tolower(str_replace_all(doc_tokens_1, "[^[:alnum:]]", " "))
					doc_tokens_lower_2			<-	tolower(str_replace_all(doc_tokens_2, "[^[:alnum:]]", " "))
					procTokenX_1				<-	sapply(doc_tokens_lower_1, function(w) gsub("^\\s+|\\s+$", "", w) ,USE.NAMES=FALSE)
					procTokenX_2				<-	sapply(doc_tokens_lower_2, function(w) gsub("^\\s+|\\s+$", "", w) ,USE.NAMES=FALSE)
					c(procTokenX_1[which(!is.na(match(procTokenX_1,anchor_words)))[line$word_1]],
					procTokenX_2[which(!is.na(match(procTokenX_2,anchor_words)))[line$word_2]],
					line$link)
				}