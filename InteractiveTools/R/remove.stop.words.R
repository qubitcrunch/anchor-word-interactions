## Input:
##		corpus: A corpus list in the format output by lexicalize or lexicalize2
##		interactive.maps: An object of the form output by initialize.interactive.maps
##		stop.words: A vector of strings we want to remove from our corpus
## Output:
##		out:
##			$documents
##			$vocab
##			$interactive.maps
remove.stop.words		<-	function(corpus, stop.words, interactive.maps=list()){
	library(plyr)
	
	## Unpack corpus
	vocab			<-	corpus$vocab
	documents		<-	corpus$documents
	active.indices	<-	corpus$active.indices
	
	##	Only consider words in stop.words that appear in our corpus.
	stop.words		<-	unique(stop.words[stop.words %in% vocab])
	
	##	Get indices in vocab.
	stop.indices		<-	as.integer(plyr::mapvalues(stop.words, vocab, c(1:length(vocab)), warn_missing=FALSE))
	
	##	If there are no words to remove, then we exit.
	if(length(stop.indices) <= 0){
		out						<-	list()
		out$corpus				<-	corpus
		out$interactive.maps	<-	interactive.maps
		return(out)
	}
	
	##	Unpack interactive.maps
	must.link.components	<- interactive.maps$must.link.components
	membership				<- NULL
	name_vector				<- NULL
	csize					<- NULL
	no						<- NULL
	
	cannot.link.map			<-	interactive.maps$cannot.link.map
	component.assignments	<-	interactive.maps$component.assignments
	if(!is.null(must.link.components)){
		membership				<-	must.link.components$membership
		name_vector				<-	names(membership)
		csize					<-	must.link.components$csize
		no						<-	must.link.components$no
	}	
	
	##	Remove stop words from vocab.
	vocab			<-	vocab[-stop.indices]
	
	##	Bring down to zero-ordered.
	stop.zero.indices	<-	stop.indices - 1
	
	
	##	Find documents that contain only words in stop.words
	deleted.docs		<-	which(sapply(documents,function(doc) all(doc[1,] %in% stop.zero.indices) ,USE.NAMES=FALSE))
	
	##	Handle deleted.docs
	if(length(deleted.docs) > 0){
		##	Delete deleted.docs from corpus
		documents			<-	documents[-deleted.docs]
		
		##	Delete deleted.docs from active.indices
		if(!is.null(active.indices)){
			active.indices		<-	active.indices[-deleted.docs]
		}
		
		##	Update interactive.maps
		if(!is.null(must.link.components)){
			##	Get the indices of membership vector corresponding to deleted.docs
			membership.indices	<-	which(unlist(sapply(strsplit(name_vector, split="[.]"), function(x) as.integer(x[1]) %in% deleted.docs, USE.NAMES=FALSE),use.names=FALSE))
			##	Get the counts associated with these elements of membership
			table.counts		<-	table(membership[membership.indices])
			affected.components	<-	as.integer(names(table.counts))
			component.counts	<-	unname(table.counts)
		
			##	Subtract off the sizes from csize
			csize[affected.components]	<-	csize[affected.components] - component.counts
			##	Delete indices from membership
			membership			<-	membership[-membership.indices]
		
			##	Get zero-sized components
			deleted.components	<-	which(csize == 0)
			if(length(deleted.components) > 0){
				##	Delete all the zero-size components from cannot.link.map
				cannot.link.map <- cannot.link.map[-deleted.components, -deleted.components]
			
				##	Shift down all the assignments in membership. 
				membership	<-	sapply(membership, function(comp) comp - sum(deleted.components < comp), USE.NAMES=FALSE)	
			
				##	Delete all the zero-size components from $csize.
				csize		<-	must.link.components$csize[-deleted.components]
			
				##	Delete all the zero-size components from component.assignments
				component.assignments	<-	component.assignments[-deleted.components]
			
				##	Update no
				no	<-	length(csize)
			}
		##	Update name_vector
		name_vector	<-	unlist(sapply(1:length(documents), function(d) sapply(1:length(documents[[d]][1,]), function(w) paste(d,w,sep='.'))), use.names=FALSE)
		}
	}
	
	##	Of remaining documents, find ones that contain words in stop.words
	affected.docs	<-	which(sapply(documents,function(doc) any(doc[1,] %in% stop.zero.indices) ,USE.NAMES=FALSE))
		
	##	Handle affected.docs
	if(length(affected.docs) > 0){
		## Delete all stop words from affected.docs
		affected.docs.indices		<-	lapply(documents[affected.docs], function(doc) which(doc[1,] %in% stop.zero.indices))
			
		documents[affected.docs]	<-	lapply(1:length(affected.docs), function(i) documents[[affected.docs[i]]][, -affected.docs.indices[[i]]])
		
		if(!is.null(active.indices)){
			active.indices[affected.docs]	<-	lapply(1:length(affected.docs), function(i) active.indices[[affected.docs[i]]][-affected.docs.indices[[i]]])
		}
		
		##	Update interactive.maps
		if(!is.null(must.link.components)){
			##	Get the indices of membership vector corresponding to stop.indices
			deleted.dws			<-	unlist(sapply(1:length(affected.docs), function(i) 
																			paste(affected.docs[i], 
																			      affected.docs.indices[[i]], 
																			      sep='.'), USE.NAMES=FALSE), use.names=FALSE)
			membership.indices	<-	which(name_vector %in% deleted.dws)
		
			##	Get the counts associated with these elements of membership
			table.counts		<-	table(membership[membership.indices])
		
			affected.components	<-	as.integer(names(table.counts))
			component.counts		<-	unname(table.counts)
			##	Subtract off the sizes from csize
			csize[affected.components]	<-	csize[affected.components] - component.counts
		
			##	Delete indices from membership
			membership			<-	membership[-membership.indices]
		
			##	Get zero-sized components
			deleted.components	<-	which(csize == 0)
		
			if(length(deleted.components) > 0){
				##	Delete all the zero-size components from cannot.link.map
				cannot.link.map <- cannot.link.map[-deleted.components, -deleted.components]
			
				##	Shift down all the assignments in membership. 
				membership	<-	sapply(membership, function(comp) comp - sum(deleted.components < comp), USE.NAMES=FALSE)	
				##	Delete all the zero-size components from $csize.
				csize		<-	must.link.components$csize[-deleted.components]
			
				##	Delete all the zero-size components from component.assignments
				component.assignments	<-	component.assignments[-deleted.components]
			
				##	Update no
				no	<-	length(csize)
			}
			##	Update name_vector
			name_vector	<-	unlist(sapply(1:length(documents), function(d) sapply(1:length(documents[[d]][1,]), function(w) paste(d,w,sep='.'))), use.names=FALSE)
		}
	}
	
	##	Shift down every word in every document.
	documents	<-	lapply(documents, function(doc) matrix( c(sapply(doc[1,], function(word) word - sum(stop.zero.indices < word), USE.NAMES=FALSE), doc[2,]), nrow=2, byrow=TRUE))
	
	##	Pack everything up.
	out	<-	list()
	corpus$documents		<-	documents
	corpus$vocab			<-	vocab
	corpus$active.indices	<-	active.indices
	out$corpus				<-	corpus
	
	if(!is.null(must.link.components)){
		names(membership)					<-	name_vector
		must.link.components$membership		<-	membership
		must.link.components$csize			<-	csize
		must.link.components$no				<-	no
		interactive.maps$must.link.components	<-	must.link.components
		interactive.maps$cannot.link.map			<-	cannot.link.map
		interactive.maps$component.assignments	<-	component.assignments

		out$interactive.maps		<-	interactive.maps
	}
	return(out)
}