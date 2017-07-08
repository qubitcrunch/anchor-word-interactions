generator.synthetic	<- function(D, V, K, alpha, eta, lambda){
	##Input: D number of docs, V size of vocab, K, num topics, dirichlet params alpha, eta, poisson mean for doc length	
	##Output: list of length D. First element contains the generated doc, the second word-topic assigns
				
	rdirichlet	<-	function(rows, cols, param){
		return(do.call("rbind", lapply(1:rows, function(i){ 
												Xs	<-	rgamma(cols, param)
												Xs/sum(Xs)
										 	})))
	}
				
				
	doc_lengths		<-	rpois(D,lambda) + 1
						
	thetas	<-	rdirichlet(D,K,alpha)
	phis	<-	rdirichlet(V,K,eta)
						
	docs		<-	lapply(1:D, function(d){
							l	<-	doc_lengths[d]
							pairs	<-	sapply(1:l,function(x){
											roll_z		<- rmultinom(1,1,thetas[d,])
											z			<-	which(roll_z==1)
											roll_word	<-	rmultinom(1,1,phis[,z])
											word			<-	which(roll_word==1)
											list(word=word,topic=(z-1)) ## 0-ordered topic assignments
										})
							out	<-	list()
							out$document			<-	paste(unlist(pairs[1,]),collapse=" ")	
							out$targetassignments	<-	unlist(pairs[2,])
							return(out)
												})
	out				<-	list()
	out$doc_lengths	<-	doc_lengths
	out$documents	<-	lapply(docs, function(d) d$document)
	out$assignments	<-	lapply(docs, function(d) d$targetassignments)
	return(out)
}