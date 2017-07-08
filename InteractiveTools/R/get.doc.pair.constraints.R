##	Input: 
##		pair: a pair of documents and their target topic vectors , 
##		feedback: a feedback set of pairs of words and document indices
##	Output:
##		out: a data frame

get.doc.pair.constraints		<-	function(pair,feedback){
	            
	            ###Example pair
	            #sim		<-	 generator.semiSynthetic("../../data/20ngr8.txt",10,.5,.1)
				#pair				<-	 list()
				#pair$indices		<-   c(1,3)
				#pair$strings[[1]]	<-	strsplit(sim$documents[[1]]," ")[[1]]
				#pair$strings[[2]]	<-	strsplit(sim$documents[[3]]," ")[[1]]
				#pair$assignments[[1]]	<-	sim$targetAssignments[[1]]
				#pair$assignments[[2]]	<-	sim$targetAssignments[[3]]
	            ###Feedback is a list as long as the number of groups of pairs. Within pair groups must link, accross group pairs cannot link.
	            ###Example feedback with 3 groups on doc with index 1
	            # feedback				<-	list()
 				# feedback$indices	<-	c(1,1)
 				# feedback$constraints[[1]]		<-	c("company","director","investor","commission")
 				# feedback$constraints[[2]]		<-	c("iraq","oil")
				# feedback$constraints[[3]]		<-	c("payments","reliability","regulators")
 
	            
	            ##Output: must-cannot link constraints
	            
		         
		        ###Private helper function for unique pair generation 
		        expand.grid.unique <- function(x, y, include.equals=FALSE){
						    			x <- unique(x)
										y <- unique(y)
    									g <- function(i){
        									z <- setdiff(y, x[seq_len(i-include.equals)])
										if(length(z)) cbind(x[i], z, deparse.level=0)
    									}

    									do.call(rbind, lapply(seq_along(x), g))
									}
				
				###Private helper processing function for pair output				
				getPairs				<- function(pair,feedback){
										##Input: pair object as described above
										##Output: data frame of must, cannot link constraints and some other info of potential use
										
										###Private helper function for some processing
										process	<- function(words,zs){
													##Input: words, topic assignments for words
													##Output: data frame of information, CAUTION: generates all n^2 pairs, not n choose 2 as it
													###should. To add utility to do that as needed.
													out			<-	cbind.data.frame(words,zs)
			 										names(out)	<- c("word_1","word_2","z_1","z_2")	
			 										out$link	<- rep("",nrow(out))
													out$link	<- ifelse(out$z_1==out$z_2,"must","cannot")
													return(out)
										}

										if(missing(feedback)){
											#print(is.atomic(pair))
										zPairsAccross		<-	expand.grid(pair$assignments[[1]],pair$assignments[[2]])
	           							zPairsWithin_1		<-	t(combn(pair$assignments[[1]],2))
	          							zPairsWithin_2		<-	t(combn(pair$assignments[[2]],2))

										wordPairsAccross	<-	expand.grid(1:length(pair$strings[[1]]),1:length(pair$strings[[2]]))
										wordPairsWithin_1	<-	t(combn(1:length(pair$strings[[1]]),2))
										wordPairsWithin_2	<-	t(combn(1:length(pair$strings[[2]]),2))
										
																				
										out1				<-	process(wordPairsAccross, zPairsAccross)
										#out1$indices	<-	rep(paste(pair$indices,collapse=","),nrow(out1))
										out1$doc_1		<-	rep(pair$indices[1],nrow(out1))
										out1$doc_2		<-	rep(pair$indices[2],nrow(out1))
										
										out2				<-	process(wordPairsWithin_1, zPairsWithin_1)
										#out2$indices	<-	rep(paste(rep(pair$indices[1],2),collapse=","),nrow(out2))
										out2$doc_1		<-	rep(pair$indices[1],nrow(out2))
										out2$doc_2		<-	rep(pair$indices[1],nrow(out2))
										
										out3				<-	process(wordPairsWithin_2, zPairsWithin_2)
										#out3$indices	<-	rep(paste(rep(pair$indices[2],2),collapse=","),nrow(out3))
										out3$doc_1		<-	rep(pair$indices[2],nrow(out3))
										out3$doc_2		<-	rep(pair$indices[2],nrow(out3))
										
										
										out				<-  rbind.data.frame(out1,out2,out3)
										out
										}
										
										if(!missing(feedback)){
										within			<-	do.call("rbind",lapply(feedback$constraints,function(x){
																#out			<-	expand.grid(x,x)
																match(x,)
																out			<-	as.data.frame(t(combn(x,2)))
																names(out)	<- 	c("word_1","word_2")
																out$link		<-	rep("must",nrow(out))
																out
																})
																)
										
										accross			<- do.call("rbind",apply(t(combn(1:length(feedback$constraints),2)),1,function(x){
														    		out	<-	expand.grid(feedback$constraints[[x[1]]],feedback$constraints[[x[2]]])
														    		names(out)	<- 	c("word_1","word_2")
																out$link		<-	rep("cannot",nrow(out))	
											   					out
																})
														    		)
										out				<-	rbind.data.frame(within,accross)
										#out$indices		<-	rep(paste(feedback$indices,collapse=","),nrow(out))
										out$doc_1		<-	rep(feedback$indices[1],nrow(out))
										out$doc_2		<-	rep(feedback$indices[2],nrow(out))
										out	
										}
										return(out)	

										}
    
	            if(missing(feedback)){
	            out		<-	 getPairs(pair)
	            out		
				}
				
				if(!missing(feedback)){
				out		<-	 getPairs(pair,feedback)
	            out		
				}
				return(out)
	            }
