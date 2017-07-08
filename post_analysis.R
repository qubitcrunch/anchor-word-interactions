require(lda)
require(tm)
require(stringr)
require(foreach)
require(doSNOW)


myfiles	<-	list.files("data")
setwd("InteractiveTools/R")
source("importsInterface.R")
setwd("../..")
constraint.matrix.files		<-	list.files("annotations/cnn/post_processing_objects")
object.files				<-	list.files("annotations/cnn/objects")

#load data file 
dataObj	<-	get(load(paste0("annotations/cnn/objects/",object.files)))

##Collect anchor words for that dataset 
anchor_words	<-	as.character(read.csv(paste0("anchor_words/final_anchor_words/",strsplit(myfiles[1],"\\.")[[1]][1],"/anchor_words.csv"))[,1])
anchor_words	<-	unlist(lapply(strsplit(anchor_words," "),function(x)str_replace_all(x[2],"[[:punct:]]","")))

##Collect anchor words for that dataset 
##Check how many documents have anchor words?
tokenX								<-	dataObj$tokenX
corpus								<-	dataObj$corpus
M	<-	match(anchor_words,corpus$vocab)
M	<-	M[which(!is.na(M))]
table(unlist(lapply(corpus$documents,function(x)sum(match(M,x[1,]+1)>0,na.rm=TRUE))))

##Anchor word corpus
anchor_words_corpus					<-	list()
anchor_words_corpus$vocab			<-	anchor_words
anchor_words_corpus$documents		<-lapply(1:length(corpus$documents),function(i){
	cat(i,"\n")
	doc_tokens						<-	tokenX[[i]]
	doc_tokens_lower				<-	tolower(str_replace_all(doc_tokens, "[^[:alnum:]]", " "))
	procTokenX						<-	sapply(doc_tokens_lower, function(w) gsub("^\\s+|\\s+$", "", w) ,USE.NAMES=FALSE)
	corpus$active.indices[[i]]		<-	which(!is.na(match(procTokenX,anchor_words)))
	M	<-	match(procTokenX[corpus$active.indices[[i]]],anchor_words)-1
	out	<-	rbind(as.integer(M),as.integer(1))
	
	(out)		
}
)
#check correctness of indices
constraint_matrices	<-	foreach(file=constraint.matrix.files)%do%{
									curr_mat		<-	get(load(paste0("annotations/cnn/post_processing_objects/",file)))$constraint.matrix 
									curr_mat
								}

lapply(1:length(constraint_matrices),function(i){
		cat(i,"\n")
sum(unlist(lapply(1:nrow(constraint_matrices[[i]]),function(j){
					line	<-	constraint_matrices[[i]][j,]
					all.equal(
						c(anchor_words[
							c(anchor_words_corpus$documents[[line$doc_1]][1,line$word_1]+1,
		  					anchor_words_corpus$documents[[line$doc_2]][1,line$word_2]+1)],
		  					line$link
		  					),
		  					check.anchor.word.constraints(constraint_matrices[[i]][j,],tokenX)
		  				)
		  			}
		  			))==0)
				})

##Initialize a model
K				<-	nrow(dataObj$vanillaModel$topics)
vanilla_model	<-	lda.collapsed.gibbs.sampler(anchor_words_corpus$documents,K, anchor_words_corpus$vocab,alpha=50/K, eta=.01,num.iterations=200,compute.log.likelihood=TRUE)
int.maps		<-	initialize.interactive.maps(vanilla_model$assignments)

##train interative models
interactive_models		<-			foreach(i=1:length(constraint_matrices),.packages=c("Matrix","igraph","Rcpp","RcppEigen","lda","plyr","hash"))%do%{
	print(i)
	con.mat				<-	do.call("rbind",constraint_matrices[1:i])
	int.maps			<-	add.constraints(int.maps$component.assignments, con.mat,K,int.maps,edge.control=TRUE)
	interactive_model	<-	interactive.lda.collapsed.gibbs.sampler.hard(anchor_words_corpus$documents,K,anchor_words,num.iterations=200,alpha=50/K, eta=.01,int.maps)
	interactive_model
	interactive_model
	}


if(FALSE){
sapply(1:length(interactive_models),function(j){top.topic.words(interactive_models[[j]]$topics,10)[,2]})[,1:43]

topic	<-	1
cbind(top.topic.words(vanilla_model$topics,10)[,topic],top.topic.words(interactive_models[[46]]$topics,10)[,topic])
topic<-topic+1

coherences	<-	lapply(c(10,15,20,30),function(x){
	cat(x,"\n")
int_coh	<-	coherence(interactive_model,corpus$documents,M=x)
van_coh	<-	coherence(vanilla_model,corpus$documents,M=x)
cbind(int_coh,van_coh)
})
# out	<-	foreach(i=1:5)%do%{
# print(i)

# vanilla_model	<-	lda.collapsed.gibbs.sampler(corpus$documents,K,corpus$vocab,alpha=50/K, eta=.01,num.iterations=50)
# int.maps		<-	initialize.interactive.maps(vanilla_model$assignments)

# van.coh			<-	coherence(vanilla_model,corpus$documents,M=10)
# int.coh			<-	list()
# #counter			<-	1
# con.mat			<-	NULL
# foreach(file in constraint.matrix.files){
	# #print(counter)
	# curr.mat			<-	get(load(paste0("annotatorData_clean/cnn/post_processing_objects/",file)))$constraint.matrix
	# con.mat				<-	rbind(con.mat, curr.mat)
	# int.maps			<-	add.constraints(int.maps$component.assignments,curr.mat,K,int.maps,edge.control=TRUE)
	# interactive_model	<-	interactive.lda.collapsed.gibbs.sampler.hard(corpus$documents,K,corpus$vocab,num.iterations=50,alpha=50/K, eta=.01,int.maps)
	# #int.coh[[counter]]	<-	coherence(interactive_model,corpus$documents,M=10)
	# #counter				<-	counter+1
# }

# stopCluster(cl)
# list(int.coh,van.coh)
# }


# #plot(unlist(lapply(int.coh,mean)),type="l",ylab="Coherence at 10 words",xlab="Interactive Round")
# #points(rep(mean(van.coh),length(int.coh)),col=2,type="l")
# #legend("bottomright",c("Interactive Coherence","Vanilla Coherence"),col=c(1,2),lty=1)
}

