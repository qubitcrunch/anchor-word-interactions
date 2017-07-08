require(lda)
require(tm)
require(stringr)
require(foreach)
require(doSNOW)


myfiles	<-	list.files("data")
setwd("InteractiveTools/R")
source("importsInterface.R")
setwd("../..")
constraint.matrix.files			<-	list.files("annotations/cnn/post_processing_objects")
constraint.matrix.files_stef	<-	constraint.matrix.files[grep("stefanos",constraint.matrix.files)]
object.files					<-	list.files("annotations/cnn/objects")
#load data file 
dataObj	<-	get(load(paste0("annotations/cnn/objects/",object.files)))

constraint_matrices	<-	foreach(file= constraint.matrix.files_stef)%do%{
									curr_mat		<-	get(load(paste0("annotations/cnn/post_processing_objects/",file)))$constraint.matrix 
									curr_mat
								}

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



constraint_matrices	<-	do.call("rbind", constraint_matrices)
K					<-	50
vanilla_model		<-	lda.collapsed.gibbs.sampler(anchor_words_corpus$documents,K, anchor_words_corpus$vocab,alpha=50/K, eta=.01,num.iterations=200,compute.log.likelihood=TRUE)
int.maps			<-	initialize.interactive.maps(vanilla_model$assignments)
myseq				<-	c(seq(1,nrow(constraint_matrices),20),nrow(constraint_matrices))
myseq				<-	myseq[-c(1)]

big_results	<-	lapply(1:10, function(i){
constraint_matrices	<-	constraint_matrices[sample(1:nrow(constraint_matrices)),]
results	<-	lapply(myseq,function(k){
	#cat(k,"\n")
	curr_con_mat		<-	constraint_matrices[1:(myseq[which(myseq==k)]-1),]
	int.maps			<-	add.constraints(int.maps$component.assignments, curr_con_mat,K,int.maps,edge.control=TRUE)
	interactive_model	<-	interactive.lda.collapsed.gibbs.sampler.hard(anchor_words_corpus$documents,K,anchor_words,num.iterations=200,alpha=50/K, eta=.01,int.maps)
	
	if(k!=myseq[length(myseq)]){
	hold_out			<-	constraint_matrices[myseq[which(myseq==k)]:myseq[length(myseq)],]
	
	const_for_eval_interactive	<-	get.constraints.for.eval(interactive_model,hold_out)
	const_for_eval_vanilla		<-	get.constraints.for.eval(vanilla_model,hold_out)

	must_interactive	<-	const_for_eval_interactive[const_for_eval_interactive[,"link"]=="must",]
	cannot_interactive	<-	const_for_eval_interactive[const_for_eval_interactive[,"link"]=="cannot",]

	interactive_mistakes<-	(c(sum(unlist(must_interactive[,1])!=unlist(must_interactive[,2])),
	sum(unlist(cannot_interactive[,1])==unlist(cannot_interactive[,2]))))/c(sum(hold_out$link=="must"),sum(hold_out$link=="cannot"))
	names(interactive_mistakes)	<-	c("int_must","int_cannot")

	must_vanilla		<-	const_for_eval_vanilla[const_for_eval_vanilla[,"link"]=="must",]
	cannot_vanilla		<-	const_for_eval_vanilla[const_for_eval_vanilla[,"link"]=="cannot",]

	vanilla_mistakes 	<-	(c(sum(unlist(must_vanilla[,1])!=unlist(must_vanilla[,2])),
	sum(unlist(cannot_vanilla[,1])==unlist(cannot_vanilla[,2]))))/c(sum(hold_out$link=="must"),sum(hold_out$link=="cannot"))

	names(vanilla_mistakes)	<-	c("van_must","van_cannot")
	out <- c(interactive_mistakes,vanilla_mistakes)
	cat(out,"\n")
	}
	out
})
})



saveRDS(big_results,file="avg_evaluation_script")

must	<-	do.call("rbind",lapply(results,function(x)x[c(1,3)]))
plot(myseq[-32],unlist(must[,1]),type="b",col="2",ylim=c(.3,.7),ylab="Proportion of incorrect constraints",xlab="# constraints provided",main="Must Link")
points(myseq[-32],unlist(must[,2]),type="b")







