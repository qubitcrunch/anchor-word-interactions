

vanilla_words			<-	 top.topic.words(vanilla_model$topics,length(anchor_words))
interactive_words		<-	 top.topic.words(interactive_model$topics,length(anchor_words))

line					<-	check.constraints(constraint.matrices[[1]][i,])
vanilla_model$topics[,line[c(1,2)]]
interactive_model$topics[,line[c(1,2)]]
i<-i+1
line


c(sum(apply(vanilla_model$topics[,line[c(1,2)]],1,function(x)x[1]!=0&&x[2]!=0)))
,sum(apply(interactive_model$topics[,line[c(1,2)]],1,function(x)x[1]!=0&&x[2]!=0))
,line[3])
interactive_model$topics[,line[c(1,2)]]
results	<-lapply(1:nrow(constraint.matrices[[1]]),function(i){


	vanilla_score<-do.call("rbind",c(lapply(1:ncol(vanilla_words),function(x)c(which(vanilla_words[,x]==line[1]),which(vanilla_words[,x]==line[2]),line[3]))))
	interactive_score<-do.call("rbind",c(lapply(1:ncol(interactive_words),function(x)c(which(interactive_words[,x]==line[1]),which(interactive_words[,x]==line[2]),line[3]))))



cbind(vanilla_score,interactive_score)
})

load("anchor_words/processed_cooccurrence_10")
mat[match(anchor_words[1],vocab), which(!vocab%in%anchor_words)]

smoothed.topics <- (interactive_model$topics + .01)/rowSums(interactive_model$topics + .01)


other_words_mat	<-	mat[!vocab%in%anchor_words,match(anchor_words,vocab)]
topic_mat		<-	(smoothed.topics%*%t(other_words_mat))
test_mat	<-	(interactive_model$topics[,"truth"]%*%t(mat[which(vocab=="truth"),]))

sapply(1:nrow(smoothed.topics),function(row){
top_1		 <-smoothed.topics[,"apple"]*(mat[match("apple",vocab),])
#colnames(interactive_model$topics)
normalized	 <- top_1/(colSums(top_1) + 1e-05)
vocab[order(normalized[,1],decreasing=TRUE)[1:20]]
}

