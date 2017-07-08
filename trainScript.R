require(stringr)
require(tm)
require(lda)
require(foreach)
require(stringr)
require(RJSONIO)


myfiles	<-	list.files("data")
##choose cnn dataset 
dataset	<-	myfiles[1]
lines	<-	readLines(paste0("data/",dataset))
htmls	<-	unname(sapply(lines,function(i)fromJSON(i)$html))
badString		<-	"Most stock quote data provided by BATS. "
badDocs			<-	grep(badString,htmls)
htmls	<-	htmls[-badDocs]	
tokens	<-	sapply(htmls,function(x)strsplit(x," "))
lengths	<-	unlist(lapply(tokens,length),use.names=FALSE)
quants	<-	quantile(lengths)
samples	<-	sample(which((lengths<=quants[4])&(lengths>=quants[2])),2000)

rawX	<-	htmls[samples]
spacedX	<-	lapply(rawX, function(x) paste(strsplit(x,"\\.")[[1]],collapse=". "))

## Source lexicalize2.R (in InteractiveTools/R)
source("InteractiveTools/R/lexicalize2.R")
stop_words	<-	c("cnn",stopwords("SMART"))
tokenX		<-	lapply(spacedX, function(x){
									strsplit(gsub("\n"," ",x)," ")[[1]]
									}
									)

##Collect anchor words for that dataset 
anchor_words	<-	as.character(read.csv(paste0("anchor_words/final_anchor_words/",strsplit(myfiles[1],"\\.")[[1]][1],"/anchor_words.csv"))[,1])
anchor_words	<-	unlist(lapply(strsplit(anchor_words," "),function(x)str_replace_all(x[2],"[[:punct:]]","")))
									
corpus			<-	lexicalize2(tokenX, stop_words)
##Check how many documents have anchor words?
M	<-	match(anchor_words,corpus$vocab)
M	<-	M[which(!is.na(M))]
table(unlist(lapply(corpus$documents,function(x)sum(match(M,x[1,]+1)>0,na.rm=TRUE))))

#replace active indices
for (i in 1:length(corpus$documents)){
	doc_tokens						<-	tokenX[[i]]
	doc_tokens_lower				<-	tolower(str_replace_all(doc_tokens, "[^[:alnum:]]", " "))
	procTokenX						<-	sapply(doc_tokens_lower, function(w) gsub("^\\s+|\\s+$", "", w) ,USE.NAMES=FALSE)
	corpus$active.indices[[i]]		<-	which(!is.na(match(procTokenX,anchor_words)))
}

#train model
K			<- 50
topicMod	<-	lda.collapsed.gibbs.sampler(corpus$documents,K,corpus$vocab,alpha=50/K, eta=.01,num.iterations=500)

##Look at the intersection of topics and anchor words
j			<-	1
top.topic.words(topicMod$topics,100)[,j][which(!is.na(match(top.topic.words(topicMod$topics,100)[,j],anchor_words)))];j<-j+1

dataObj					<-	list()
dataObj$corpus			<-	corpus
dataObj$rawX			<-	rawX
dataObj$spacedX			<-	spacedX
dataObj$tokenX			<-	tokenX
dataObj$vanillaModel	<-	topicMod

curr_date				<-	date()
file					<-	paste0("annotations/",strsplit(dataset,"\\.")[[1]][1],"/","objects/","dataObj::",curr_date)
save(dataObj,file=file)