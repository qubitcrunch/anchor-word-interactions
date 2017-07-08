getDropElements	<-	function(x,type){
 								##Input:  drop elements, type: word (1) ,or wordid (0)
 								##Output: clean charater vector of words
 								## and #rows as many as constraints
 								
 								###NEW
 								require(stringr)
 								#print(x)
 								feed				<-	str_trim(str_replace_all(x,"[\r\n]"," "),side="both")
    							feed				<-	stripWhitespace(feed)
    							#print(feed)
    							if(length(feed)>0){
    							splitFeed		<-	strsplit(feed," ")[[1]]
    							feed				<-	splitFeed[which((1:length(splitFeed))%%2==type)]
								#print(feed)
								#print(sum(!is.na(feed)))
								if(length(na.omit(feed))>0){
    								out	<-	feed
    							}
    							else{out<-	NULL}
    							}else{out<-NULL}
    							###OLD
    							if(FALSE){
    							feed				<-	str_trim(str_replace_all(x,"[\r\n]"," "),side="both")
    							feed				<-	stripWhitespace(feed)
    							
    							#print(feed)
    							if(feed!=""){
    								
    								feedOut			<-	str_trim(strsplit(feed," ")[[1]],side="both")
									if(sum(feedOut=="")>0){feedOut	<-	feedOut[feedOut!=""]}	
									feedOut
									#print(feedOut)
    							}
    							else{NULL}
    							}
    							#print(out)
    							out
    							}
    							    							

displayDoc	<-	function(topicAndColors,index,corpus,model,tokenX){
   				##Input: a vector of topics and assigned colors, present in the pair
   				#		 index of document in the pair
   				#		 current corpus
   				#		 current topic model
   				##Output: div and span tags to displayed on the server
   				
   					j	<-	1
   					id	<-	NULL
   					#id_const	<-	NULL
   					#Get words in doc
  					words	<-	tokenX[index][[1]]
  					assignments	<-	document.assignments(model,index)
  					#loop through and find active indices
  					out	<-	foreach(i=1:length(words))%do%{
  						#create draggable elements for active words
   						if(i%in%corpus$active.indices[[index]]){
   							topic_ass	<-	assignments[j]
   							id			<-	append(id,paste(index,i, topic_ass,sep="|"))
   							#id_const	<-	append(id_const,paste(index,j, topic_ass,sep="|"))
            				drag	  	<-	dragUI(id[j],words[i],style=
   							paste0("float:left;","width:",nchar(words[i])*10,"px;","background-color:",names(topicAndColors)[match(topic_ass, topicAndColors)])
												)
							drag$children[[2]]<-	span(style="visibility:hidden",id[j])
							j		<-	j+1
							drag
												}
							#create span elements for non-active words					
							else{
								span(words[i],style=paste0("float:left;","width:",nchar(words[i])*10,"px;"))
								}
            		}
            	   names(out)[corpus$active.indices[[index]]]	<- id	
         		   return(out)
         }


update_displayDoc	<-	function(ids,values_displayDoc1,values_displayDoc2,values_docNum1,values_docNum2){
						## Changes color opacity of selected words 
						##Input: An input drop UI, output of displayDoc, stored in reactive values
						##Output: Updated slots of outputs same to that of displayDoc
						
						##Get the ids
						#ids		<-	getDropElements(inputUI,0)
						#print(ids)
						##initial		<-	strsplit(ids,";")[[1]][1]
						docs		<-	unique(as.numeric(
										unlist(
											lapply(strsplit(ids,"\\|"),function(x)x[1])
										)
										))
						#print(paste("Docs:",docs))
						out			<-	replicate(length(docs),list())
						names(out)	<-	docs							
								for(id in ids){
									#print(id)
										doc		<-	as.numeric(strsplit(id,"\\|")[[1]][1])
										#print(doc)
										#print(values_docNum1)
											if(doc==values_docNum1){
												
												out[["1"]][id]	<-	list(span(unlist(values_displayDoc1[[id]]$children[[1]]),style=paste0(values_displayDoc1[[id]]$attribs$style,";opacity:.5")))
																}
												if(doc==values_docNum2){
												out[["2"]][id]	<-	list(span(unlist(values_displayDoc2[[id]]$children[[1]]),style=paste0(values_displayDoc2[[id]]$attribs$style,";opacity:.5")))
																}
															}
												 out
												  }




if(FALSE){   
which(unlist(lapply(obj[which(unlist(lapply(obj,function(x)x$name))=="div")],function(x)x$children))=="policy")      
c(myobj$attribs$id,unlist(myobj$children))
getIds	<-	function(obj){
			require(XML)
			getId	<-	function(x){
				xmlGetAttr(htmlTreeParse(x)$children$html[["body"]][["div"]],"id")
				}
			lapply(obj,getId)
			}


myids	<-	getIds(obj)
}
colorString		<-	function(docNum,sentenceRange,model,valueldaCorpus,valtopicColors){
	
								#Input: docNum: document Number from Corpus, sentenceRange:range of word positions in sentence, 
								#model: topic modelto get assignments to use for word coloring 
								#Ouput:	out:
								##$ feedbackList: a list of colored words for dragging and dropping
								##$	html: html string to be passed to be shown
								
								require(foreach)
										
								originalString	<-	paste(strsplit(rawX[docNum],"\\.")[[1]][sentenceRange],collapse=" .")	
								originalProc		<-	stripWhitespace(str_replace_all(originalString,
								"[^[:alnum:]]", " "))
								original			<-	strsplit(str_trim(removePunctuation(removeWords(tolower(originalProc),mystopWords)),side="both")," ")[[1]]
								
								if(sum(original=="")>0){original	<-original[original!=""]}	
								
								origSplit	<-	strsplit(tolower(originalProc)," ")[[1]]
								mat			<-	data.frame(matrix(NA,nrow=length(origSplit),ncol=2))
								names(mat)	<-	c("word","assignment")
								mat$word		<-	origSplit
								
								docWords		<-	valueldaCorpus$vocab[(valueldaCorpus$documents[[docNum]][1,]+1)]
								
								matches	<-	sapply(origSplit,function(x){
												which(docWords==x)
											})
								
								wordSequence		<-	rep(NA,length(origSplit))
								index			<-	1
								while(length(matches[[index]])==0){
	 						 			index				<-	index+1
	  									}
								
								wordSequence[index]	<-	matches[[index]][1]
								index				<-	index+1
								for (i in index:length(matches)){
									if(length(matches[[i]])>0){
										currDiff			<-	matches[[i]]-na.omit(rev(wordSequence))[1]

										if(sum(currDiff>0)>0){
											newCurrDiff			<-	currDiff[which(currDiff>0)]
											minCurrDiffIndex		<-	match(min(newCurrDiff),currDiff)
											wordSequence[i]		<-	matches[[i]][minCurrDiffIndex]		
										}
									}
								}									
	 						 
	  	  							 	
								#currAssignments								<-	model$assignments[[docNum]][which(!is.na(wordSequence))]	
								#print(names(model))
								currAssignments								<-	document.assignments(model, docNum)[which(!is.na(wordSequence))]
								mat$assignment[which(!is.na(wordSequence))]	<-	currAssignments
								mat$color	<-	rep("",nrow(mat))
								
							    for(topic in 0:(K-1)){
									mat$color[which(mat$assignment==topic)]	<-	rep(valtopicColors[topic+1],length(which(mat$assignment==topic)))
									mat$color[which(is.na(mat$assignment))]	<- 	rep("white",length(which(is.na(mat$assignment))))
								}
							    
							    split		<-	c(unlist(sapply(strsplit(originalString,"\\.")[[1]],function(x){
            																	out			<- 	strsplit(x," ")[[1]]
            																	out
            																	})))
            					colors		<-	paste("background-color:",mat$color)
            					mat$color	<-	colors
            					mat$doc		<-	rep(docNum,nrow(mat))

            				 	#names(split)	<- rep("",length(split))
								#notColor	<-	c(which(!c(1:length(mat$color))%in%match(original,tolower(mat$word))),c(which(is.na(match(original,tolower(mat$word))))))
								
								# spanOut		<-  foreach(i=c(1:length(split))) %do%{
            										# if(i%in%notColor){span(split[i])}
            										# else{span(split[i],style=colors[i])
            											# }}
            					spanOut		<-	foreach(i=1:nrow(mat))%do%{
            									span(mat$word[i],style=mat$color[i])
            					}
            					
            					feedbackOut				<-	mat[which(!is.na(mat$assignment)),]
            					feedbackOut$position	<-	wordSequence[which(!is.na(mat$assignment))]
            					
            					
            					out						<-	list()
            					out$feedbackList			<-	feedbackOut
            					out$html					<-	spanOut
            							
            					return(out)
            					}
            					
dragSetUIOld	<-	function(feedbackList,id){
					htmltools::tags$div(lapply(1:nrow(feedbackList),function(x,div_id=id){
  					dragUI(paste0(div_id, x), feedbackList$word[x],style=feedbackList$color[x])
 					}),style="width: 250px;
    							height: 250px;
    							border-radius: 250%;
   							background: #FFFFE0;")
 					}  

dragSetUINew	<-	function(feedbackList,id){
					htmltools::tags$div(lapply(1:nrow(feedbackList),function(x,div_id=id){
  					dragUI(paste0(div_id, x),paste0(paste(feedbackList[x,-c(which(names(feedbackList)=="color"))],collapse="|"),";"),style=paste0(feedbackList$color[x],";",
  					"width:100px;font-size:10px;"))
 					}),style="	width: 300px;
    							height: 300;
    							border-radius: 300%;
   								background: #FFFFE0;")
 					}  
 					
getFeedbackMatrix	<-	function(x,values_corpus){
 								##New
 								##Input: ids in a bucket
    							feedbackMatrix	<-do.call("rbind.data.frame",list(t(sapply(	1:length(x),function(i){
    											splitted		<-	strsplit(x[i],"\\|")[[1]]
    											doc				<-	as.numeric(splitted[1])
    											word			<-	as.numeric(splitted[2])
    											word.true		<-	which(values_corpus$active.indices[[doc]]==word)
    											splitted[2]		<-	word.true
    											names(splitted)	<-	names(splitted) 	<-	c("doc","word",	"z")
    											data.frame(doc=as.numeric(splitted["doc"]),word=as.numeric(splitted["word"]),z=as.numeric(splitted["z"]))
    											},USE.NAMES=FALSE)))
    											)
    							if(length(x)==1){
    								feedbackMatrix	<-t(as.data.frame(apply(feedbackMatrix,2,as.numeric)))
    							}else{
    								feedbackMatrix	<-as.data.frame(apply(feedbackMatrix,2,as.numeric))
    							}
    							#print(nrow(feedbackMatrix))
    							##OLD
 								if(FALSE){
 								##Input:  drop elements
 								##Output: a matrix with columns 'z,doc#,word#'
 								## and #rows as many as constraints 
    							feed				<-	str_trim(str_replace_all(x,"[\r\n]"," "),side="both")
    							if(feed!=""){
    								feedbackMatrix	<-	do.call("rbind.data.frame",
    													lapply(strsplit(feed,";")[[1]],function(x){
    													strsplit(x,"\\|")[[1]]
    													})
    												)[,-1]
	
    								names(feedbackMatrix) 		<-	c("z","doc",	"word")
    								rownames(feedbackMatrix)	<-	as.character(1:nrow(feedbackMatrix))
    								feedbackMatrix
    							}
    							else{NULL}
    							}
    							feedbackMatrix
    							}

getMustLinkConstraints	<-	function(feedbackMatrix){
								##NEW
								##Input:  a feedback matrix with columns 'z,doc#,word#'
 								##Output: conversion to must link constraint matrix with 			#rows=choose(#nrowfeedbackMatrix,2)
								##NEW
								if((nrow(feedbackMatrix)>1)){
								combs	<-	t(combn(feedbackMatrix$word,2))
								#print(paste("Combinations:",combs))
    								mustMat	<-	do.call("rbind.data.frame",lapply(1:nrow(combs),function(x){
    								out	<-	cbind.data.frame(
    								feedbackMatrix$doc[which(feedbackMatrix$word==combs[x,1])],
    								feedbackMatrix$doc[which(feedbackMatrix$word==combs[x,2]	)],
    								combs[x,1],combs[x,2],
    								feedbackMatrix$z[which(feedbackMatrix$word==combs[x,1])],
    								feedbackMatrix$z[which(feedbackMatrix$word==combs[x,2])]
    								)
    								names(out)	<-	c("doc_1","doc_2","word_1","word_2","z_1","z_2")
    								out
    								})
    								)
    								mustMat$link	<-	rep("must",nrow(mustMat))
    								#print(mustMat)
    								out	<-	mustMat
    								}
    								else{out<-NULL}
								##OLD
								##Input:  a feedback matrix with columns 'z,doc#,word#'
 								##Output: conversion to must link constraint matrix with 			#rows=choose(#nrowfeedbackMatrix,2)
								#
								if(FALSE){
								print(feedbackMatrix)
								if((nrow(feedbackMatrix)>1)&&(!is.null(feedbackMatrix))){
									combs	<-	t(combn(feedbackMatrix$word,2))
    								mustMat	<-	do.call("rbind.data.frame",lapply(1:nrow(combs),function(x){
    								out	<-	cbind.data.frame(
    								feedbackMatrix$doc[which(feedbackMatrix$word==combs[x,1])],
    								feedbackMatrix$doc[which(feedbackMatrix$word==combs[x,2]	)],
    								combs[x,1],combs[x,2],
    								feedbackMatrix$z[which(feedbackMatrix$word==combs[x,1])],
    								feedbackMatrix$z[which(feedbackMatrix$word==combs[x,2])]
    								)
    								names(out)	<-	c("doc_1","doc_2","word_1","word_2","z_1","z_2")
    								out
    								})
    								)
    								mustMat$link	<-	rep("must",nrow(mustMat))
    								mustMat
    								print(mustMat)
    							}
    							else{NULL}
    							}
    							out
    							}

getCannotLinkConstraints	<-	function(cannotLinkList){
									##OLD	
									##Input: 	List of feedback matrices
 									##Output: 	a matrix of cannot link constraints
 									#!any(is.null(cannotLinkList))
 								
 									if(length(cannotLinkList)>1){
 									cannotLinkList	<-	cannotLinkList[which(!unlist(lapply(cannotLinkList,is.null)))]
 									}
 									out <<- cannotLinkList
									if(length(cannotLinkList)>1){
										
										combs		<-	t(combn(1:length(cannotLinkList),2))
										cannotMat	<-	do.call("rbind.data.frame",lapply(1:nrow(combs),function(x){
											cat(x)
										grid			<-	expand.grid(rownames(cannotLinkList[[combs[x,1]]]),
										rownames(cannotLinkList[[combs[x,2]]])
										)
								
										do.call("rbind.data.frame",lapply(1:nrow(grid),function(xx){
										out		<-	data.frame(doc_1=NA,doc_2=NA,word_1=NA,
										word_2=NA,z_1=NA,z_2=NA)
										out[,c("doc_1","word_1","z_1")]<-cannotLinkList[[combs[x,1]]][grid[xx,1],c("doc","word","z")]
										out[,c("doc_2","word_2","z_2")]<-cannotLinkList[[combs[x,2]]][grid[xx,2],c("doc","word","z")]
										out$link<-"cannot"
										out
										}))
										}))
										cannotMat	<-	na.omit(cannotMat)
										cannotMat
										#print(cannotMat)
										}
										else{list()}
								}								

getConstraints	<-	function(mustLink,cannotLink){
											##NEW
											#if((length(mustLink)||(length(cannotLink))>0)){
												rbind(mustLink,cannotLink)
												#out
												#print(out)
											#}
											#else{
											#	out	<-	matrix("")
											#	out
											#}
											#out
											##OLD
											if(FALSE){
											if(ncol(mustLink)==ncol(cannotLink)){
											out	<-	rbind(mustLink,cannotLink)
											}
											else{
												out<-mustLink
												}
											if(ncol(out)==7){
											out	<-	na.omit(out)
											out
											print(out)
											}
											else{
											matrix("")
											}
											}
										}   		
if(FALSE){ 					
dragSetUINew	<-	function(feedbackList,id){
					htmltools::tags$div(lapply(1:nrow(feedbackList),function(x,div_id=id){
  					dragUI(paste0(div_id, x),processRow(feedbackList[x,]))
 					}),style="width: 300px;
    							height: 300;
    							border-radius: 300%;
   								background: #FFFFE0;")
 					} 
 					

processRow	<-	function(row){
					foreach(i=which(names(row)!="color"))%do%{
					list(span(row$word,style=paste0(row$color,";width: 200px;")),
					span(row[which(names(row!="word"))],style=paste0("background: #FFFFE0;")))
					}
				}
}
#paste(row[-c(which(names(row)=="color"))],collapse="-")

if(FALSE){
colorFun	<-	function(docNum1,docNum2,model,sentenceRange){
	
			require(foreach)
			mycolors		<-	read.csv("../colorList.csv",header=TRUE)
			numTopics	<-	nrow(model$topics)
			topicColors	<-	sample(as.character(mycolors$Hex),numTopics)

			
						
			
			doc_1	<-	list(original=paste(strsplit(rawX[docNum1],"\\.")[[1]][sentenceRange],collapse=" ."),
							 processed=textLines[docNum1],
							 assignments=model$assignments[[docNum1]],
							 color=colorString(paste(strsplit(rawX[docNum1],"\\.")[[1]][sentenceRange],collapse=" ."),docNum1))
							 
			doc_2	<-	list(original=paste(strsplit(rawX[docNum1],"\\.")[[1]][sentenceRange],collapse=" ."),
							 processed=textLines[docNum2],
							 assignments=model$assignments[[docNum2]],
							 color=colorString(paste(strsplit(rawX[docNum2],"\\.")[[1]][sentenceRange],collapse=" ."),docNum2))
			
			
			colorWord	<-	function(doc){
					M	<-	match(
					tolower(strsplit(doc$original," ")[[1]]),
					strsplit(doc$processed," ")[[1]]
					)
					
					mat			<-	cbind.data.frame(strsplit(doc$original," ")[[1]],doc$assignments[M])
					mat$color	<-	rep("",nrow(mat))
										
					if(sum(is.na(mat[,1]))>0){mat<-mat[!is.na(mat[,1]),]}
					for(topic in 0:(numTopics-1)){
					mat$color[which(mat[,2]==topic)]=rep(topicColors[topic+1],length(which(mat[,2]==topic)))
					}
					
					mat$color[which(is.na(mat[,2]))]<- rep("white",length(which(is.na(mat[,2]))))
					doc$color<- mat$color
					return(doc)
					}
			
			#doc_1	<-	colorWord(doc_1)
			#doc_2	<-	colorWord(doc_2)
			
			colors_1		<-	paste("background-color:",doc_1$color)
			colors_2		<-	paste("background-color:",doc_2$color)
            
            mat_1		<-	colorString(docNum1)
            split_1		<-	c(unlist(sapply(strsplit(doc_1$original,"\\.")[[1]],function(x){
            																	out			<- 	strsplit(x," ")[[1]]
            																	out
            																	})))
            names(split_1)	<- rep("",length(split_1))
            notColor			<-	which(is.na(match(split_1,mat$word)))
            span_1		  	<-  foreach(i=c(1:length(split_1))) %do%{
            					if(i%in%notColor){span(split_1[i])}
            					else{span(split_2[i],style=colors_1[i])}
            }
            
            
            split_2		<-	strsplit(removePunctuation(doc_2$original)," ")[[1]]

            out			<-	list(doc_1=(foreach(i=c(1:length(split_1))[-which(is.na(match(split_1,mat$word)))
])%do%{span(split_1[i],style=colors_1[i])}),
            		doc_2=p(foreach(i=1:length(split_2))%do%{span(split_2[i],style=colors_1[i])}))

			return(out)
			}
			}
		