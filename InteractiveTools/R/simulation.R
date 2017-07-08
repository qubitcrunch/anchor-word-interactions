source("imports.R")

simulation		<-	function(D, V, Ks, alpha, eta, lambda, word.num, num.iterations=200, gibbs.steps=10, mc.iterations=5, uniform=TRUE, dir_name=NULL){
	require(foreach)
	p					<-	1
	final				<-	list()
	# final[["Err"]]		<-	 list()
	# final[["Acc"]]		<-	 list()
	# final[["Prec"]]		<-	 list()
	# final[["Rec"]]		<-	 list()
	# final[["LogLik"]]		<-	 list()

	print("generating data")
	css	<-	c()
	for (K in Ks){
		dataset 		<-	generator.synthetic(D, V, K, alpha, eta, lambda)
		corpus.size	<-	sum(dataset$doc_lengths)
		corpus 		<-	lexicalize(unlist(dataset$documents))
		num.consts	<-	choose(corpus.size, 2)
		cs			<- 	sum(unlist(lapply(corpus$documents, function(x) ncol(x))))
		objects		<-	foreach::foreach(iter=1: mc.iterations)%do%{
			print(paste("K is", K))
			print("initializations")
			plain		 			<-	lda.collapsed.gibbs.sampler(corpus$documents, K, corpus$vocab, gibbs.steps, alpha, eta)

			int.maps.with.zs			<-	initialize.interactive.maps(plain$assignments)
			int.maps.edge			<-	initialize.interactive.maps(plain$assignments)

			if(uniform){
				constraint.matrix		<-	uniform.sample.constraints(dataset$assignments, word.num, dataset$doc_lengths)
				while(length(unique(constraint.matrix$link))<2){
					constraint.matrix		<-	uniform.sample.constraints(dataset$assignments, word.num, dataset$doc_lengths)
				}
			}
			else{
				constraint.matrix		<-	sample.doc.buckets.constraints(dataset$assignments, word.num, dataset$doc_lengths)
				while(length(unique(constraint.matrix$link))<2){
					constraint.matrix		<-	sample.doc.buckets.constraints(dataset$assignments, word.num, dataset$doc_lengths)
				}
			}

			int.maps.with.zs		<-	add.constraints(int.maps.with.zs$component.assignments, constraint.matrix, K, int.maps.with.zs,edge.control=FALSE)
			int.maps.edge		<-	add.constraints(int.maps.edge$component.assignments, constraint.matrix[,-c(5,6)], K, int.maps.edge,edge.control=TRUE)

			do.call("rbind", foreach::foreach(t=1:num.iterations)%do%{
				print(t)
				##Models
				plain	<-	lda.collapsed.gibbs.sampler(corpus$documents, K, corpus$vocab, gibbs.steps, alpha, eta, initial=list(assignments=plain$assignments))
				zs		<-	interactive.lda.collapsed.gibbs.sampler.hard(corpus$documents, K, corpus$vocab, gibbs.steps, alpha, eta, int.maps.with.zs)
				edge	<-	interactive.lda.collapsed.gibbs.sampler.hard(corpus$documents, K, corpus$vocab, gibbs.steps, alpha, eta, int.maps.edge)
			
				zs.assignments		<- corpus.assignments(int.maps.with.zs$must.link.components, zs$component.assignments)
				edge.assignments		<- corpus.assignments(int.maps.edge$must.link.components, edge$component.assignments)
			
				## Measurable quantities
				outErr	<-	c(error(plain$assignments, dataset$assignments),
							  error(zs.assignments, dataset$assignments),
							  error(edge.assignments, dataset$assignments))		
			
				outAcc	<-	c(accuracy(plain$assignments, dataset$assignments),
							  accuracy(zs.assignments, dataset$assignments),
							  accuracy(edge.assignments, dataset$assignments))
					
				outPrec	<-	c(precision(plain$assignments,dataset$assignments),
							  precision(zs.assignments,dataset$assignments),
							  precision(edge.assignments, dataset$assignments))

				outRec	<-	c(recall(plain$assignments,dataset$assignments),
							  recall(zs.assignments, dataset$assignments),
							  recall(edge.assignments, dataset$assignments))

				outlogLik <-	 c(loglikelihood(plain$topics,plain$document_sums,plain$topic_sums,eta,alpha),
							   loglikelihood(zs$topics,zs$document_sums,as.matrix(zs$topic_sums),eta,alpha),
							   loglikelihood(edge$topics,edge$document_sums,as.matrix(edge$topic_sums),eta,alpha))
				 
				###New Constraints and Maps
				if(uniform){
					constraint.matrix		<-	uniform.sample.constraints(dataset$assignments, word.num, dataset$doc_lengths)
					while(length(unique(constraint.matrix$link))<2){
						constraint.matrix		<-	uniform.sample.constraints(dataset$assignments, word.num, dataset$doc_lengths)
					}
				}
				else{
					constraint.matrix		<-	sample.doc.buckets.constraints(dataset$assignments, word.num, dataset$doc_lengths)
					while(length(unique(constraint.matrix$link))<2){
						constraint.matrix	<-	sample.doc.buckets.constraints(dataset$assignments, word.num, dataset$doc_lengths)
					}
				}
				int.maps.with.zs	 <-	add.constraints(zs$component.assignments, constraint.matrix, K, int.maps.with.zs)
				int.maps.edge	 <-	add.constraints(edge$component.assignments,constraint.matrix[,-c(5,6)], K,int.maps.edge, edge.control=TRUE)

				output			<-	c(outErr, outAcc, outPrec, outRec, outlogLik)
				names(output)	<-	c("Err_lda","Err_interact_zs","Err_interact_edge",
								  	  "Acc_lda","Acc_interact_zs","Acc_interact_edge",
								      "Prec_lda","Prec_interact_zs","Prec_interact_edge",
							  	  	  "Rec_lda","Rec_interact_zs","Rec_interact_edge",
							  	  	  "LogLik_lda","LogLik_interact_zs","LogLik_interact_edge")
			
				output
			})
		}

		final[[p]]	<-	do.call("rbind", lapply(objects, function(x){
						c_df		<-	data.frame(1:nrow(x),x)
						names(c_df)	<-	c("interactions", colnames(objects[[1]]))
						c_df}))
	
		css	<-	c(css,cs)
		p	<-	p+1
	}
	
	names(final)	<-	Ks
	
	##	If filename is not null, then we plot
	if(!is.null(dir_name)){
		require(latex2exp)
		library(ggplot2)
		library(gridExtra)
		library(grid)
		## Define plotting function
		grid_arrange_shared_legend <- function(plots) {
    		# plots <- list(...)
    		g <- ggplot2::ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
    		legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    		lheight <- sum(legend$height)
    		gridExtra::grid.arrange(do.call(gridExtra::arrangeGrob, lapply(plots, function(x) x + theme(legend.position="none"))), legend, ncol = 1, heights = unit.c(unit(1, "npc") - lheight, lheight))
        }
        
        ## Define helper functions
        sd_error	<-	function(x){ (sd(x)/sqrt(length(x))) }
		upper		<-	function(x){ mean(x)+sd_error(x) }
		lower		<-	function(x){ mean(x)-sd_error(x) }
        
        
        methods			<-	c("_lda", "_interact_zs", "_interact_edge")
        proper_measures	<-	c("Error", "Accuracy", "Precision", "Recall", "Log-likelihood")
        abbrev_measures	<-	c("Err", "Acc", "Prec", "Rec", "LogLik")
        for(j in 1:length(abbrev_measures)){
        		measure		<-	abbrev_measures[j]	
        		columns		<-	c("interactions", paste(measure, methods, sep=''))
        		plot_list	<-	lapply(1:length(Ks), function(i){
        							print(columns)
        							K		<-	Ks[i]
        							cs		<-	css[i]
        							currDf	<-	final[[i]][,columns]
        							names(currDf)[-1]	<-	c("lda","interact_zs","interact_edge")
									results		<-	do.call("rbind", lapply(c("lda","interact_zs","interact_edge"), 
											function(model){
												outDF	<-	cbind.data.frame(c(1: num.iterations),
												aggregate(currDf[,model],list(interactions=currDf$interactions),FUN="mean")[,2],
												as.factor(rep(model,num.iterations)))
												names(outDF)	<-	c("interactions","mean","model")
												outDF
									}))
									yaxisName	<-	proper_measures[j]			
									gg	<- ggplot2::ggplot(data=results,aes(x=interactions,y=mean,group=model,colour=model))
									ggg	<- gg + ggplot2::stat_smooth()
									ggg+ylab(yaxisName)+xlab("# Interactions")+labs(title=TeX(paste("K=",K,","," Corpus Size=",cs,sep="")))+theme(axis.title=element_text(color="chocolate"))
	        					})
    	   	 	gConst		<-	grid_arrange_shared_legend(plot_list)
    		    	# rand_int		<-	as.character(sample(1000,1))
        		if(substr(dir_name, nchar(dir_name),nchar(dir_name)) != "/"){
        			dir_name		<-	paste(dir_name,"/",sep="")
        		}
	        	# current_date		<-	gsub("-", "_", Sys.Date())
    		    	png(filename=paste(dir_name, measure, "_wordNum_",word.num,"_gibbsSteps_",gibbs.steps,".png",sep=""))
    	    		grid::grid.draw(gConst)
        		dev.off()
        }
	}
	return(final)
}

# gConst	<-	grid_arrange_shared_legend(final[["Err"]][[1]],final[["Err"]][[2]],final[["Err"]][[3]],final[["Err"]][[4]])
# png(filename=paste("../../Results/",now,"/","Err_word.num_",word.num,"_","gibbs.steps_",gibbs.steps,".png",sep=""))
# grid.draw(gConst)
# dev.off()

# gConst	<-	grid_arrange_shared_legend(final[["Acc"]][[1]],final[["Acc"]][[2]],final[["Acc"]][[3]],final[["Acc"]][[4]])
# png(filename=paste("../../Results/",now,"/","Acc_word.num_",word.num,"_","gibbs.steps_",gibbs.steps,".png",sep=""))
# grid.draw(gConst)
# dev.off()

# gPrec	<-	grid_arrange_shared_legend(final[["Prec"]][[1]],final[["Prec"]][[2]],final[["Prec"]][[3]],final[["Prec"]][[4]])
# png(filename=paste("../../Results/",now,"/","Prec_word.num_",word.num,"_","gibbs.steps_",gibbs.steps,".png",sep=""))
# grid.draw(gPrec)
# dev.off()

# gRec	<-	grid_arrange_shared_legend(final[["Rec"]][[1]],final[["Rec"]][[2]],final[["Rec"]][[3]],final[["Rec"]][[4]])
# png(filename=paste("../../Results/",now,"/","Rec_word.num_",word.num,"_","gibbs.steps_",gibbs.steps,".png",sep=""))
# grid.draw(gRec)
# dev.off()

# gLogLik	<-	grid_arrange_shared_legend(final[["LogLik"]][[1]],final[["LogLik"]][[2]],final[["LogLik"]][[3]],final[["LogLik"]][[4]])
# png(filename=paste("../../Results/",now,"/","LogLik_word.num_",word.num,"_","gibbs.steps_",gibbs.steps,".png",sep=""))
# grid.draw(gLogLik)
# dev.off()

