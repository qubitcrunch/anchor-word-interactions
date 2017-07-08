get.constraints.for.eval		<-	function(model, hold_out_set){
	t(sapply(1:nrow(hold_out_set),function(i){
	cat(i,"\n")
	line				<-	hold_out_set[i,]
	doc_assignments_1			<-	document.assignments(model,unlist(line[1]))
	doc_assignments_2			<-	document.assignments(model,unlist(line[2]))
	df							<-data.frame(doc_assignments_1[unlist(line[3])],doc_assignments_2[unlist(line[4])],line[5])
	names(df)[c(1,2)]			<-	c("ass_1","ass_2")
	df
	}
	))
	}
