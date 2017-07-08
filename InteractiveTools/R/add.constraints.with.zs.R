## Input:
##		assignments: assignment list in format for interactive.lda.collapsed.gibbs.sampler
##		new.constraint.matrix: list generated in the format according to getConstraints().
##		K:
##		must.link.components: 
##		cannot.link.map:
##		int.maps:
## Output:
##		out: 
##			$component.assignments: 
##			$must.link.components
##			$cannot.link.graph:
##			$known.assignments

add.constraints.with.zs		<-	function(component.assignments, new.constraint.matrix, K, interactive.maps){	
	must.link.components 		<- 	interactive.maps$must.link.components
	name.vector					<-	names(must.link.components$membership)
	cannot.link.map				<-	interactive.maps$cannot.link.map
	known.assignments			<-	interactive.maps$known.assignments
	
	# cat("(Add) All constraints obeyed at beginning of method: ", obeys.constraints(component.assignments, cannot.link.map), "\n")
	## Grab all the cannot.link.constraints
	# print("Grab all the cannot.link.constraints")
	cannot.link.constraints		<-	new.constraint.matrix[new.constraint.matrix$link == "cannot",]
	
	if(length(cannot.link.constraints$link) > 0){
		## Add all the cannot.link.constraints
		a_vector		<-	unname(sapply(1:length(cannot.link.constraints$link), function(i) 
			must.link.components$membership[paste0(c(as.character(cannot.link.constraints$doc_1[i]),".",as.character(cannot.link.constraints$word_1[i])), collapse='')], USE.NAMES=FALSE)
		)
		b_vector		<-	unname(sapply(1:length(cannot.link.constraints$link), function(i) 
			must.link.components$membership[paste0(c(as.character(cannot.link.constraints$doc_2[i]),".",as.character(cannot.link.constraints$word_2[i])), collapse='')], USE.NAMES=FALSE)
		)
	
		indices	<-	unique(rbind(unname(cbind(a_vector, b_vector)), unname(cbind(b_vector, a_vector))))
		cannot.link.map[indices]		<-	TRUE
	}

	## Grab all the must.link.constraints
	# print("Grab all the must.link.constraints")
	must.link.constraints			<-	new.constraint.matrix[new.constraint.matrix$link == "must",]
	
	if(length(must.link.constraints$link) > 0){
			# cat("# of must.link.constraints: ", length(must.link.constraints$link), "\n")
			## must.link.pairs has an entry for each must.link.constraint corresponding to the pairs of components that must be merged.
			## Turns integers to characterss
			must.link.pairs				<-	lapply(1:length(must.link.constraints$link), function(i) 
			c(as.character(must.link.components$membership[paste0(c(as.character(must.link.constraints$doc_1[i]),".",as.character(must.link.constraints$word_1[i])), collapse='')]),
			 as.character(must.link.components$membership[paste0(c(as.character(must.link.constraints$doc_2[i]),".",as.character(must.link.constraints$word_2[i])), collapse='')])))
	
	
		# print("iGraph stuff")
		## components.to.collapse represents those components that are connected by new must.link.edges
		el	<-	do.call("rbind", lapply(must.link.pairs, embed, 2))
		gg	<-	graph.edgelist(el, directed=FALSE)
		char.components.to.collapse		<-	split(V(gg)$name, clusters(gg)$membership)
	
		##	Remove singleton elements.
		char.components.to.collapse 		<-	char.components.to.collapse[which(sapply(char.components.to.collapse, function(x) length(x) > 1, USE.NAMES=FALSE))]
		# print("char.components.to.collapse: ")
		# print(char.components.to.collapse)
	
	
		if(length(char.components.to.collapse) > 0){
		## For each component list, strip the v's and sort. 
		# print("For each component list, strip the v's and sort.")
		# print("convert to integers")
			components.to.collapse		<-	unname(lapply(char.components.to.collapse, function(x) sort(as.integer(x))))
		
			# print("components.to.collapse: ")
			# print(components.to.collapse)
	
			## Choose the first element of each component to be the representative of that component.
			saved.representatives		<-	unlist(lapply(components.to.collapse, function(vector) vector[1]),use.names=FALSE)
	
			component.representatives	<-	unlist(lapply(components.to.collapse, function(vector) vector[1]),use.names=FALSE)
			# cat("component.representatives: ", component.representatives, "\n")
	
			## For each component, grab all the members that do not belond to the representative and assign them to the representative.
			component.member.list	<-	lapply(components.to.collapse, function(vector) which(must.link.components$membership %in% vector[-1]))
			
			rep.rep.vector			<-	unlist(lapply(1:length(component.representatives), function(i) rep.int(component.representatives[i], length(component.member.list[[i]]))), use.names=FALSE)
			
			
			must.link.components$membership[unlist(component.member.list, use.names=FALSE)]	<-	rep.rep.vector
	
			# print("Add all the sizes of the component.")
			##	Add all the sizes of the component.
			must.link.components$csize[component.representatives]	<-	unlist(lapply(components.to.collapse, function(vector) sum(must.link.components$csize[vector])), use.names=FALSE)
	
			
			# print("Set the adjacency for all of the representatives.")
			##	Set the adjacency for all of the representatives
			component.outgoing.edges		<-	lapply(components.to.collapse, function(vector) which(Reduce("|", lapply(vector, function(comp) cannot.link.map[,comp]))))
			non.null.edges					<-	which(sapply(component.outgoing.edges, function(x) length(x) > 0, USE.NAMES=FALSE))
			if(length(non.null.edges) > 0){
				non.null.reps					<-	component.representatives[non.null.edges]
				non.null.outgoing.edges			<-	component.outgoing.edges[non.null.edges]			
				# print("component.outgoing.edges")
				indices_1	<-	Reduce(rbind, lapply(1:length(non.null.reps), function(i) cbind(non.null.outgoing.edges[[i]], non.null.reps[i])))
	
				# print("indice_1")
				indices_2	<-	Reduce(rbind, lapply(1:length(non.null.reps), function(i) cbind(non.null.reps[i], non.null.outgoing.edges[[i]])))	
				# print("indices_2")
				
				cannot.link.map[unique(rbind(indices_1, indices_2))]		<-	TRUE
			}
			
			# print("cannot.link.map")
			deleted.components		<-	sort(setdiff(unlist(components.to.collapse, use.names=FALSE), component.representatives))
	
			# print("deleted.components")
			# print("Delete the non-representatives from cannot.link.map.")
			##	Delete the non-representatives from cannot.link.map
			cannot.link.map <- cannot.link.map[-deleted.components, -deleted.components]
			
			# print("cannot.link.map")
			##	Now we need to shift down all the components.
		
			##	Shift down all the assignments in $membership. 
			must.link.components$membership			<-	sapply(must.link.components$membership, function(comp) comp - sum(deleted.components < comp), USE.NAMES=FALSE)	
			# print("must.link.components$membership")
			
			names(must.link.components$membership)	<-	name.vector
			
			# print("names")
			##	Delete all the zero-size components from $csize.
			must.link.components$csize	<-	must.link.components$csize[-deleted.components]
			# print("csize")
			
			##	Delete all the zero-size components from component.assignments
			component.assignments		<-	component.assignments[-deleted.components]
			# print("component.assignments")
			##	update $no
			must.link.components$no			<-	length(must.link.components$csize)
		}
	}

	d1.w1		<-	apply(new.constraint.matrix[c("doc_1", "word_1")], 1, paste, collapse='.')
	z1			<-	as.list(new.constraint.matrix$z_1)
	
	d2.w2		<-	apply(new.constraint.matrix[c("doc_2", "word_2")], 1, paste, collapse='.')
	z2			<-	as.list(new.constraint.matrix$z_2)
	
	known.assignments[d1.w1]		<-	z1
	known.assignments[d2.w2]		<-	z2
	
	# valid.keys		<-	unlist(intersect(keys(known.assignments), name.vector), use.names=FALSE)
	# invalid.keys		<-	unlist(setdiff(keys(known.assignments), valid.keys), use.names=FALSE)
	
	# for(x in invalid.keys){
		# del(x, known.assignments)
	# }

	keys.components	<-	must.link.components$membership[names(known.assignments)]
	component.assignments[keys.components]	<-	unlist(as.list(known.assignments[names(known.assignments)]), use.names=FALSE)
	
	out								<-	list()
	out$component.assignments		<-	component.assignments
	out$must.link.components		<-	must.link.components
	out$cannot.link.map				<-	as(cannot.link.map, "ngCMatrix")
	out$known.assignments			<-	known.assignments
	return(out)
}