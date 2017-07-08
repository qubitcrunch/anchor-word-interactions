## Input:
##		assignments: assignment list in format for interactive.lda.collapsed.gibbs.sampler
##		new.constraint.matrix: list generated in the format according to getConstraints().
##		K:
##		must.link.components: 
##		cannot.link.graph:
##		int.maps:
## Output:
##		out: 
##			$assignments: 
##			$must.link.components
##			$cannot.link.graph:
##			$num.must.link.constraints:
##			$num.cannot.link.constraints:
add.constraints.without.zs.edge.control		<-	function(component.assignments, new.constraint.matrix, K, interactive.maps){
	must.link.components 		<- 	interactive.maps$must.link.components
	name.vector					<-	names(must.link.components$membership)
	cannot.link.map				<-	as(interactive.maps$cannot.link.map, "ngCMatrix")
	
	# cat("(Add) All constraints obeyed at beginning of method: ", obeys.constraints(component.assignments, cannot.link.map), "\n")
	## Grab all the cannot.link.constraints
	# print("Grab all the cannot.link.constraints")
	cannot.link.constraints		<-	new.constraint.matrix[new.constraint.matrix$link == "cannot",]
	cannot.link.nodes			<-	c()
	
	if(length(cannot.link.constraints$link) > 0){
		## Add all the cannot.link.constraints
		a_vector		<-	unname(sapply(1:length(cannot.link.constraints$link), function(i) 
			must.link.components$membership[paste0(c(as.character(cannot.link.constraints$doc_1[i]),".",as.character(cannot.link.constraints$word_1[i])), collapse='')], USE.NAMES=FALSE)
		)
		b_vector		<-	unname(sapply(1:length(cannot.link.constraints$link), function(i) 
			must.link.components$membership[paste0(c(as.character(cannot.link.constraints$doc_2[i]),".",as.character(cannot.link.constraints$word_2[i])), collapse='')], USE.NAMES=FALSE)
		)
	
		indices	<-	unique(rbind(unname(cbind(a_vector, b_vector)), unname(cbind(b_vector, a_vector))))
		#print("indices:")
		#print(indices)
		cannot.link.map[indices]		<-	TRUE
		cannot.link.nodes	<-	union(a_vector, b_vector)
	}


	## Grab all the must.link.constraints
	# print("Grab all the must.link.constraints")
	must.link.constraints			<-	new.constraint.matrix[new.constraint.matrix$link == "must",]
	component.representatives		<-	c()
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
		#print("el:")
		#print(el)
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
			component.representatives	<-	unlist(lapply(components.to.collapse, function(vector) vector[1]),use.names=FALSE)
		
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
	
		
	
			# print("For each element of cannot.link.nodes, find its representative if it is in the component, otherwise leave it how it is.")
			## For each element of cannot.link.nodes, find its representative if it is in a component, otherwise leave it how it is.
			rep.rep.vector			<-	unlist(lapply(1:length(components.to.collapse), function(i) rep(component.representatives[i], length(components.to.collapse[[i]]))), use.names=FALSE)
			components.vector		<-	unlist(components.to.collapse, use.names=FALSE)
			cannot.link.nodes		<-	unique(mapvalues(cannot.link.nodes, components.vector, rep.rep.vector, warn_missing=FALSE))
	
			## Grab the deleted components.			
			deleted.components		<-	sort(setdiff(unlist(components.to.collapse, use.names=FALSE), component.representatives))

			# print("Delete the non-representatives from cannot.link.map.")
			##	Delete the non-representatives from cannot.link.map

			cannot.link.map <- cannot.link.map[-deleted.components, -deleted.components]
			
			##	Delete the non-representatives from $csize.
			must.link.components$csize	<-	must.link.components$csize[-deleted.components]
			
			##	Delete the non-representatives from component.assignments
			component.assignments		<-	component.assignments[-deleted.components]
			
			##	update $no
			must.link.components$no			<-	length(must.link.components$csize)
			
			
			##	Now we need to shift down all the components.
		
			##	Shift down all the assignments in $membership. 
			must.link.components$membership			<-	sapply(must.link.components$membership, function(comp) comp - sum(deleted.components < comp), USE.NAMES=FALSE)	
			names(must.link.components$membership)	<-	name.vector

			
			##	Shift down all elements of component.representatives.
			component.representatives	<-	sapply(component.representatives, function(comp) comp - sum(deleted.components < comp), USE.NAMES=FALSE)
	
			##	Shift down all elements of cannot.link.nodes.
			cannot.link.nodes	<-	sapply(cannot.link.nodes, function(comp) comp - sum(deleted.components < comp), USE.NAMES=FALSE)
		}
	}
	
	# print("Delete edges until edge degrees are small enough.")
	##	Delete edges until edge degrees are small enough
	num.neighbors		<-	colSums(cannot.link.map)
	high.degree.nodes	<-	which(num.neighbors >= K - 1)
	high.degrees			<-	num.neighbors[high.degree.nodes]
	
	##	For each high.degree.node, select edges to delete randomly.
	if(length(high.degrees) > 0){
		# print("Choose edges to delete.")
		edges.to.delete		<-	lapply(1:length(high.degree.nodes), function(i) sample(which(cannot.link.map[, high.degree.nodes[i]]), size=(high.degrees[i] - K + 2)))
		
		##	Delete these outgoing edges from cannot.link.map[high.degree.nodes] to edges.to.delete.
		indices_1	<-	Reduce(rbind, lapply(1:length(high.degree.nodes), function(i) cbind(edges.to.delete[[i]], high.degree.nodes[i])))
		indices_2	<-	Reduce(rbind, lapply(1:length(high.degree.nodes), function(i) cbind(high.degree.nodes[i], edges.to.delete[[i]])))
		cannot.link.map[rbind(indices_1, indices_2)]		<-	FALSE
	}
	
	cannot.link.map	<-	as(cannot.link.map, "ngCMatrix")

	# print("Find satisfying assignments for all components involved in new.constraint.matrix.")
	##	Find satisfying assignments for all components involved in new.constraint.matrix
	##	Union + sort.
	
	nodes	<-	component.representatives
	
	if(length(cannot.link.nodes)){
		nodes	<-	union(component.representatives, cannot.link.nodes)
	}	
	
	
	if(length(nodes) == 1){
		available.topics		<- c(0:(K-1))
		
		outside.neighbors	<- which(cannot.link.map[,nodes])
		if(length(outside.neighbors) > 0){
			available.topics <-	setdiff(c(0:(K-1)), component.assignments[outside.neighbors])
		}
		
		if(length(available.topics) > 0){
			component.assignments[nodes]	 <- sample(available.topics,1)
		}
		
	} else if(length(nodes) > 1){
		nodes	<-	sort(nodes)	
		
		##	Get all the edges between these nodes.
		component.neighbor.vectors	<- as(cannot.link.map[nodes,nodes], "dgCMatrix")

		##	Get topics not taken by outside topics.
		available.topics		<-	lapply(nodes, function(comp){
										outside.neighbors	<- setdiff(which(cannot.link.map[,comp]), nodes)
										if(length(outside.neighbors) > 0){
											setdiff(c(0:(K-1)), component.assignments[outside.neighbors])
										}
										else{
											c(0:(K-1))	
										}
									})

		##	Create a matrix which encodes available.topics
		available.topic.matrix	<-	matrix(0, nrow=length(nodes), ncol=K)
		node.rep.vector			<-	unlist(lapply(1:length(nodes), function(i) rep(i, length(available.topics[[i]]))), use.names=FALSE)
		available.topic.vector	<-	unlist(available.topics, use.names=FALSE) + 1
		available.topic.matrix[cbind(node.rep.vector, available.topic.vector)] <- 1
	
		component.assignments[nodes]		<-	initializeCpp(component.neighbor.vectors, available.topic.matrix)
	}
	
	# cat("(Add) Max number of neighbors: ", max(colSums(cannot.link.map)), "\n")
	# cat("(Add) max - min of component.assignments: ", max(component.assignments), " - ", min(component.assignments), "\n")
	# cat("(Add) Cannot link map is symmetric at end of method: ", isSymmetric(cannot.link.map), "\n")
	# cat("(Add) All constraints obeyed at end of method: ", obeys.constraints(component.assignments, cannot.link.map), "\n")
	
	out									<-	list()
	out$component.assignments			<-	component.assignments
	out$must.link.components				<-	must.link.components
	out$cannot.link.map					<-	cannot.link.map
	return(out)
}

obeys.constraints	<-	function(component.assignments, cannot.link.map){
	A	<-	summary(cannot.link.map)
	
	not.violated		<-	all(sapply(1:length(A[,1]), function(i) component.assignments[A[i,1]] != component.assignments[A[i,2]], USE.NAMES=FALSE))
	
	return(not.violated)
}