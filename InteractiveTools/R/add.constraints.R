require(hash)
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
##			$cannot.link.map:
##			$known.assignments:
add.constraints		<-	function(component.assignments, new.constraint.matrix, K, interactive.maps, edge.control=TRUE){
	if("z_1" %in% names(new.constraint.matrix)){
		#print("Adding constraints with z's")
		return(add.constraints.with.zs(component.assignments, new.constraint.matrix, K, interactive.maps))
	}
	else if(edge.control){
		#print("Adding constraints without z's: edge control")
		return(add.constraints.without.zs.edge.control(component.assignments, new.constraint.matrix, K, interactive.maps))
	}
	else{
		return(NULL)
		# return(add.constraints.without.zs(component.assignments, new.constraint.matrix, K, interactive.maps))
	}
}
