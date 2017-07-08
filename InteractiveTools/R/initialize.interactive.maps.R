## Input:
##		assignments: assignment list in format for interactive.lda.collapsed.gibbs.sampler
## Output:
##		interactive.maps:
##			$must.link.components
##			$cannot.link.map
initialize.interactive.maps		<-	function(assignments){
	## Grab name vector from assignments.
	name.vector	<-	unlist(sapply(1:length(assignments), function(d) sapply(1:length(assignments[[d]]), function(w) paste0(c(as.character(d),".",as.character(w)), collapse=''))), use.names=FALSE)
	
	
	## Initialize must.link.components so that every instance is in its own component.
	must.link.components 					<-	list()
	num.components		 					<-	length(name.vector)
	must.link.components$membership			<-	c(1:num.components)
	names(must.link.components$membership)	<-	name.vector
	must.link.components$csize				<-	rep(as.integer(1), times=num.components)
	must.link.components$no					<-	num.components
	
	## Initialize cannot.link.map as an sparse Matrix
	cannot.link.map			<-	sparseMatrix(1,1,dims=c(must.link.components$no, must.link.components$no))
	cannot.link.map[1,1]		<-	FALSE
	
	## Initialize component.assignments so that it agrees with assignments.
	component.assignments			<-	unlist(assignments, use.names=FALSE)
	
	interactive.maps							<-	list()
	interactive.maps$must.link.components	<-	must.link.components
	interactive.maps$cannot.link.map			<-	cannot.link.map
	interactive.maps$component.assignments	<-	component.assignments
	interactive.maps$known.assignments		<-	list()
	
	return(interactive.maps)
}