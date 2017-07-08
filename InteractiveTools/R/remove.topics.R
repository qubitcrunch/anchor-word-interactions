## Input:
##		interactive.maps: An object of the form output by initialize.interactive.maps
##		K: the present number of topics
##		topics.to.remove: A vector of topics we want to remove from our model
## Output:
##		out:
##			$interactive.maps
##			$K: the number of topics in our model after removing topics in topics.to.remove
remove.topics		<-	function(interactive.maps, K, topics.to.remove){	
	##	Only consider topics that are within range
	topics.to.remove		<-	unique(topics.to.remove[(topics.to.remove >= 0) & (topics.to.remove < K)])
	
	remaining.topics	<-	setdiff(c(0:(K-1)), topics.to.remove)
		
	##	If there are no words to remove, then we exit.
	if(length(topics.to.remove) <= 0){
		out						<-	list()
		out$interactive.maps	<-	interactive.maps
		out$K					<-	K
		return(out)
	}
	if(length(topics.to.remove) >= K){
		print("Too many topics to remove. Exiting without change.")
		out						<-	list()
		out$interactive.maps		<-	interactive.maps
		out$K					<-	K
		return(out)
	}
	
	component.assignments	<-	interactive.maps$component.assignments
	
	##	Get components currently assigned to a topic in topics.to.remove
	components.to.reassign	<-	which(component.assignments %in% topics.to.remove)
	
	##	Reassign the components randomly
	component.assignments[components.to.reassign]	<-	sample(remaining.topics, length(components.to.reassign), replace=TRUE)
	
	##	Shift down all elements of component.assignments
	component.assignments	<-	sapply(component.assignments, function(top) top - sum(topics.to.remove < top), USE.NAMES=FALSE)	

	##	Shift down K
	K	<-	length(remaining.topics)	
	
	##	Pack everything up.
	out	<-	list()
	
	interactive.maps$component.assignments	<-	component.assignments
	
	out$interactive.maps	<-	interactive.maps
	
	out$K	<-	K
	return(out)
}