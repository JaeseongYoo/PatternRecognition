make_expandedRhombus = function (parent_nodes, child_nodes, Probs = NULL, nodename = NULL, cardinality = NULL) 
{
	nodes = parent_nodes + child_nodes;

	if (parent_nodes < 1) {
		stop("Need More Parent Nodes!");
	} else if (child_nodes < 1) {
		stop("Need More Child Nodes!");
	}


	arcs_mat = matrix(0, nodes, nodes)
	for (i in 1:parent_nodes)
	{
		for (j in (1+parent_nodes):nodes)
		{
			arcs_mat[i,j] = 1
		}
	}
	
	
	# Check Input Probs & Cardinalities
	checker = check_cardinality(arcs_mat = arcs_mat, nodename = nodename, cardinality = cardinality);
	cardinality = checker$cardinality;
	num_of_probs = checker$num_of_probs;
	nodename = checker$nodename;
	

	#####	
	if (is.null(Probs) & is.null(cardinality))
	{
		Probs = list()
		for (i in 1:parent_nodes)
		{
			Probs[[i]] = runif(1);
		}

		for (i in (1+parent_nodes):nodes)
		{
			Probs[[i]] = runif(2^2)
		}
	} else if (is.null(Probs)) {
		Probs = list()
		for (i in 1:length(num_of_probs))
		{
			Probs[[i]] = runif(num_of_probs[i])
		}
	}
	#####
	
	
	result = list(arcs_mat = arcs_mat, Probs = Probs, nodename = nodename, cardinality = cardinality, num_of_nodes = nodes);

	return(result)
}
