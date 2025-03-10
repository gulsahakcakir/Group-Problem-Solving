
# Function to add n random edges without creating duplicates
add_random_edges <- function(g, n){
  for (i in 1:n) {
    #Get all pairs of nodes that do not have an edge between them
    possible_edges <- which(!as.matrix(get.adjacency(g)), arr.ind = TRUE)
    
    #Filter to ensure we only consider edges where the first node is less than the second
    valid_pairs <- possible_edges[possible_edges[,1] < possible_edges[,2], , drop = FALSE]
    
    # If there are no more valid pairs, break the loop
    if (nrow(valid_pairs) == 0) {
      #if (is.null(valid_pairs)){
      cat("No more possible edges to add without creating duplicates.\n")
      break
    }
    
    #Randomly select a valid pair
    random_pair <- valid_pairs[sample(1:nrow(valid_pairs), 1), ]
    
    #Add the selected edge to the graph
    g <- add_edges(g, c(random_pair[1], random_pair[2]))
  }
  return(g)
}  


# Create network 
init_network <- function(n_of_agents, network_type) {
  # if network_type is numeric then start with a linear network and add network_type random edges
  # network_type is the number of additional edges
  # network_type should be <= n(n-1)/2-n-1 = (n-2)(n-1)/2, e.g. for n = 16: network_type < 120 (full) - 15 (linear)
  if(is.numeric(network_type)){
    g<-make_ring(n_of_agents,circular=FALSE)
    return(add_random_edges(g,network_type))
  }
  if (network_type == "full") {
    return(make_full_graph(n_of_agents))
  } else if (network_type == "ring") {
    return(make_ring(n_of_agents,circular=FALSE))
  } else {
    return(graph_from_adjacency_matrix(MW[[match(network_type, LETTERS[1:8])]], mode = "undirected"))
  }
}
