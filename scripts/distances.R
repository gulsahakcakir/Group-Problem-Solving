# Function to compute pairwise distances between grid cells and store in a matrix
comp_dist <- function(grid_coords){
  n <- nrow(grid_coords) # number of grid points
  
  # Initialize distance matrix
  distance_matrix <- matrix(NA, nrow = n, ncol = n, 
                            dimnames = list(paste("(", grid_coords$x, ",", grid_coords$y, ")"), 
                                            paste("(", grid_coords$x, ",", grid_coords$y, ")")))
  
  # Compute pairwise distances (efficiently)
  for (i in 1:(n-1)) {
    distance_matrix[i, (i+1):n] <- distance_matrix[(i+1):n, i] <- # since symmetrical
      sqrt((grid_coords$x[i] - grid_coords$x[(i+1):n])^2 + 
             (grid_coords$y[i] - grid_coords$y[(i+1):n])^2)
  }
  
  # Fill diagonal with zeros
  diag(distance_matrix) <- 0
  
  return(distance_matrix)
}