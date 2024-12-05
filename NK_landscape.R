
library(tidyverse)
#library(plotly)
library(dplyr)
library(ggplot2)
library(igraph)
library(R.matlab)
library(purrr)
#library(patchwork)
library(akima)
library(parallel)
library(gtools)

# Set the number of cores to the value of NSLOTS provided by the job scheduler
num_cores <- as.integer(Sys.getenv("NSLOTS"))

# If NSLOTS is not set, default to 1 core (when testing locally)
if (is.na(num_cores) || num_cores < 1) num_cores <- 1

init_network <- function(n_of_agents, network_type) {
  if (network_type == "full") {
    return(make_full_graph(n_of_agents))
  } else if (network_type == "ring") {
    return(make_ring(n_of_agents,circular=FALSE))
  } else {
    return(graph_from_adjacency_matrix(MW[[match(network_type, LETTERS[1:8])]], mode = "undirected"))
  }
}
############################### NK landscapes ##################################
# Function to create NK landscape as implemented by Barkoczi and Galesic
NK<-function(N,K,LS,fitness,depends){
  
  if(K==0){ # simple landscape, no epistasis
    fitness.score <- vector()
    for (ag in 1:nrow(LS)){
      rows<-as.numeric(LS[ag,])+1
      values<-sapply(1:N, function(y) fitness[rows[y],y] )
      fitness.score[ag]<-mean(values)
    }
    
  } else { # K in 1:N-1, with epistasis (dependencies)
    indx1<-do.call(paste0,as.data.frame(fitness[,c(1:(K+1))]))
    indx2<-sapply(1:N, function(y) do.call(`paste0`,as.data.frame(LS[,depends[,y]])))
    fitness.score <- sapply(1:nrow(LS), function(o) mean(diag(sapply(indx2[o,], function(x) fitness[which(indx1 %in% x),(K+2):ncol(fitness)]))))
  }
  
  landscape<-cbind(LS[,1:N],fitness.score)
  colnames(landscape)[(N + 1)] <- "fitness"
  return(as.data.frame(landscape))
  
}

# Generate landscape

# How to make sure two landscapes we are using are comparable wrt ruggedness level?
# LF uses N=20 & K = 5 -- MW landscape is mapped on a 100x100 grid; 2^13 < 100^2 < 2^14
# Set parameters
N <- 15
K <- 7

genNK <- function(N, K){

  LS<-permutations(2,N,v=c(0,1),repeats.allowed=TRUE) # all possible binary strings
  LS<-as.data.frame(LS)
  
  # Define dependencies
  if (K==0){
    depends <- as.vector(1:N)
    values <- replicate(N,round(runif(2,0,1),1)) # round to one decimal point -- is this appropriate?
    fitness <- values
  } else {
    # might wanna fix the dependencies w set.seed()
    #depends <- rbind(1:N,replicate(N,sample(c(1:N),K,replace=F))) # might pick self again, leading to K-1 dependencies
    depends <- as.data.frame(rbind(1:N, sapply(1:N, function(y) {
      sample(setdiff(1:N, y), K, replace = FALSE)
    })))
    combinations <- permutations(2,K+1,v=c(0,1),repeats.allowed=TRUE) # 2^(K+1) permutations
    values <- replicate(N,round(runif(nrow(combinations),0,1),1))
    fitness <- cbind(combinations,values)
  }
  landscape <- NK(N,K,LS,fitness,depends)
  landscape <- landscape %>%
    mutate( Index = 0:(nrow(landscape)-1), 
            fitness = (fitness/max(fitness))^8) # exponentiated to 8? why?
  ### From the paper: Most solutions tend to cluster around very similar payoff values. 
  # Following Lazer and Friedman, we use a monotonic transformation, 
  # raising PNorm to the power of 8 ((PNorm)8) to widen the distribution, 
  # making most solutions ‘mediocre’ and only a few solutions ‘very good’.
  return(landscape)
}

# Function to count the number of set bits (Hamming distance)
vector_bitwBitCount <- function(x, N) {
  # Count the number of set bits up to the Nth bit
  sapply(x, function(val) sum(as.integer(intToBits(val)[1:N])))
}

# Optimized function to get visible cells for an agent
baseline_visible <- function(agent, Surf, hamming1_neighbors, N, r) {
  # Extract agent's Index and type
  agent_index <- agent$Index
  agent_type <- agent$type
  
  # Immediate neighbors (Hamming distance = 1)
  # Since Index starts at 0 and row_number starts at 1, add 1 to get the row
  neighbor_rows1 <- hamming1_neighbors[[agent_index + 1]]
  neighbors1 <- Surf[neighbor_rows1, ]
  # Add hamming_distance column with value 1 to match columns with type_matching_cells
  neighbors1 <- neighbors1 %>%
    mutate(hamming_distance = 1)
  
  # Type-matching solutions with Hamming distance < r
  # Filter Surf for the agent's type
  type_filtered_Surf <- Surf %>%
    filter(type == agent_type)
  
  # Compute Hamming distances only for the filtered rows
  hamming_distances <- vector_hamming_distance(agent_index, type_filtered_Surf$Index, N)
  
  # Add the Hamming distance to the filtered data
  type_matching <- type_filtered_Surf %>%
    mutate(hamming_distance = hamming_distances) %>%
    filter(hamming_distance < r)

  # Combine both sets and ensure uniqueness
  visible_solutions <- unique(rbind(neighbors1, type_matching)) %>% select(Index)
  
  return(visible_solutions)
}

# Vectorized Hamming distance calculation
vector_hamming_distance <- function(agent_index, all_indices, N) {
  # Perform bitwise XOR between agent_index and all_indices
  xor_results <- bitwXor(agent_index, all_indices)
  
  # Count the number of set bits up to N bits
  hamming_distances <- vector_bitwBitCount(xor_results, N)
  
  return(hamming_distances)
}


# Function to precompute Hamming distance = 1 neighbors using Index
precompute_hamming1_neighbors <- function(Surf, N) {
  total_genotypes <- nrow(Surf)
  
  # Initialize a list to store neighbors for each genotype
  hamming1_neighbors <- vector("list", total_genotypes)
  
  for (i in 1:total_genotypes) {
    current_index <- Surf$Index[i]
    
    # Generate all possible single-bit flips
    neighbors_indices <- bitwXor(current_index, 2^(0:(N-1)))
    
    # Ensure neighbors are within the valid range
    neighbors_indices <- neighbors_indices[neighbors_indices >= 0 & neighbors_indices < total_genotypes]
    
    # Map neighbor indices to row numbers (since Index = row_number() - 1)
    #neighbor_rows <- neighbors_indices + 1
    
    #hamming1_neighbors[[i]] <- neighbor_rows
    hamming1_neighbors[[i]] <-neighbors_indices
  }
  
  return(hamming1_neighbors)
}


##############################Simulation########################################
simulateNK <- function(network_type, p_,  n_of_reps, n_of_agents, n_of_types, Surf, distance_matrix, r, t, randomSurface=FALSE, timeLimit=FALSE) {
  results <- data.frame()  # Initialize an empty dataframe to store results
  
  # Main simulation loop
  for (reps in 1:n_of_reps) {
    if(randomSurface){Surf<-genNK(N,K)
    #plot_ly(Surf,x=~x,y=~y,z=~z,type="mesh3d")
    }
    
    print(reps)
    # Generate and set a random seed
    r_seed <- sample.int(.Machine$integer.max, 1)
    set.seed(r_seed)
    
    # Initialize the network 
    network <- init_network(n_of_agents, network_type)
    
    # Randomly assign agents to points on the landscape and to a type
    agents <- data.frame(
      type = sample(1:n_of_types, size = n_of_agents, replace = TRUE),
      Index = sample(Surf$Index, size = n_of_agents, replace = TRUE)
    )
    
    # Assign types to points on grid
    S <- Surf %>%
      mutate(type = sample(1:n_of_types, size = n(), replace = TRUE))
    
    stop_condition <- FALSE
    tick <- 0
    ts <- 0
    
    while (!stop_condition) {
      # Every tick reset stopping condition and update order
      agents_have_options <- rep(TRUE, n_of_agents)
      update_order <- sample(1:n_of_agents)
      
      for (i in update_order) {
        agent <- agents[i,]
        agent_index <- agent$Index
        #current_payoff <- S %>% semi_join(agent, by = c("x", "y")) %>% pull(z)
        current_payoff <- as.numeric(S[agent_index == S$Index, "fitness"])
        agent_p <- runif(1)
        
        visible_solutions <- baseline_visible(agent, S, hamming1_neighbors, N, r)
        
        
        if (agent_p <= p_) {
          visible_solutions <- unique(rbind(visible_solutions, agents[neighbors(network, i),] %>% select(Index)))
        } else {
          # With fully connected networks this takes too long
          neighbor_type_matches <- S %>%
            filter(type %in% agents[neighbors(network, i),]$type) %>%
            mutate(distance = vector_hamming_distance(agent_index, Index, N)) %>%
            filter(distance < r) %>%
            select(Index)
          
          visible_solutions <- unique(rbind(visible_solutions, neighbor_type_matches))
        }
        
        visible_payoffs <- S %>% 
          semi_join(visible_solutions, by = "Index") %>% 
          filter(fitness > current_payoff)
        
        if (nrow(visible_payoffs) == 0) {
          agents_have_options[i] <- FALSE
        } else {
          candidates <- visible_payoffs[which(visible_payoffs$fitness == max(visible_payoffs$fitness)),]
          agents[i, c("Index")] <- candidates %>% sample_n(1) %>% select(Index)
        }
      }
      
      if(!timeLimit){
        if (all(!agents_have_options)| tick > 10) {
          ts <- ts + 1
          if (ts == t) {
            stop_condition <- TRUE
          }
        } else {
          ts <- 0
        }
      }else{
        if(tick>t){stop_condition<-TRUE}
      }
      
      tick <- tick + 1
    }
    # Update results df
    results <- rbind(results, 
                     data.frame(network = network_type,
                                p_level = p_,
                                r = r,
                                n_of_types = n_of_types,
                                rep_no = reps,
                                agent = 1:n_of_agents,
                                payoff = agents %>%
                                  left_join(S, by = "Index") %>%
                                  select(fitness),
                                tick_count = tick,
                                rand_seed = r_seed))
  }
  
  return(results)
}

#################################### Fig 3 #####################################
# Parameters
n_of_agents <- 16
n_of_types <- 10
r <- 3 # euclidean, manhattan, radius?
p <- seq(0,1, by=0.1) # probability of copying solutions
n_of_reps <- 5
t <- 1 # time stuck for stop condition or time limit for all sims if timeLimit=TRUE
network_type = c("ring", "full")
problem_type <-  "complex"

# Initialize surface
#Surf <- init_surface("complex", Grid)
Surf<-genNK(N,K)

# Find distance 1 solutions for each genotype
hamming1_neighbors <- precompute_hamming1_neighbors(Surf, N)

# Add the precomputed neighbors to Surf
Surf$neighbors1 <- hamming1_neighbors # a list

# Use mapping (purrr) to run the simulation function with different parameter combinations
cond <- expand_grid(network_type, p) # all possible network-p combinations
start_time <- Sys.time()
results <- map2_df(cond$network_type, cond$p, 
                   ~simulateNK(.x, .y, n_of_reps, n_of_agents, n_of_types, Surf, dist_matrix, r, t,randomSurface=TRUE,timeLimit=FALSE))
end_time <- Sys.time()

cat("Run time:", end_time - start_time, "\n") 


results_summary <- results %>% group_by(network, p_level, rep_no) %>% 
  summarise(avg_payoff = mean(fitness),
            max_payoff = max(fitness),
            found_peak = (max(fitness)==1)) %>% 
  group_by(network, p_level) %>%
  summarise(avg_avg_payoff = mean(avg_payoff),
            avg_max_payoff = mean(max_payoff),
            max_max_payoff = max(max_payoff),
            percent_found_peak = mean(found_peak))

# Plot Fig 3
results_summary %>% 
  ggplot(aes(x = (1-p_level), colour = network, group = network)) +
  geom_smooth(aes(y = avg_avg_payoff, linetype = "Average Payoff"),se = F) +
  geom_smooth(aes(y = avg_max_payoff, linetype = "Max Payoff"), se = F) +
  scale_color_manual(values = c( "red", "blue"), labels = c("Fully connected", "Ring")) +
  scale_linetype_manual(name = "Line Type", values = c("solid", "dotted")) +
  guides(
    color = guide_legend(title = "Network Type"),
    linetype = guide_legend(title = "Line Type", override.aes = list(colour = c("black", "black")))
  )+ 
  labs(title = "NK, r = 3", 
       x = "Probability of Sharing Perspectives", 
       y= "Solution Payoff")+
  theme(
    panel.background = element_rect(fill = "white", color = "black"),
  )

#Fig. 3 not smoothed
results_summary %>% 
  ggplot(aes(x = (1-p_level), colour = network, group = network)) +
  geom_line(aes(y = avg_avg_payoff, linetype = "Average Payoff")) +
  geom_line(aes(y = avg_max_payoff, linetype = "Max Payoff")) +
  scale_color_manual(values = c( "red", "blue"), labels = c("Fully connected", "Ring")) +
  scale_linetype_manual(name = "Line Type", values = c("solid", "dotted")) +
  guides(
    color = guide_legend(title = "Network Type"),
    linetype = guide_legend(title = "Line Type", override.aes = list(colour = c("black", "black")))
  )+ 
  labs(title = "NK, r = 3", 
       x = "Probability of Sharing Perspectives", 
       y= "Solution Payoff")+
  theme(
    panel.background = element_rect(fill = "white", color = "black"),
  )
