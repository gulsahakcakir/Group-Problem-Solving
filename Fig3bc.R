## Note: When MASS package is also loaded select() functions clash ##
## Need to specify dplyr::select()

library(tidyverse)
library(plotly)
library(dplyr)
library(igraph)
library(R.matlab)

#Parameters
grid_size <-100
n_of_agents <-16
n_of_types <-10
r <- 3 # euclidean, manhattan, radius?
p <- seq(0,1, by = 0.05) # probability of copying solutions
n_of_reps <- 20
network_type = c("ring", "full")
problem_type <- c("complex")

# Euclidean distance
d <-function(x,y){sqrt(sum((x-y)^2))}

# Create landscape: simple and complex
init_surface <- function(problem_type){
  # Make a grid
  S <-tibble(expand.grid(x=c(1:grid_size),y=c(1:grid_size)))
  
  # Create the payoffs (z)
  if (problem_type == "simple") {
    S <-S %>% rowwise() %>% mutate(z=(100-d(c(x,y),c(50,50)))^15) #Distance from (50,50) exponentiated
    max_z <- max(S$z)
    S <-S %>% mutate(z=(z/max_z) * 100) # Normalize
    # %>% rowwise() %>% mutate(z=z+rnorm(1,sd=.05)) #Normalize and add noise 
  } else {
    # Read in payoffs from Matlab
    S<- S %>% add_column(z = as.vector(readMat('~/Library/CloudStorage/Box-Box/gulsah/Teams (lamberson@ucla.edu)/Simple Model/Surf.mat')$V))
  }
  # Label each point with a "type"
  # For now types are randomly distributed. Later consider what happens when types are correlated in space
  # i.e. certain areas of the space are better known to certain types
  S <- S %>% add_column(type=sample(1:n_of_types,size=dim(S)[1],replace=TRUE))
  return(S)
}


# Create network
# For now fully_connected: "full", ring: "ring"
init_network <- function(n_of_agents, network_type) {
  if (network_type == "full") {
    return(make_full_graph(n_of_agents))
  } else if (network_type == "ring") {
    return(make_ring(n_of_agents))
  }
}


results3<- data.frame(network = character(),
                     p_level = numeric(),
                     rep_no = numeric(),
                     max_perf = numeric(),
                     avg_perf = numeric(),
                     tick_count = numeric(),
                     stringsAsFactors = T)

start_time <- Sys.time()
for (surf in problem_type){
  S <- init_surface(surf)
  for (net in network_type){
    network <- init_network(n_of_agents, net)
    print(paste("network:", net))
    for (p_ in p){
      print(paste("p:", p_))
      for (reps in 1:n_of_reps){
        print(paste("rep:", reps))
        
        # Create a set of agents with random types and initial positions
        agents <- data.frame(
          type = sample(1:n_of_types, size = n_of_agents, replace = TRUE),
          x = sample(1:grid_size, size = n_of_agents, replace = TRUE),
          y = sample(1:grid_size, size = n_of_agents, replace = TRUE)
        )
        
        stop_condition <- FALSE
        tick <- 0
        
        while (!stop_condition) {
          
          print(paste("tick:", tick))
          # Initialize a logical vector to track whether each agent has options
          agents_have_options <- rep(TRUE, n_of_agents)
          
          update_order <- sample(1:n_of_agents)
          for (i in update_order){
            #print(paste("agent:", i))
            # For each agent
            agent <- agents[i,]
            current_payoff <- S %>% semi_join(agent, by = c("x", "y")) %>% pull(z)
            
            # Draw a random number
            agent_p <- runif(1)
            # Gather the list of visible cells
            visible_cells <- distinct(
              rbind(
                # Adjacent cells 
                expand.grid(x = max(1, agent$x - 1):min(grid_size, agent$x + 1),
                            y = max(1, agent$y - 1):min(grid_size, agent$y + 1)),
                
                # Cells of the same type within distance r (euclidean for now)
                S %>% 
                  filter(type == agent$type & d(c(x,y), c(agent$x,agent$y)) < r) %>% 
                  dplyr::select(x, y) ))
            # Sharing solutions (<= p)
            if (agent_p <= p_) {
              # visible_cells <- visible_cells + neighbors' current location
              visible_cells <-distinct(rbind(visible_cells, agents[neighbors(network, i), c("x", "y")])) 
            } else { # Sharing perspectives (> p)
              # visible_cells <- visible_cells + cells matching neighbors' type 
              
              # neighbor_type_matches <- S %>% 
              #   filter(type %in% agents[neighbors(network, i),]$type & d(c(x,y), c(agent$x,agent$y)) < r) %>% 
              #   select(x,y)
              
              neighbor_type_matches <- S %>%
                semi_join(agents[neighbors(network, i), ], by = c("type")) %>%
                filter(d(c(x, y), c(agent$x, agent$y)) < r) %>%
                select(x, y)
              visible_cells <-distinct(rbind(visible_cells, neighbor_type_matches))
            }
            # Identify the cells with the highest payoff 
            
            # create a list of candidate points (payoff must be larger than current location)
            visible_payoffs <- S %>% semi_join(visible_cells, by = c("x", "y")) %>% filter(z > current_payoff)
            
            # Check if candidates list is empty for the current agent
            if (nrow(visible_payoffs) == 0) {
              agents_have_options[i] <- FALSE
            } else {
              # Check which cell in the set has the highest z value
              candidates <- visible_payoffs[which(visible_payoffs$z == max(visible_payoffs$z)),]
              # Randomly pick one from the candidate list and move (update the main df)
              agents[i, c("x", "y")] <- candidates[sample(nrow(candidates), 1), c("x", "y")]
            }
          }
          
          # Check for equilibrium
          if (all(!agents_have_options) | tick > 50) {
            stop_condition <- TRUE
          } 
          tick <- tick + 1
          
        }
        # Update dataframe
        results3 <- rbind(results3, 
                          data.frame(network=net,
                                     p_level = p_,
                                     rep_no = reps,
                                     agent = 1:n_of_agents,
                                     payoff = agents %>%
                                       left_join(S, by = c("x", "y")) %>%
                                       select(z),
                                     tick_count = tick ))
      }
    }
  }
}
end_time <- Sys.time()
cat("Run time:", end_time - start_time, "\n") 

df3 <-results3 %>% group_by(network, p_level, rep_no) %>%
  summarise(count=n_distinct(z)) %>%
  group_by(network, p_level) %>%
  summarise(avg_count = mean(count))

df3c <- results3 %>% group_by(network, p_level) %>%
  summarise(avg_tick=mean(tick_count))

fig3b <- df3 %>%
  ggplot(aes(x = p_level, y = avg_count, group = network)) +
  geom_smooth(aes(color=network), se = F)+
  scale_y_continuous(breaks=seq(1,16,2))+
  labs(title = "Complex landscape", x = "p_level", y = "Number of unique solutions")

fig3c <- df3c %>%
  ggplot(aes(x = p_level, y = avg_tick, group = network)) +
  geom_smooth(aes(color=network), se = F)+
  labs(title = "Complex landscape", x = "p_level", y = "Convergence time")

