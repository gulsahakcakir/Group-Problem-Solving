## Note: When MASS package is also loaded select() functions clash ##
## Need to specify dplyr::select()

library(tidyverse)
library(plotly)
library(dplyr)
library(igraph)

setwd("~/Library/CloudStorage/Box-Box/gulsah/Teams (lamberson@ucla.edu)/Simple Model")

#Parameters
grid_size <-100
n_of_agents <-16
n_of_types <-10
r <- 3 # euclidean, manhattan, radius?
p <- seq(0,1, by=0.05) # probability of copying solutions
n_of_reps <- 20
network_type = c("ring", "full")

# Euclidean distance
d <-function(x,y){sqrt(sum((x-y)^2))}


# # Make a grid
S <-tibble(expand.grid(x=c(1:grid_size),y=c(1:grid_size)))

##'[Replaced with Matlab output]##
# # Create the payoffs (z)
# S <-S %>% rowwise() %>% mutate(z=(100-d(c(x,y),c(50,50)))^8) #Distance from (50,50) exponentiated
# max_z <- max(S$z)
# S <-S %>% mutate(z=(z/max_z)) %>% rowwise() %>% mutate(z=z+rnorm(1,sd=.05)) #Normalize and add noise             
##'[Replaced with Matlab output]##

# Read in payoffs from mMtlab
library(R.matlab)
S<- S %>% add_column(z = as.vector(readMat('Surf.mat')$V))

plot_ly(S,x=~x,y=~y,z=~z,type="mesh3d")


# Label each point with a "type"
# For now types are randomly distributed. Later consider what happens when types are correlated in space
# i.e. certain areas of the space are better known to certain types

S <- S %>% add_column(type=sample(1:n_of_types,size=dim(S)[1],replace=TRUE))


# Create network
# For now fully_connected: "full", ring: "ring"
init_network <- function(n_of_agents, network_type) {
  if (network_type == "full") {
    return(make_full_graph(n_of_agents))
  } else if (network_type == "ring") {
    return(make_ring(n_of_agents))
  }
}


results<- data.frame(network = character(),
                      p_level = numeric(),
                      rep_no = numeric(),
                      max_perf = numeric(),
                      avg_perf = numeric(),
                      tick_count = numeric(),
                      stringsAsFactors = T)

# Simulation
start_time <- Sys.time()
for (net in network_type){
  print(paste("network:", net))
  for (p_ in p){
    print(paste("p:", p_))
    for (reps in 1:n_of_reps){
      print(paste("rep:", reps))
      
      ##'[ Do I need pairwise comparison of network types with the same agent initializations?]## 
      network <- init_network(n_of_agents, net)
      
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
              
              ##'[ Do we wanna wrap the grid ? ]## 
              # expand.grid(
              #   x = seq(agent$x - 1, agent$x + 1) %% grid_size + 1,
              #   y = seq(agent$y - 1, agent$y + 1) %% grid_size + 1
              # )
              
              # Cells of the same type within distance r (euclidean for now)
              S %>% 
                filter(type == agent$type & d(c(x,y), c(agent$x,agent$y)) < r) %>% 
                dplyr::select(x, y) ))
          # Sharing solutions (<= p)
          if (agent_p <= p_) {
            # visible_cells <- visible_cells + neighbors' current location
            visible_cells <-distinct(rbind(visible_cells, agents[neighbors(network, i),] %>% 
                                             select(x,y)))
          } else { # Sharing perspectives (> p)
            # visible_cells <- visible_cells + cells matching neighbors' type 
            neighbor_type_matches <- S %>% 
              filter(type %in% agents[neighbors(network, i),]$type & d(c(x,y), c(agent$x,agent$y)) < r) %>% 
              select(x,y)
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
            agents[i, c("x", "y")] <- candidates %>% sample_n(1) %>% select(x, y)
          }
        }
        
        # Check for equilibrium
        if (all(!agents_have_options)) {
          stop_condition <- TRUE
        }
        tick <- tick + 1
        
      }
      # Report average payoff and convergence time
      results <- rbind(results, 
                       data.frame(network=net,
                                  p_level = p_,
                                  rep_no = reps,
                                  agents %>%
                                    left_join(S, by = c("x", "y")) %>%
                                    select(x, y, z) %>% 
                                    summarize(avg_payoff= mean(z),
                                              max_payoff= max(z)),
                                  tick_count = tick ))
    }
  }
}
end_time <- Sys.time()
cat("Run time:", end_time - start_time, "\n")

##'[ Should update be synchronous or asynchronous? ]##

# results %>% group_by(network, p_level) %>% summarize(avg_avg = mean(avg_payoff),
#                                                      avg_max = mean(max_payoff))%>%
#   ggplot()+
#   geom_line(aes(x = p_level, y = avg_avg, colour=network, group = network))

results %>% 
  ggplot(aes(x = p_level, colour = network, group = network)) +
  geom_smooth(aes(y = avg_payoff, linetype = "Average Payoff"), se = FALSE) +
  geom_smooth(aes(y = max_payoff, linetype = "Max Payoff"), se = FALSE) +
  scale_color_manual(values = c("blue", "red")) +
  scale_linetype_manual(name = "Line Type", values = c("solid", "dotted")) +
  guides(
    color = guide_legend(title = "Network Type"),
    linetype = guide_legend(title = "Line Type", override.aes = list(colour = c("black", "black")))
  )



