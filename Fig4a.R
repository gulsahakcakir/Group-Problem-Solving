## Note: When MASS package is also loaded select() functions clash ##
## Need to specify dplyr::select()

library(tidyverse)
library(plotly)
library(dplyr)
library(igraph)
library(R.matlab)
library(patchwork)

#Parameters
grid_size <-100
n_of_agents <-16
n_of_types <-10
r <- 3 # euclidean, manhattan, radius?
p <- c(0.1, 0.9) # probability of copying solutions
n_of_reps <- 50
network_type = c("ring", "full", LETTERS[1:8])
MW<- unlist(as.vector(readMat("~/Library/CloudStorage/Box-Box/gulsah/Teams (lamberson@ucla.edu)/Simple Model/L8.mat")$L))

# Euclidean distance
d <-function(x,y){sqrt(sum((x-y)^2))}

# Make a grid
S <-tibble(expand.grid(x=c(1:grid_size),y=c(1:grid_size)))

# Read in payoffs from Matlab
S<- S %>% add_column(z = as.vector(readMat("~/Library/CloudStorage/Box-Box/gulsah/Teams (lamberson@ucla.edu)/Simple Model/Surf.mat")$V))

# Label each point with a "type"
# For now types are randomly distributed. Later consider what happens when types are correlated in space
# i.e. certain areas of the space are better known to certain types
S <- S %>% add_column(type=sample(1:n_of_types,size=dim(S)[1],replace=TRUE))

# Visualize 
plot_ly(S,x=~x,y=~y,z=~z,type="mesh3d")


# Create network
init_network <- function(n_of_agents, network_type) {
  if (network_type == "full") {
    return(make_full_graph(n_of_agents))
  } else if (network_type == "ring") {
    return(make_ring(n_of_agents))
  } else {
      return(graph_from_adjacency_matrix(MW[[match(network_type, LETTERS[1:8])]], mode = "undirected"))
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


results <- results %>% mutate(category = ifelse(network %in% c("A","B","C","D","full"), "efficient", "inefficient"))

####### Boxplot of all data ########
fig4_1 <- results %>% filter(p_level == 0.1) %>%
  ggplot(aes(x = network, y = avg_payoff, color=category)) +
  geom_boxplot(position = "dodge") +
  scale_color_manual(values = c("efficient" = "orange", "inefficient" = "light green")) +
  labs(title = "Sharing solutions (p=0.1)",
       x = "Network type",
       y = "Average payoff",
       color = "Efficiency Category")+
  theme(legend.position = "none")

fig4_9 <- results %>% filter(p_level == 0.9) %>%
  ggplot(aes(x = network, y = avg_payoff, color=category)) +
  geom_boxplot(position = "dodge") +
  scale_color_manual(values = c("efficient" = "orange", "inefficient" = "light green")) +
  labs(title = "Sharing perspectives (p=0.9)",
       x = "Network type",
       y = "Average payoff",
       color = "Efficiency Category")+
    theme(legend.position = "none")

fig4_1+fig4_9

######### Figure 4a ############
category_order <- c("efficient", "inefficient")
network_order <- c("full", "A", "B", "C", "D", "E", "F", "G", "H", "ring")

summary_df <- results %>% group_by(network, p_level) %>% 
  summarise(avg_avg = mean(avg_payoff),
            se_avg = sd(avg_payoff)/sqrt(n_of_reps),
            avg_max = mean(max_payoff),
            se_max = sd(max_payoff)/sqrt(n_of_reps)) %>%  
  mutate(category = ifelse(network %in% c("full","A","B","C","D"), "efficient", "inefficient"),
         network = factor(network, levels = network_order),
         category = factor(category, levels = category_order)) 

## Fig4a_avg: average payoff by each network
fig4est_1 <- summary_df %>% filter(p_level == 0.1) %>%
  arrange(category) %>%
  ggplot(aes(x = network, y = avg_avg, color=category)) +
  geom_point()+
  geom_errorbar(aes(ymin = avg_avg - 2 * se_avg, ymax = avg_avg + 2 * se_avg),
                position = position_dodge(width = 0.3),
                width = 0.2)+
  ylim(35,70)+
  scale_color_manual(values = c("efficient" = "orange", "inefficient" = "light green")) +
  labs(title = "p=0.1",
     x = "Network type",
     y = "Average payoff") +
  theme(legend.position = "none")

fig4est_9 <- summary_df %>% filter(p_level == 0.9) %>%
  arrange(category) %>%
  ggplot(aes(x = network, y = avg_avg, color=category)) +
  geom_point()+
  geom_errorbar(aes(ymin = avg_avg - 2 * se_avg, ymax = avg_avg + 2 * se_avg),
                position = position_dodge(width = 0.3),
                width = 0.2)+
  ylim(35,70)+
  scale_color_manual(values = c("efficient" = "orange", "inefficient" = "light green")) +
  labs(title = "p=0.9",
       x = "Network type",
       y = "Average payoff",
       color = "Efficiency Category")
    
fig4est_1 + fig4est_9

## Fig4a_max: maximum payoff by each network

fig4max_1 <- summary_df %>% filter(p_level == 0.1) %>%
  arrange(category) %>%
  ggplot(aes(x = network, y = avg_max, color=category)) +
  geom_point()+
  geom_errorbar(aes(ymin = avg_max - 2 * se_max, ymax = avg_max + 2 * se_max),
                position = position_dodge(width = 0.3),
                width = 0.2)+
  ylim(42,75)+
  scale_color_manual(values = c("efficient" = "orange", "inefficient" = "light green")) +
  labs(title = "p=0.1",
       x = "Network type",
       y = "Max payoff") +
  theme(legend.position = "none")

fig4max_9 <- summary_df %>% filter(p_level == 0.9) %>%
  arrange(category) %>%
  ggplot(aes(x = network, y = avg_max, color=category)) +
  geom_point()+
  geom_errorbar(aes(ymin = avg_max - 2 * se_max, ymax = avg_max + 2 * se_max),
                position = position_dodge(width = 0.3),
                width = 0.2)+
  ylim(42,75)+
  scale_color_manual(values = c("efficient" = "orange", "inefficient" = "light green")) +
  labs(title = "p=0.9",
       x = "Network type",
       y = "Max payoff",
       color = "Efficiency Category")

fig4max_1 + fig4max_9  

