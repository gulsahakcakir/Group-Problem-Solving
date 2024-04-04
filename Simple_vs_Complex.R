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
p <- c(0.1,0.9) # probability of copying solutions
n_of_reps <- 20
t <- 1 # time stuck for stop condition
network_type = c("ring", "full")
problem_type <- c("simple", "complex")

# Euclidean distance
d <-function(x,y){sqrt(sum((x-y)^2))}

# Create landscape: simple and complex
init_surface <- function(problem_type){
  # Make a grid
  Surf <-tibble(expand.grid(x=c(1:grid_size),y=c(1:grid_size)))
  
  # Create the payoffs (z)
  if (problem_type == "simple") {
    Surf <-Surf %>% rowwise() %>% mutate(z=(100-d(c(x,y),c(50,50)))^15) #Distance from (50,50) exponentiated
    max_z <- max(Surf$z)
    Surf <-Surf %>% mutate(z=(z/max_z) * 100) # Normalize
    # %>% rowwise() %>% mutate(z=z+rnorm(1,sd=.05)) #Normalize and add noise 
  } else {
    # Read in payoffs from Matlab
    Surf <- Surf %>% add_column(z = as.vector(readMat('~/Library/CloudStorage/Box-Box/gulsah/Teams (lamberson@ucla.edu)/Simple Model/Surf.mat')$V))
  }
  # Label each point with a "type"
  # For now types are randomly distributed. Later consider what happens when types are correlated in space
  # i.e. certain areas of the space are better known to certain types
  #S <- S %>% add_column(type=sample(1:n_of_types,size=dim(S)[1],replace=TRUE))
  return(Surf)
}

# Visualize both surfaces
plt1 <-plot_ly(init_surface("simple"),x=~x,y=~y,z=~z,type="mesh3d")
plt2 <-plot_ly(init_surface("complex"),x=~x,y=~y,z=~z,type="mesh3d") 
plt1
plt2

# Create network
# For now fully_connected: "full", ring: "ring"
init_network <- function(n_of_agents, network_type) {
  if (network_type == "full") {
    return(make_full_graph(n_of_agents))
  } else if (network_type == "ring") {
    return(make_ring(n_of_agents))
  }
}


#### Figure 1: performance over time in simple vs. complex environments with p = 0 and p = 1 #### 
results1<- data.frame(surface= character(),
                      network = character(),
                      p_level = numeric(),
                      rep_no = numeric(),
                      agent = numeric(),
                      payoff = numeric(),
                      tick_count = numeric(),
                      stringsAsFactors = T)


start_time <- Sys.time()
for (surf in problem_type){
  print(paste("surface:", surf))
  for (net in network_type){
    print(paste("network:", net))
    for (p_ in p){
      print(paste("p:", p_))
      for (reps in 1:n_of_reps){
        print(paste("rep:", reps))
        
        Surf <- init_surface(surf)
        network <- init_network(n_of_agents, net)
        # Create a set of agents with random types and initial positions
        agents <- data.frame(
          type = sample(1:n_of_types, size = n_of_agents, replace = TRUE),
          x = sample(1:grid_size, size = n_of_agents, replace = TRUE),
          y = sample(1:grid_size, size = n_of_agents, replace = TRUE)
        )
        
        # Label each point on S with a "type"
        S <- Surf %>% add_column(type=sample(1:n_of_types,size=dim(Surf)[1],replace=TRUE))
        
        stop_condition <- FALSE
        tick <- 0
        ts <- 0
        
        while (!stop_condition) {
          # Update dataframe
          results1 <- rbind(results1, 
                            data.frame(surface= surf,
                                       network=net,
                                       p_level = p_,
                                       rep_no = reps,
                                       agent = 1:n_of_agents,
                                       payoff = agents %>%
                                          left_join(S, by = c("x", "y")) %>%
                                          select(z),
                                       tick_count = tick ))
          
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
            ts <- ts + 1
            if (ts == t){
              stop_condition <- TRUE
            }
          }
          else{
            ts <- 0
          }
          
          tick <- tick + 1
          
        }
      }
    }
  }
}
end_time <- Sys.time()
cat("Run time:", end_time - start_time, "\n") 

## fixing early drop out issue ##
res_upd <- expand_grid(surface = problem_type, 
                   network = network_type, 
                   p_level = p, 
                   rep_no = 1:reps , 
                   agent = 1:n_of_agents , 
                   tick_count = 0:max(results1$tick_count))
res_upd<- left_join(res_upd, results1, by = c("surface", "network", "p_level","rep_no","agent","tick_count"))

# fill all NAs with the last non-NA value
library(zoo)
res_upd <- res_upd %>%
  mutate_all(na.locf)

df1 <-res_upd %>% group_by(surface, network, p_level, tick_count, rep_no) %>% 
  summarise(avg_payoff = mean(z),
            max_payoff = max(z)) %>%
  group_by(surface, network, p_level, tick_count) %>%
  summarise(avg_avg_payoff = mean(avg_payoff),
            avg_max_payoff = mean(max_payoff),
            max_max_payoff = max(max_payoff))

df2 <-res_upd %>% group_by(surface, network, p_level, tick_count, rep_no) %>% 
  summarise(count=n_distinct(z)) %>%
  group_by(surface, network, p_level, tick_count) %>%
  summarise(avg_count = mean(count))


library(patchwork)
# Plot Figure 1a: Simple vs. Complex Landscapes - Avg Performance
fig1a_simple <- df1 %>% filter(surface=="simple") %>%
  ggplot(aes(x = tick_count, y = avg_avg_payoff, group = interaction(network, p_level))) +
  geom_line(aes(color=network, linetype = as.factor(p_level)))+
  labs(title = "Simple landscape", x = "Tick", y = "Average performance")+
  guides(
    color = guide_legend(title = "Network type"),
    linetype = guide_legend(title = "Strategy", override.aes = list(colour = c("black", "black"))))+
  theme(legend.position="none")

fig1a_complex <- df1%>% filter(surface=="complex") %>%
  ggplot(aes(x = tick_count, y = avg_avg_payoff, group = interaction(network, p_level))) +
  geom_line(aes(color=network, linetype = as.factor(p_level)))+
  labs(title = "Complex landscape", x = "Tick", y = "Average performance")+
  scale_color_discrete( name = "Network type", labels = c("Fully connected", "Linear"))+
  guides(
    linetype = guide_legend(title = "Strategy", override.aes = list(colour = c("black", "black"))))

fig1a_simple+fig1a_complex

# Plot Figure 1b: Simple vs. Complex Landscapes - Average Max Performance
fig1b_simple <- df1 %>% filter(surface=="simple") %>%
  ggplot(aes(x = tick_count, y = avg_max_payoff, group = interaction(network, p_level))) +
  geom_line(aes(color=network, linetype = as.factor(p_level)))+
  labs(title = "Simple landscape", x = "Tick", y = "Max performance (average)")+
  guides(
    color = guide_legend(title = "Network type"),
    linetype = guide_legend(title = "Strategy", override.aes = list(colour = c("black", "black"))))+
  theme(legend.position="none")

fig1b_complex <- df1%>% filter(surface=="complex") %>%
  ggplot(aes(x = tick_count, y = avg_max_payoff, group = interaction(network, p_level))) +
  geom_line(aes(color=network, linetype = as.factor(p_level)))+
  labs(title = "Complex landscape", x = "Tick", y = "Max performance (average)")+
  scale_color_discrete( name = "Network type", labels = c("Fully connected", "Linear"))+
  guides(
    linetype = guide_legend(title = "Strategy", override.aes = list(colour = c("black", "black"))))

fig1b_simple+fig1b_complex

# Plot Figure 1c: Simple vs. Complex Landscapes - Maximum Max Performance
fig1c_simple <- df1 %>% filter(surface=="simple") %>%
  ggplot(aes(x = tick_count, y = max_max_payoff, group = interaction(network, p_level))) +
  geom_line(aes(color=network, linetype = as.factor(p_level)))+
  labs(title = "Simple landscape", x = "Tick", y = "Max performance (max)")+
  guides(
    color = guide_legend(title = "Network type"),
    linetype = guide_legend(title = "Strategy", override.aes = list(colour = c("black", "black"))))+
  theme(legend.position="none")

fig1c_complex <- df1%>% filter(surface=="complex") %>%
  ggplot(aes(x = tick_count, y = max_max_payoff, group = interaction(network, p_level))) +
  geom_line(aes(color=network, linetype = as.factor(p_level)))+
  labs(title = "Complex landscape", x = "Tick", y = "Max performance (max)")+
  scale_color_discrete( name = "Network type", labels = c("Fully connected", "Linear"))+
  guides(
    linetype = guide_legend(title = "Strategy", override.aes = list(colour = c("black", "black"))))

fig1c_simple+fig1c_complex

# Plot Figure 2: Simple vs. Complex Landscapes - N of Unique Solutions
fig2_simple <- df2 %>% filter(surface=="simple") %>%
  ggplot(aes(x = tick_count, y = avg_count, group = interaction(network, p_level))) +
  geom_line(aes(color=network, linetype = as.factor(p_level)))+
  labs(title = "Simple landscape", x = "Tick", y = "Number of unique solutions")+
  guides(
    color = guide_legend(title = "Network type"),
    linetype = guide_legend(title = "Strategy", override.aes = list(colour = c("black", "black"))))+
  theme(legend.position="none")

fig2_complex <-df2 %>% filter(surface=="complex") %>%
  ggplot(aes(x = tick_count, y = avg_count, group = interaction(network, p_level))) +
  geom_line(aes(color=network, linetype = as.factor(p_level)))+
  labs(title = "Complex landscape", x = "Tick", y = "Number of unique solutions")+
  scale_color_discrete( name = "Network type", labels = c("Fully connected", "Linear"))+
  guides(
    linetype = guide_legend(title = "Strategy", override.aes = list(colour = c("black", "black"))))

fig2_simple+fig2_complex

