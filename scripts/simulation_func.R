# Wrap the simulation code within a function
simulate <- function(network_type, p_,  n_of_reps, n_of_agents, n_of_types, distance_matrix, r, t,timeLimit=FALSE,trackHistory=FALSE,problem_type='complex',collaborateFirst=FALSE,timeFirstCopy=NA) {
  
  # Initialize an empty dataframe to store results
  results <- data.frame()  
  
  # Print the network type and p value to the console
  cat("net:", network_type)
  cat("p:", p_)
  
  
  if(problem_type=='simple'){
    # Create simple landscape
    # Euclidean distance
    d <-function(x,y){sqrt(sum((x-y)^2))}
    
    Surf <-Grid %>% rowwise() %>% mutate(z=(100-d(c(x,y),c(50,50)))^5) #Distance from (50,50) exponentiated
    max_z <- max(Surf$z)
    Surf <-Surf %>% mutate(z=(z/max_z) * 100) %>% ungroup() # Normalize
    # %>% rowwise() %>% mutate(z=z+rnorm(1,sd=.05)) #Normalize and add noise 
  }
  
  
  # Main simulation loop
  for (reps in 1:n_of_reps) {
    
    # Generate and set a random seed
    r_seed <- sample.int(.Machine$integer.max, 1)
    set.seed(r_seed)
    
    # Generate a random surface using the Mason and Watts algorithim
    if(problem_type=='complex'){
      Surf <- MasonWatts(100)
    }
    
    grid_size <- max(Surf$x)
    
    # Initialize the network 
    network <- init_network(n_of_agents, network_type)
    
    # Randomly assign agents to points on grid and to a type
    agents <- data.frame(
      type = sample(1:n_of_types, size = n_of_agents, replace = TRUE),
      x = sample(1:grid_size, size = n_of_agents, replace = TRUE),
      y = sample(1:grid_size, size = n_of_agents, replace = TRUE)
    )
    
    # Assign types to points on grid
    S <- Surf %>% add_column(type = sample(1:n_of_types, size = dim(Surf)[1], replace = TRUE))
    
    stop_condition <- FALSE
    tick <- 0
    ts <- 0
    
    while (!stop_condition) {
      
      if(trackHistory){
        # If trackHistory = TRUE, save the status every time step
        results <- rbind(results, 
                         data.frame(network = network_type,
                                    surface= problem_type,
                                    r = r,
                                    n_of_types = n_of_types,
                                    p_level = p_,
                                    rep_no = reps,
                                    agent = 1:n_of_agents,
                                    payoff = agents %>%
                                      left_join(S, by = c("x", "y")) %>%
                                      select(z),
                                    tick_count = tick,
                                    rand_seed = r_seed,
                                    timeFirstCopy = timeFirstCopy))
      }
      cat("tick: ",tick)
      # Every tick reset stopping condition and update order
      agents_have_options <- rep(TRUE, n_of_agents)
      update_order <- sample(1:n_of_agents)
      
      
      
      
      for (i in update_order) {
        
        agent <- agents[i,]
        current_payoff <- as.numeric(S[agent$x == S$x & agent$y == S$y, "z"])
        
        
        visible_cells <- unique(
          rbind(
            expand.grid(x = max(1, agent$x - 1):min(grid_size, agent$x + 1),
                        y = max(1, agent$y - 1):min(grid_size, agent$y + 1)),
            S %>% 
              filter(type == agent$type) %>%
              mutate(distance = distance_matrix[paste("(", x, ",", y, ")"), paste("(", agent$x, ",", agent$y, ")")]) %>%
              filter(distance < r) %>%
              select(x, y)
          )
        )
        
        
        #If collaborateFirst is TRUE, then always collaborate for tick<timeFirstCopy
        #Then always copy for tick >= timeFirstCopy
        if(collaborateFirst){
          if(tick>=timeFirstCopy){p_ <- 0}else{p_ <- 1}}
        
        if(!(p_==99)){ #If p_ is 99, then use independent problem solvers
          if (runif(1) > p_) {
            # If copying, add neighbors' positions to visible cells
            visible_cells <- unique(rbind(visible_cells, agents[neighbors(network, i),] %>% select(x, y)))
          } else {
            # If collaborating, add cells within distance r of current position and matching neighbors' types to visible cells 
            neighbor_type_matches <- S %>%
              filter(type %in% agents[neighbors(network, i),]$type) %>%
              mutate(distance = distance_matrix[paste("(", x, ",", y, ")"), paste("(", agent$x, ",", agent$y, ")")]) %>% 
              filter(distance < r) %>%
              select(x, y)
            
            visible_cells <- unique(rbind(visible_cells, neighbor_type_matches))
          }
        }
        
        
        # Get the payoffs of visible cells and filter to include only those greater than the current payoff
        visible_payoffs <- S %>% 
          semi_join(visible_cells, by = c("x", "y")) %>% 
          filter(z > current_payoff)
        
        
        # If there are no visible cells with higher payoffs than the current payoff, record that the agent has no options
        if (nrow(visible_payoffs) == 0) {
          agents_have_options[i] <- FALSE
        } else {
          # Select randomly from the highest payoff visible cells and move the agent there
          candidates <- visible_payoffs[which(visible_payoffs$z == max(visible_payoffs$z)),]
          agents[i, c("x", "y")] <- candidates %>% sample_n(1) %>% select(x, y)
        }
      }
      
      
      
      if(!timeLimit){
        if (all(!agents_have_options)) {
          # If all agents are stuck, increase ts (time stuck) by 1 
          ts <- ts + 1
          if (ts == t ) {
            stop_condition <- TRUE
          }
        } else {
          # Otherwise set ts (time stuck) back to zero
          ts <- 0
        }
      }else{
        # If there is a set time limit, stop when it is reached
        if(tick == t-1){stop_condition<-TRUE}
      }
      
      tick <- tick + 1
    }
    
    # Save the final results at the end 
    results <- rbind(results, 
                     data.frame(network = network_type,
                                surface= problem_type,
                                r = r,
                                n_of_types = n_of_types,
                                p_level = p_,
                                rep_no = reps,
                                agent = 1:n_of_agents,
                                payoff = agents %>%
                                  left_join(S, by = c("x", "y")) %>%
                                  select(z),
                                tick_count = tick,
                                rand_seed = r_seed,
                                timeFirstCopy = timeFirstCopy))
  }
  
  return(results)
}