# Figure 7: rounds of collaboration before copying x network structure
# Usage (from the repository root): Rscript scripts/simulate_colFirst.R

source("scripts/setup.R")

# Parameters for collaborate first plot
n_of_agents <- 16
n_of_types <- 100
r <- 6
p <- c(0)
t <- 10
network_type <- c("ring", "full")
problem_type <- "complex"
timeLimit <- TRUE
trackHistory <- FALSE
collaborateFirst <- TRUE
timeFirstCopy <- c(0:11) # tick at which agents switch from collaborating to copying
title <- "collaborateFirst"

# Parameter combinations
comb2 <- tidyr::crossing(network_type = network_type, p_ = p, types = n_of_types, problems = problem_type, copyTimes = timeFirstCopy) # all possible combinations

# Run the simulation
results_list <- mclapply(1:nrow(comb2), function(i) {
  row <- comb2[i, ]
  simulate(network_type = row$network_type,
           p_ = row$p_,
           n_of_reps = n_of_reps,
           n_of_agents = n_of_agents,
           n_of_types = row$types,
           distance_matrix = dist_matrix,
           r = r,
           t = t,
           timeLimit = timeLimit,
           trackHistory = trackHistory,
           problem_type = row$problems,
           collaborateFirst = collaborateFirst,
           timeFirstCopy = row$copyTimes)
}, mc.cores = num_cores)

results_t <- bind_rows(results_list)

save_results(results_t, title)
