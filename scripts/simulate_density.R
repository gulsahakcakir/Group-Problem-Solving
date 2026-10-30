# Figure 6: copy vs. collaborate by network density
# Usage (from the repository root): Rscript scripts/simulate_density.R

source("scripts/setup.R")

# Parameters for network density plot
n_of_agents <- 16
n_of_types <- 100
r <- 6
p <- c(0, 1)
t <- 1
network_type <- seq(0, 105, 5) # number of random edges added to the linear network
problem_type <- "complex"
timeLimit <- FALSE
trackHistory <- FALSE
collaborateFirst <- FALSE
timeFirstCopy <- NA
title <- "density"

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
