# Figures 3 and 4: performance over time on simple vs. complex landscapes
# Usage (from the repository root): Rscript scripts/simulate_overTime.R

source("scripts/setup.R")

# Parameters for over time plots
n_of_agents <- 16
n_of_types <- 100
r <- 6
p <- c(0, 0.1, 0.9, 1, 99) # 99 = independent problem solvers (no interaction)
t <- 1
network_type <- "full"
problem_type <- c("simple", "complex")
timeLimit <- FALSE
trackHistory <- TRUE
collaborateFirst <- FALSE
timeFirstCopy <- NA
title <- "overTime"

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
