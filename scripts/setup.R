# Shared setup for all simulate_*.R scripts: libraries, model functions,
# number of cores, random seed, and the grid distance matrix.
# Run scripts from the repository root, e.g. Rscript scripts/simulate_overTime.R

# Load necessary libraries
library(tidyverse)
library(igraph)
library(akima)
library(parallel)

# Set the number of cores to the value of NSLOTS provided by the job scheduler
# Only valid when running in batch mode on a server or similar environment
num_cores <- as.integer(Sys.getenv("NSLOTS"))

# If NSLOTS is not set, default to 1 core (when testing locally)
if (is.na(num_cores) || num_cores < 1) num_cores <- 1

# Job ID from the scheduler (SGE); empty when running locally.
# Used in output file names so batches from different jobs don't overwrite each other.
job_id <- Sys.getenv("JOB_ID")

# Master random seed. Set SEED to rerun a batch exactly; otherwise the job ID is used,
# so each cluster job gets a different but reproducible stream.
# L'Ecuyer-CMRG gives independent, reproducible streams across mclapply workers.
# Each replication also records its own seed in the rand_seed column of the results.
RNGkind("L'Ecuyer-CMRG")
seed <- as.integer(Sys.getenv("SEED", unset = ifelse(job_id == "", "2025", job_id)))
set.seed(seed)

# Source the function to create the fitness landscape from Mason and Watts, as implemented by Barkoczi and Galesic
source("scripts/MW_landscape.R")

# Source function to create team networks
source("scripts/networks.R")
# Main function is init_network(n_of_agents, network_type)
# network_type can be:
# "full"
# "ring"
# A number <= n(n-1)/2-n-1 = (n-2)(n-1)/2, e.g. for n = 16: network_type < 120 (full) - 15 (linear)
# Numeric values add n random edges to the linear network

# Source function to calculate distances on the grid
source("scripts/distances.R")

# Source main simulation function
source("scripts/simulation_func.R")
# Main function is simulate(network_type, p_,  n_of_reps, n_of_agents, n_of_types, distance_matrix, r, t,timeLimit=FALSE,trackHistory=FALSE,problem_type='complex')

# Compute distances
grid_size <- 100
Grid <- tibble(expand.grid(x = c(1:grid_size), y = c(1:grid_size)))
dist_matrix <- comp_dist(Grid)

# Replications per job. The saved data were produced as batches of 50 reps per job
# (combine_results() assumes batch_size = 50). Override with N_REPS for quick tests.
n_of_reps <- as.integer(Sys.getenv("N_REPS", unset = "50"))

# Save results to data/<title>_r<r>_types<n_of_types>_timeLimit<timeLimit><t>_<job_id>.RData
save_results <- function(results_t, title) {
  dir.create("data", showWarnings = FALSE)
  file <- paste0("data/", title, "_r", r, "_types", n_of_types, "_timeLimit", timeLimit, t, "_", job_id, ".RData")
  save(results_t, seed, file = file)
  cat("\nSaved", file, "\n")
}
