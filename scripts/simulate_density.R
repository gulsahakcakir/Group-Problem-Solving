# Load necessary libraries and source functions
library(tidyverse)
library(dplyr)
library(ggplot2)
library(igraph)
library(purrr)
library(akima)
library(parallel)

# Set the number of cores to the value of NSLOTS provided by the job scheduler 
# Only valid when running in batch mode on a server or similar environment
num_cores <- as.integer(Sys.getenv("NSLOTS"))

# If NSLOTS is not set, default to 1 core (when testing locally)
if (is.na(num_cores) || num_cores < 1) num_cores <- 1

# Source the function to create the fitness landscape from Mason and Watts, as implemented by Barkoczi and Galesic
source("scripts/MW_landscape.R")

# Source function to create team networks
source("scripts/networks.R")
# Main function is init_network(n_of_agents, network_type)
# network_type can be:
# "full"
# "ring"
# "A" to "H" for networks from Mason and Watts
# A number <= n(n-1)/2-n-1 = (n-2)(n-1)/2, e.g. for n = 16: network_type < 120 (full) - 15 (linear)
# Numeric values add n random edges to the linear network

# Source function to calculate distances on the grid
source("scripts/distances.R")

# Source main simulation function
source("scripts/simulation_func.R")
# Main function is simulate(network_type, p_,  n_of_reps, n_of_agents, n_of_types, distance_matrix, r, t,timeLimit=FALSE,trackHistory=FALSE,problem_type='complex')

# Parameters for network density plot
n_of_agents <-16
n_of_types <-100
r <- 6
p <- c(0, 1)
n_of_reps <- 50
t <- 1
network_type <- seq(0, 105, 5)
problem_type <-  "complex"
timeLimit<-FALSE
trackHistory<-FALSE
timeFirstCopy <- NA
title <- "density"

# Compute distances
grid_size<-100
Grid <-tibble(expand.grid(x=c(1:grid_size),y=c(1:grid_size)))
dist_matrix <- comp_dist(Grid)

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
           problem_type = row$problems)
}, mc.cores = num_cores)

results_d <- bind_rows(results_list)


#remove JOBID part --> if not running in batches
save(results_d, file = paste0("data/", title,"_r",r,"_types",n_of_types,"_timeLimit",timeLimit,t,"_", Sys.getenv("JOB_ID"), ".RData"))






