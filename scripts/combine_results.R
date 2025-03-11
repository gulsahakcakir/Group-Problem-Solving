combine_results <- function(data_dir, files_pattern, batch_size = 50) {
  # Get a list of all matching files in the specified directory
  files <- list.files(path = data_dir, pattern = files_pattern, full.names = TRUE)
  
  # Initialize an empty data frame to store combined results
  combined_results <- data.frame()
  
  # Load and modify each file in a loop
  for (i in seq_along(files)) {
    load(files[i])  # Load each file in sequence
    
    # Modify 'rep_no' column to reflect the replication number incrementally
    results <- results %>% mutate(rep_no = rep_no + (i - 1) * batch_size)
    
    # Append results_t to the combined results
    combined_results <- rbind(combined_results, results)
  }
  
  # Return the combined results
  return(combined_results)
}
