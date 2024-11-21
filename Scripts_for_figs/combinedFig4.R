############################## Simulations on MW nets ##############################
directory <- "/Users/gakcakir/Library/CloudStorage/Box-Box/gulsah/Teams (lamberson@ucla.edu)/Simple Model/sensitivity/data"
library(tidyverse)
library(patchwork)

folder <- "MW_types" # change the folder name for other analyses (e.g., r, timeLimit)

full_path <- file.path(directory, folder)
setwd(full_path)

################################# # of types ##################################
# List all files matching the pattern
file_list <- list.files(pattern = "MWnets_r6_types_reps50_MWrandom_timeLimit15_\\d+") # adjust based on name patterns in different folders

# Empty list to store each loaded dataset with modifications
results_list <- list()

# Load and modify each file in a loop
for (i in seq_along(file_list)) {
  load(file_list[i])  # Load each file in sequence
  
  # Modify 'rep_no' column to reflect the replication number incrementally
  results <- results_MW %>% mutate(rep_no = rep_no + (i - 1) * 50)
  
  # Save each modified dataset into results_list
  results_list[[i]] <- results
}

# Combine all results in results_list into a single data frame
results4 <- do.call(rbind, results_list)

# Create summary table 
results4_sum <- results4 %>% group_by(network, p_level, n_of_types, rep_no) %>% 
  summarise(avg_payoff = mean(z),
            max_payoff = max(z)) 

results4_sum <- results4_sum %>% mutate(category = ifelse(network %in% c("A","B","C","D","full"), "efficient", "inefficient"))

##Boxplot of all data ##
fig4_1 <- results4_sum %>% filter(p_level == 0.1 & n_of_types == 10) %>%
  ggplot(aes(x = network, y = avg_payoff, color=category)) +
  geom_boxplot(position = "dodge") +
  scale_color_manual(values = c("efficient" = "orange", "inefficient" = "light green")) +
  labs(title = "Sharing solutions (p=0.1)",
       x = "Network type",
       y = "Average payoff",
       color = "Efficiency Category")+
  theme(legend.position = "none")

fig4_9 <- results4_sum %>% filter(p_level == 0.9 & n_of_types == 10) %>%
  ggplot(aes(x = network, y = avg_payoff, color=category)) +
  geom_boxplot(position = "dodge") +
  scale_color_manual(values = c("efficient" = "orange", "inefficient" = "light green")) +
  labs(title = "Sharing perspectives (p=0.9)",
       x = "Network type",
       y = "Average payoff",
       color = "Efficiency Category")+
  theme(legend.position = "none")

fig4_1+fig4_9

### Figure 4a ###
category_order <- c("efficient", "inefficient")
network_order <- c("full", "A", "B", "C", "D", "E", "F", "G", "H", "ring")
n_of_reps <- 1950

summary_df <- results4_sum %>% filter(n_of_types == 10) %>% group_by(network, p_level) %>% 
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
  ylim(55,70)+
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
  ylim(55,70)+
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
  ylim(70,85)+
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
  ylim(70,85)+
  scale_color_manual(values = c("efficient" = "orange", "inefficient" = "light green")) +
  labs(title = "p=0.9",
       x = "Network type",
       y = "Max payoff",
       color = "Efficiency Category")

fig4max_1 + fig4max_9  

