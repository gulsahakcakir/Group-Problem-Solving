setwd('/Users/gakcakir/Library/CloudStorage/Box-Box/gulsah/Teams (lamberson@ucla.edu)/Simple Model/data/density')
library(tidyverse)
##################### Load simulation data #####################

load(file='density_p01_seq5_r10_types100_reps50_MWrandom_notimeLimit')
results1<-results
load(file='density_p01_seq5_r10_types100_reps50_MWrandom_notimeLimit_2')
results2<-results %>% mutate(rep_no=rep_no+10^2)
load(file='density_p01_seq5_r10_types100_reps50_MWrandom_notimeLimit_3')
results3<-results %>% mutate(rep_no=rep_no+2*10^2)
load(file='density_p01_seq5_r10_types100_reps50_MWrandom_notimeLimit_4')
results4<-results %>% mutate(rep_no=rep_no+3*10^2)
load(file='density_p01_seq5_r10_types100_reps50_MWrandom_notimeLimit_5')
results5<-results %>% mutate(rep_no=rep_no+4*10^2)
load(file='density_p01_seq5_r10_types100_reps50_MWrandom_notimeLimit_6')
results6<-results %>% mutate(rep_no=rep_no+5*10^2)
load(file='density_p01_seq5_r10_types100_reps50_MWrandom_notimeLimit_7')
results7<-results %>% mutate(rep_no=rep_no+6*10^2)
load(file='density_p01_seq5_r10_types100_reps50_MWrandom_notimeLimit_8')
results8<-results %>% mutate(rep_no=rep_no+7*10^2)
load(file='density_p01_seq5_r10_types100_reps50_MWrandom_notimeLimit_9')
results9<-results %>% mutate(rep_no=rep_no+8*10^2)
load(file='density_p01_seq5_r10_types100_reps50_MWrandom_notimeLimit_10')
results10<-results %>% mutate(rep_no=rep_no+9*10^2)

# Combine all replications
results<-rbind(results1,results2,results3,results4,results5,results6,results7,results8,results9,results10)

###################### Analysis ###################### 
# Create summary table 
results_summary <- results %>% group_by(network, p_level, rep_no) %>% 
  summarise(avg_payoff = mean(z),
            max_payoff = max(z),
            found_peak = (max(z)==100)) %>% 
  group_by(network, p_level) %>%
  summarise(avg_avg_payoff = mean(avg_payoff),
            avg_max_payoff = mean(max_payoff),
            max_max_payoff = max(max_payoff),
            percent_found_peak = mean(found_peak)) %>%
  mutate(density = 2 * (n_of_agents-1+network) / (n_of_agents*(n_of_agents-1))) # density = 2*E/n(n-1)

results_partial_summary <- results %>% group_by(network, p_level, rep_no) %>% 
  summarise(avg_payoff = mean(z),
            max_payoff = max(z)) %>%
  mutate(density = 2 * (n_of_agents-1+network) / (n_of_agents*(n_of_agents-1)))


#results_partial_summary%>%ggplot(aes(x=(1-p_level),colour = network, group = network)) + geom_point(aes(y=max_payoff))
#results_summary %>% ggplot(aes(x=(1-p_level),colour = network, group = network)) + geom_line(aes(y=percent_found_peak))

# Plot Fig 6 (density)
ggplot(results_summary, aes(x = density, y = avg_avg_payoff)) +
  #geom_smooth() +    
  geom_line(size = 1) + 
  geom_point(size = 1) + 
  facet_wrap(~ p_level, ncol = 2,               
             labeller = labeller(p_level = c("0" = "Collaborate (p = 0)", "1" = "Copy (p = 1)"))) +
  labs(
    title = "",
    x = "Density",
    y = "Average Payoff"
  ) +
  theme_minimal() +                     
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

ggplot(results_partial_summary, aes(x = as.factor(density), y = avg_payoff)) + 
  geom_boxplot() +  
  facet_wrap(~ p_level, ncol = 2, 
             labeller = labeller(p_level = c("0" = "Collaborate (p = 0)", "1" = "Copy (p = 1)"))) +  
  labs(
    title = "",
    x = "Density",
    y = "Average Payoff"
  ) +  
  theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )
