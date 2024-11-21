library(ggthemes)
load(file='data/linear_r3_types10_reps100_MWrandom')
results1<-results
load(file='data/linear_r3_types10_reps100_MWrandom2')
results2<-results %>% mutate(rep_no=rep_no+10^3)
load(file='data/linear_r3_types10_reps100_MWrandom3')
results3<-results %>% mutate(rep_no=rep_no+10^4)
load(file='data/linear_r3_types10_reps100_MWrandom4')
results4<-results %>% mutate(rep_no=rep_no+10^5)
load(file='data/linear_r3_types10_reps100_MWrandom5')
results5<-results %>% mutate(rep_no=rep_no+10^6)

results<-rbind(results1,results2,results3,results4,results5)


# Create summary table 
results_summary <- results %>% group_by(network, p_level, rep_no) %>% 
  summarise(avg_payoff = mean(z),
            max_payoff = max(z),
            found_peak = (max(z)==100)) %>% 
  group_by(network, p_level) %>%
  summarise(avg_avg_payoff = mean(avg_payoff),
            avg_max_payoff = mean(max_payoff),
            max_max_payoff = max(max_payoff),
            percent_found_peak = mean(found_peak))

results_partial_summary <- results %>% group_by(network, p_level, rep_no) %>% 
  summarise(avg_payoff = mean(z),
            max_payoff = max(z))


#results_partial_summary%>%ggplot(aes(x=(1-p_level),colour = network, group = network)) + geom_point(aes(y=max_payoff))
#results_summary %>% ggplot(aes(x=(1-p_level),colour = network, group = network)) + geom_line(aes(y=percent_found_peak))


# Plot Fig 3
results_summary %>% 
  ggplot(aes(x = (1-p_level), colour = network, group = network)) +
  geom_smooth(aes(y = avg_avg_payoff, linetype = "Average Payoff"),se = F) +
  geom_smooth(aes(y = avg_max_payoff, linetype = "Max Payoff"), se = F) +
  scale_color_manual(values = c( "#440154", "#5ec962"), labels = c("Fully connected", "Ring")) +
  scale_linetype_manual(name = "Line Type", values = c("solid", "dotted")) +
  guides(
    color = guide_legend(title = "Network Type"),
    linetype = guide_legend(title = "Line Type", override.aes = list(colour = c("black", "black")))
  )+ 
  labs(title = "", 
       x = "Probability of Sharing Perspectives", 
       y= "Solution Payoff")+
  theme(
    panel.background = element_rect(fill = "white", color = "black"),
  )+theme_classic()



#Fig. 3 not smoothed
results_summary %>% 
  ggplot(aes(x = (1-p_level), colour = network, group = network)) +
  geom_line(aes(y = avg_avg_payoff, linetype = "Average Payoff"),se = F) +
  geom_line(aes(y = avg_max_payoff, linetype = "Max Payoff"), se = F) +
  scale_color_manual(values = c( "#440154", "#5ec962"), labels = c("Fully connected", "Ring")) +
  scale_linetype_manual(name = "Line Type", values = c("solid", "dotted")) +
  guides(
    color = guide_legend(title = "Network Type"),
    linetype = guide_legend(title = "Line Type", override.aes = list(colour = c("black", "black")))
  )+ 
  labs(title = "r = 3", 
       x = "Probability of Sharing Perspectives", 
       y= "Solution Payoff")+
  theme(
    panel.background = element_rect(fill = "white", color = "black"),
  )


########################TIME LIMIT 20
load(file='linear_r3_types10_reps100_MWrandom_timeLimit20')
results1<-results
load(file='linear_r3_types10_reps100_MWrandom_timeLimit20_2')
results2<-results %>% mutate(rep_no=rep_no+10^3)
# load(file='linear_r3_types10_reps100_MWrandom3')
# results3<-results %>% mutate(rep_no=rep_no+10^4)
# load(file='linear_r3_types10_reps100_MWrandom4')
# results4<-results %>% mutate(rep_no=rep_no+10^5)
# load(file='linear_r3_types10_reps100_MWrandom5')
# results5<-results %>% mutate(rep_no=rep_no+10^6)

results<-rbind(results1,results2)
#,results3,results4,results5)


# Create summary table 
results_summary <- results %>% group_by(network, p_level, rep_no) %>% 
  summarise(avg_payoff = mean(z),
            max_payoff = max(z),
            found_peak = (max(z)==100)) %>% 
  group_by(network, p_level) %>%
  summarise(avg_avg_payoff = mean(avg_payoff),
            avg_max_payoff = mean(max_payoff),
            max_max_payoff = max(max_payoff),
            percent_found_peak = mean(found_peak))

results_partial_summary <- results %>% group_by(network, p_level, rep_no) %>% 
  summarise(avg_payoff = mean(z),
            max_payoff = max(z))


#results_partial_summary%>%ggplot(aes(x=(1-p_level),colour = network, group = network)) + geom_point(aes(y=max_payoff))
#results_summary %>% ggplot(aes(x=(1-p_level),colour = network, group = network)) + geom_line(aes(y=percent_found_peak))


# Plot Fig 3
results_summary %>% 
  ggplot(aes(x = (1-p_level), colour = network, group = network)) +
  geom_smooth(aes(y = avg_avg_payoff, linetype = "Average Payoff"),se = F) +
  geom_smooth(aes(y = avg_max_payoff, linetype = "Max Payoff"), se = F) +
  scale_color_manual(values = c( "#440154", "#5ec962"), labels = c("Fully connected", "Ring")) +
  scale_linetype_manual(name = "Line Type", values = c("solid", "dotted")) +
  guides(
    color = guide_legend(title = "Network Type"),
    linetype = guide_legend(title = "Line Type", override.aes = list(colour = c("black", "black")))
  )+ 
  labs(title = "", 
       x = "Probability of Sharing Perspectives", 
       y= "Solution Payoff")+
  theme(
    panel.background = element_rect(fill = "white", color = "black"),
  )+theme_classic()



#Fig. 3 not smoothed
results_summary %>% 
  ggplot(aes(x = (1-p_level), colour = network, group = network)) +
  geom_line(aes(y = avg_avg_payoff, linetype = "Average Payoff"),se = F) +
  geom_line(aes(y = avg_max_payoff, linetype = "Max Payoff"), se = F) +
  scale_color_manual(values = c( "#440154", "#5ec962"), labels = c("Fully connected", "Ring")) +
  scale_linetype_manual(name = "Line Type", values = c("solid", "dotted")) +
  guides(
    color = guide_legend(title = "Network Type"),
    linetype = guide_legend(title = "Line Type", override.aes = list(colour = c("black", "black")))
  )+ 
  labs(title = "r = 3", 
       x = "Probability of Sharing Perspectives", 
       y= "Solution Payoff")+
  theme(
    panel.background = element_rect(fill = "white", color = "black"),
  )


