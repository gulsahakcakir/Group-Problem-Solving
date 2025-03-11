library(patchwork)
source("scripts/combine_results.R") # function to combine results from multiple batches
# inputs: data_dir, files_pattern, batch_size=50 (default)
# specify batch_size if other than 50
data_dir <- "data"

###################### Figures 3 and 4 ########################## 
# Parameters for over time plots
n_of_agents <-16
n_of_types <- c(100)
r <- 6
p <- c("0", "0.1", "0.9", "1","Individual")
n_of_reps <- 1
t <- 1 
network_type <- c("full")
problem_type <- c("simple","complex")
timeLimit<-FALSE
trackHistory<-TRUE

# Load simulation data
files_pattern <- "overTime_r6_types100_timeLimitFALSE1_.*\\.RData"

results <- combine_results(data_dir, files_pattern) %>% select(-c("timeFirstCopy"))

# For independent problem solvers
results$p_level<-as.character(results$p_level)
results$p_level[which(results$p_level=="99")]<-"Individual"

## Fixing early drop out issue (only matters for overtime plots)
res_upd <- expand_grid(network = network_type,
                       surface = problem_type,
                       r = r,
                       n_of_types = n_of_types,
                       p_level = p,
                       rep_no = 1:n_of_reps ,
                       agent = 1:n_of_agents ,
                       tick_count = 0:max(results$tick_count))
res_upd<- left_join(res_upd, results, by = c( "network","surface", "r","n_of_types", "p_level","rep_no","agent","tick_count"))

# fill all NAs with the last non-NA value
library(zoo)
res_upd <- res_upd %>%
  mutate_all(na.locf)



df1 <-res_upd %>% group_by(surface, p_level, tick_count, rep_no) %>% 
  summarise(avg_payoff = mean(z),
            max_payoff = max(z)) %>%
  group_by(surface, p_level, tick_count) %>%
  summarise(avg_avg_payoff = mean(avg_payoff),
            avg_max_payoff = mean(max_payoff))

df1<-df1 %>% mutate(allOrMixed=case_match(p_level,c("0","1","Individual")~"all",c("0.9","0.1")~"mixed"))

# Figure 3: Simple vs. Complex Landscapes - Average Max Performance
fig3_simple <- df1 %>% filter(surface=="simple") %>%
  ggplot(aes(x = tick_count, y = avg_max_payoff, group = p_level)) +
  geom_line(aes(color=as.factor(p_level),linetype=allOrMixed))+
  scale_color_manual(values=c("#5ec962","#5ec962","#440154","#440154",'red'))+
  labs(title = "Simple landscape", x = "Time", y = "Best member payoff")+
  guides(color = "none")+
  guides(linetype = "none")+
  theme(legend.position="none")+
  theme_classic()+xlim(0,15)+ylim(45,100)+
  theme(axis.text.y = element_text(margin = margin(r = 8)))


fig3_complex <- df1%>% filter(surface=="complex") %>%
  ggplot(aes(x = tick_count, y = avg_max_payoff, group = p_level)) +
  geom_line(aes(color=as.factor(p_level),linetype=allOrMixed))+
  scale_color_manual(values=c("#5ec962","#5ec962","#440154","#440154",'red'))+
  labs(title = "Complex landscape", x = "Time", y = "Best member payoff")+
  guides(
    color = guide_legend(title = "Collaboration\nFrequency (p)",override.aes=list(linetype=c(1,2,2,1,1))))+guides(linetype='none')+
  theme_classic()+xlim(0,15)+ylim(45,100)+
  theme(axis.text.y = element_text(margin = margin(r = 8)))

fig3 <- fig3_simple + fig3_complex

ggsave("figures/overTimeMax.pdf", plot = fig3, width = 9, height = 3, family = "Times", pointsize = 12)


# Figure 4: Simple vs. Complex Landscapes - Average Ave Performance
fig4_simple <- df1 %>% filter(surface=="simple") %>%
  ggplot(aes(x = tick_count, y = avg_avg_payoff, group = p_level)) +
  geom_line(aes(color=as.factor(p_level),linetype=allOrMixed))+
  scale_color_manual(values=c("#5ec962","#5ec962","#440154","#440154",'red'))+
  labs(title = "Simple landscape", x = "Time", y = "Average payoff")+
  guides(color = "none")+
  guides(linetype = "none")+
  theme(legend.position="none")+
  theme_classic()+xlim(0,15)+ylim(10,100)+
  theme(axis.text.y = element_text(margin = margin(r = 8)))


fig4_complex <- df1%>% filter(surface=="complex") %>%
  ggplot(aes(x = tick_count, y = avg_avg_payoff, group = p_level)) +
  geom_line(aes(color=as.factor(p_level),linetype=allOrMixed))+
  scale_color_manual(values=c("#5ec962","#5ec962","#440154","#440154",'red'))+
  labs(title = "Complex landscape", x = "Time", y = "Average payoff")+
  guides(
    color = guide_legend(title = "Collaboration\nFrequency (p)",override.aes=list(linetype=c(1,2,2,1,1))))+guides(linetype='none')+
  theme_classic()+xlim(0,15)+ylim(10,100)+
  theme(axis.text.y = element_text(margin = margin(r = 8)))


fig4 <- fig4_simple + fig4_complex

ggsave("figures/overTimeAve.pdf", plot = fig4, width = 9, height = 3, family = "Times", pointsize = 12)

################################ Figure 5 ###################################### 
# Load simulation data
files_pattern <- "networkInteraction_r6_types100_timeLimitFALSE1_.*\\.RData"
results <- combine_results(data_dir, files_pattern)

# Create summary table 
results_summary <- results %>% group_by(network, p_level, rep_no) %>% 
  summarise(avg_payoff = mean(z),
            max_payoff = max(z),
            found_peak = (max(z)==100)) %>% 
  group_by(network, p_level) %>%
  summarise(avg_avg_payoff = mean(avg_payoff),
            se_avg_payoff = sd(avg_payoff) / sqrt(n()),
            avg_max_payoff = mean(max_payoff),
            se_max_payoff = sd(max_payoff) / sqrt(n()),
            max_max_payoff = max(max_payoff),
            percent_found_peak = mean(found_peak))

# Figure 5: Communication profile x Network structure - Avg and Max Performances
fig5_avg <- results_summary %>% 
  ggplot(aes(x = p_level, y = avg_avg_payoff, colour = network, group = network)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(y = avg_avg_payoff, 
                    ymin = avg_avg_payoff - se_avg_payoff, 
                    ymax = avg_avg_payoff + se_avg_payoff),
                width=0)+
  scale_color_manual(values = c( "#440154", "#5ec962"), labels = c("Fully connected", "Linear")) +
  guides(color = guide_legend(title = "Network"))+ 
  labs(
    x = "Probability of Collaborating", 
    y= "Average payoff")+
  theme_classic()+
  theme(axis.text.y = element_text(margin = margin(r = 8)))

fig5_max <- results_summary %>%
  ggplot(aes(x = p_level, y = avg_max_payoff, colour = network, group = network)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(y = avg_max_payoff, 
                    ymin = avg_max_payoff - se_max_payoff, 
                    ymax = avg_max_payoff + se_max_payoff),
                width=0)+
  scale_color_manual(values = c( "#440154", "#5ec962"), labels = c("Fully connected", "Linear")) +
  guides(color = "none")+
  theme(legend.position = "none")+ 
  labs(
    x = "Probability of Collaborating", 
    y= "Best member payoff")+
  theme_classic()+
  theme(axis.text.y = element_text(margin = margin(r = 8)))

fig5 <- fig5_max + fig5_avg

ggsave("figures/networkInteraction.pdf", plot = fig5, width = 9, height = 3, family = "Times", pointsize = 12)

################################ Figure 6 ###################################### 
# Load simulation data
files_pattern <- "density_r6_types100_timeLimitFALSE1_.*\\.RData"
results <- combine_results(data_dir, files_pattern)

# Create summary table 
results_summary <- results %>% group_by(network, p_level, rep_no) %>% 
  summarise(avg_payoff = mean(z),
            max_payoff = max(z),
            found_peak = (max(z)==100)) %>% 
  group_by(network, p_level) %>%
  summarise(avg_avg_payoff = mean(avg_payoff),
            se_avg_payoff = sd(avg_payoff) / sqrt(n()),
            avg_max_payoff = mean(max_payoff),
            se_max_payoff = sd(max_payoff) / sqrt(n()),
            max_max_payoff = max(max_payoff),
            percent_found_peak = mean(found_peak)) %>%
  mutate(density = 2 * (n_of_agents-1+network) / (n_of_agents*(n_of_agents-1))) # density = 2*E/n(n-1)

# Figure 6: Copy v Collaborate by Network density - Average performance
fig6_copy <- results_summary %>% filter(p_level==0) %>%
  ggplot(aes(x = density, y = avg_avg_payoff)) +
  geom_line()+
  geom_point() +
  geom_errorbar(aes(y = avg_avg_payoff, 
                    ymin = avg_avg_payoff - se_avg_payoff, 
                    ymax = avg_avg_payoff + se_avg_payoff),
                width=0)+
  labs(title = "Copy (p=0)", x = "Density", y = "Average payoff")+
  guides(color = "none")+
  guides(linetype = "none")+
  theme(legend.position="none")+
  theme_classic()+
  theme(axis.text.y = element_text(margin = margin(r = 8)))

fig6_collaborate <- results_summary %>% filter(p_level==1) %>%
  ggplot(aes(x = density, y = avg_avg_payoff)) +
  geom_line()+
  geom_point()+
  geom_errorbar(aes(y = avg_avg_payoff, 
                    ymin = avg_avg_payoff - se_avg_payoff, 
                    ymax = avg_avg_payoff + se_avg_payoff),
                width=0)+
  labs(title = "Collaborate (p=1)", x = "Density", y = "Average payoff")+
  guides(color = "none")+
  guides(linetype = "none")+
  theme(legend.position="none")+
  theme_classic()+
  ylim(45,49)+
  theme(axis.text.y = element_text(margin = margin(r = 8)))

fig6 <- fig6_copy + fig6_collaborate

ggsave("figures/networkInteraction.pdf", plot = fig6, width = 9, height = 3, family = "Times", pointsize = 12)


################################ Figure 7 ###################################### 
# Load simulation data
files_pattern <- "collaborateFirst_r6_types100_timeLimitTRUE10_.*\\.RData"
results <- combine_results(data_dir, files_pattern)

# Create summary table 
results_summary <- results %>% group_by(network, timeFirstCopy, rep_no) %>% 
  summarise(avg_payoff = mean(z),
            max_payoff = max(z),
            found_peak = (max(z)==100)) %>% 
  group_by(network, timeFirstCopy) %>%
  summarise(avg_avg_payoff = mean(avg_payoff),
            se_avg_payoff = sd(avg_payoff) / sqrt(n()),
            avg_max_payoff = mean(max_payoff),
            se_max_payoff = sd(max_payoff) / sqrt(n()),
            max_max_payoff = max(max_payoff),
            percent_found_peak = mean(found_peak))


# Figure 7: # of rounds of collaboration x Network structure - Avg and Max Performances

collabFirstMax <- results_summary %>%
  ggplot(aes(x = timeFirstCopy, y = avg_max_payoff, colour = network, group = network)) +
  geom_line() +
  geom_point() +
  ylim(44,75)+
  geom_errorbar(aes(y = avg_max_payoff, 
                    ymin = avg_max_payoff - se_max_payoff, 
                    ymax = avg_max_payoff + se_max_payoff),
                width=0)+
  scale_color_manual(values = c( "#440154", "#5ec962"), labels = c("Fully connected", "Linear")) +
  scale_x_continuous(breaks = seq(0, t, by = 2),limits=c(0,t))+
  guides(color = "none")+
  theme(legend.position = "none")+ 
  labs(#title = paste("r =",results[1,]$r,"   types =",results[1,]$n_of_types), 
    x = "Rounds of collaboration (k)", 
    y= "Best member payoff")+
  theme_classic()+
  theme(axis.text.y = element_text(margin = margin(r = 8)))

collabFirstAve <- results_summary %>%
  ggplot(aes(x = timeFirstCopy, y = avg_avg_payoff, colour = network, group = network)) +
  geom_line() +
  geom_point() +
  ylim(44,75)+
  geom_errorbar(aes(y = avg_avg_payoff, 
                    ymin = avg_avg_payoff - se_avg_payoff, 
                    ymax = avg_avg_payoff + se_avg_payoff),
                width=0)+
  scale_color_manual(values = c( "#440154", "#5ec962"), labels = c("Fully connected", "Linear")) +
  scale_x_continuous(breaks = seq(0, t, by = 2),limits=c(0,t))+
  guides(color = guide_legend(title = "Network"))+ 
  #theme(legend.position = "none")+ 
  labs(#title = paste("r =",results[1,]$r,"   types =",results[1,]$n_of_types), 
    x = "Rounds of collaboration (k)", 
    y= "Average payoff")+
  theme_classic()+
  theme(axis.text.y = element_text(margin = margin(r = 8)))

fig7 <- collabFirstMax + collabFirstAve

ggsave("figures/collabFirst.pdf", plot = fig7, width = 9, height = 3, family = "Times", pointsize = 12)
