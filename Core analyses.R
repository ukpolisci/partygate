# THE IMPACT OF PARTYGATE ON IDENTIFICATION WITH THE CONSERVATIVE PARTY #
# R Version 2023.06.1+524 #

# CORE ANALYSES #

##### Environment set-up #####
# Set working directory
setwd("~/OneDrive - Nexus365/03. Research and training/Partygate/Data")

# Clear environment
rm(list = ls())

# Load packages
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(gridExtra)
library(stargazer)

# Set seed for reproducibility
set.seed(1234)

# Load data
d_raw <- read_dta("BES panel - wave 26.dta")

##### Data preparation #####
# Subset data to respondents who completed either wave 21 or wave 22 or both (unbalanced panel)
d_wide <- d_raw[d_raw$wave21 == 1 | d_raw$wave22 == 1,]

# Subset to variables of interest
d_wide <- subset(d_wide, select = c(
  
  # Wave completion variables
  "id", "starttimeW26", "starttimeW25", "starttimeW24", "starttimeW23", "starttimeW22", "starttimeW21", "starttimeW20", "starttimeW19", "starttimeW18", "starttimeW17", "starttimeW16", "starttimeW15", "starttimeW14", "starttimeW13", "wave26", "wave25", "wave24", "wave23", "wave22", "wave21", "wave20", "wave19", "wave18", "wave17", "wave16", "wave15", "wave14", "wave13", 
  
  # Party identification variables
  "partyIdW26", "partyIdW25", "partyIdW24", "partyIdW23", "partyIdW22", "partyIdW21", "partyIdW20", "partyIdW19", "partyIdW18", "partyIdW17", "partyIdW16", "partyIdW15", "partyIdW14", "partyIdW13", 
  
  # Vote intention
  "generalElectionVoteW26", "generalElectionVoteW25", "generalElectionVoteW24", "generalElectionVoteW23", "generalElectionVoteW22", "generalElectionVoteW21", "generalElectionVoteW20", "generalElectionVoteW19", "generalElectionVoteW18", "generalElectionVoteW17", "generalElectionVoteW16", "generalElectionVoteW15", "generalElectionVoteW14", "generalElectionVoteW13",
  
  # Party id strength
  "partyIdStrengthW26", "partyIdStrengthW25", "partyIdStrengthW24", "partyIdStrengthW23", "partyIdStrengthW22", "partyIdStrengthW21", "partyIdStrengthW20", "partyIdStrengthW19", "partyIdStrengthW18", "partyIdStrengthW17", "partyIdStrengthW16", "partyIdStrengthW15", "partyIdStrengthW14", "partyIdStrengthW13",
  
  # Background covariates
  "leftRightW21", "polAttentionW21", "p_paper_readW21", "p_past_vote_2019", "coronaDiedW21", "ageW21", "gender", "p_ethnicityW21", "p_educationW21", "p_gross_householdW21", "ns_sec_analyticW21"))

# Eliminate respondents without id
d_wide <- d_wide[!is.na(d_wide$id),]

# Rename variables
colnames(d_wide) <- c("id", "start_time_W26", "start_time_W25", "start_time_W24", "start_time_W23", "start_time_W22", "start_time_W21", "start_time_W20", "start_time_W19", "start_time_W18", "start_time_W17", "start_time_W16", "start_time_W15", "start_time_W14", "start_time_W13", "W26", "W25", "W24", "W23", "W22", "W21", "W20", "W19", "W18", "W17", "W16", "W15", "W14", "W13", 
                      
                      "party_id_W26", "party_id_W25", "party_id_W24", "party_id_W23", "party_id_W22", "party_id_W21", "party_id_W20", "party_id_W19", "party_id_W18", "party_id_W17", "party_id_W16", "party_id_W15", "party_id_W14", "party_id_W13", 
                      
                      "vote_intention_W26", "vote_intention_W25", "vote_intention_W24", "vote_intention_W23", "vote_intention_W22", "vote_intention_W21", "vote_intention_W20", "vote_intention_W19", "vote_intention_W18", "vote_intention_W17", "vote_intention_W16", "vote_intention_W15", "vote_intention_W14", "vote_intention_W13",
                      
                      "party_id_strength_W26", "party_id_strength_W25", "party_id_strength_W24", "party_id_strength_W23", "party_id_strength_W22", "party_id_strength_W21", "party_id_strength_W20", "party_id_strength_W19", "party_id_strength_W18", "party_id_strength_W17", "party_id_strength_W16", "party_id_strength_W15", "party_id_strength_W14", "party_id_strength_W13", 
                      
                      "left_right", "pol_attention", "paper_read", "vote_2019", "covid_died", "age", "gender", "ethnicity", "education", "house_income", "soc_econ")

# Create long-format dataset
d0 <- subset(d_wide, select = c("id", "start_time_W26", "start_time_W25", "start_time_W24", "start_time_W23", "start_time_W22", "start_time_W21", "start_time_W20", "start_time_W19", "start_time_W18", "start_time_W17", "start_time_W16", "start_time_W15", "start_time_W14", "start_time_W13"))
d0 <- d0 %>% pivot_longer(cols = -id, names_to = c("wave"), names_pattern = "start_time_W(.*)", values_to = "start_time")

d1 <- subset(d_wide, select = c("id", "W26", "W25", "W24", "W23", "W22", "W21", "W20", "W19", "W18", "W17", "W16", "W15", "W14", "W13"))
d1 <- d1 %>% pivot_longer(cols = -id, names_to = c("wave"), names_pattern = "W(.*)", values_to = "responded")

d2 <- subset(d_wide, select = c("id", "party_id_W26", "party_id_W25", "party_id_W24", "party_id_W23", "party_id_W22", "party_id_W21", "party_id_W20", "party_id_W19", "party_id_W18", "party_id_W17", "party_id_W16", "party_id_W15", "party_id_W14", "party_id_W13"))
d2 <- d2 %>% pivot_longer(cols = -id, names_to = c("wave"), names_pattern = "party_id_W(.*)", values_to = "party_id")

d3 <- subset(d_wide, select = c("id", "vote_intention_W26", "vote_intention_W25", "vote_intention_W24", "vote_intention_W23", "vote_intention_W22", "vote_intention_W21", "vote_intention_W20", "vote_intention_W19", "vote_intention_W18", "vote_intention_W17", "vote_intention_W16", "vote_intention_W15", "vote_intention_W14", "vote_intention_W13"))
d3 <- d3 %>% pivot_longer(cols = -id, names_to = c("wave"), names_pattern = "vote_intention_W(.*)", values_to = "vote_intention")

d4 <- subset(d_wide, select = c("id", "party_id_strength_W26", "party_id_strength_W25", "party_id_strength_W24", "party_id_strength_W23", "party_id_strength_W22", "party_id_strength_W21", "party_id_strength_W20", "party_id_strength_W19", "party_id_strength_W18", "party_id_strength_W17", "party_id_strength_W16", "party_id_strength_W15", "party_id_strength_W14", "party_id_strength_W13"))
d4 <- d4 %>% pivot_longer(cols = -id, names_to = c("wave"), names_pattern = "party_id_strength_W(.*)", values_to = "party_id_strength")

d5 <- subset(d_wide, select = c("id", "left_right", "pol_attention", "paper_read", "vote_2019", "covid_died", "age", "gender", "ethnicity", "education", "house_income", "soc_econ"))
d5 <- d5[rep(seq_len(nrow(d5)), each = 14), ]

# Create long-format dataset
d <- cbind(d0, d1, d2, d3, d4, d5)
d <- subset(d, select = -c(4:5, 7:8, 10:11, 13:14, 16))

# Recode categorical variables
d$paper_read <- as_factor(d$paper_read)
d$vote_2019 <- as_factor(d$vote_2019)
d$covid_died <- as_factor(d$covid_died)
d$gender <- as_factor(d$gender)
d$ethnicity <- as_factor(d$ethnicity)
d$education <- as_factor(d$education)
d$house_income <- as_factor(d$house_income)
d$soc_econ <- as_factor(d$soc_econ) 

# Calculate sample size for Wave 21
nrow(filter(d, wave == 21 & responded == 1)) # 30,281

# Calculate sample size for Wave 22
nrow(filter(d, wave == 22 & responded == 1)) # 28,113

##### Figure 1a - PID trend over time #####
# Remove NAs from party identification variable
d <- d[!is.na(d$party_id), ]

# Recode party identity as Conservative versus non-Conservative
d$party_id <- ifelse(d$party_id == 1, 1, 0)

# Calculate the proportion of Conservative identifiers in each wave
pid_plot <- d %>%
  group_by(wave) %>%
  mutate(total_in_wave = n()) %>%
  group_by(wave, party_id) %>%
  summarise(proportion = n() / first(total_in_wave), .groups = "drop") %>%
  mutate(party_id = ifelse(party_id == 1, "Conservative", "Non-Conservative"))

# Create subsets
pid_plot_con <- pid_plot %>%
  filter(party_id == "Conservative")
pid_plot_nocon <- pid_plot %>%
  filter(party_id == "Non-Conservative")

# Plot proportion of Conservative identifiers
fig1a <- ggplot(pid_plot_con, 
       aes(x = wave, y = proportion, group = party_id)) +
  geom_line(color = "blue") + 
  geom_point(color = "blue") +
  geom_vline(xintercept = 10, linetype = "dashed", color = "red") +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  ylim(0.10, 0.50) +
  labs(x = "Wave",
       y = "Prop. Conservative identifiers") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 10))

##### Figure 2 - PID trend over time zoomed in  #####
fig2 <- ggplot(pid_plot_con, 
               aes(x = wave, y = proportion, group = party_id)) +
  geom_line(color = "blue") + 
  geom_point(color = "blue") +
  geom_vline(xintercept = 10, linetype = "dashed", , color = "red") +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  labs(x = "Wave",
       y = "Prop. Conservative identifiers") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 10))

ggsave(file = "fig2.jpeg", fig2, width = 6, height = 5) 

##### Difference-in-means estimation PID #####
# Subset to respondents who completed wave 22
d_naive <- d[d$wave == 22,]

# Eliminate observations on 7th November 2021
d_naive <- d_naive[!grepl("^2021-12-07", d_naive$start_time), ]

# Assign treatment and control status
indices <- (grepl("2021-12-08", d_naive$start_time) | grepl("2021-12-09", d_naive$start_time) | grepl("2021-12-10", d_naive$start_time) | grepl("2021-12-11", d_naive$start_time) | grepl("2021-12-12", d_naive$start_time) | grepl("2021-12-13", d_naive$start_time) | grepl("2021-12-14", d_naive$start_time) | grepl("2021-12-15", d_naive$start_time))

d_naive$treat <- ifelse(d_naive$id %in% d_naive$id[indices], 1, 0)

# Check treatment and control group size
n_distinct(d_naive %>% filter(treat == 1) %>% select(id)) # 5,955 treated
n_distinct(d_naive %>% filter(treat == 0) %>% select(id)) # 32,867 control

# Assess covariate balance
cov_balance <- lm(treat  ~ left_right + pol_attention + paper_read + vote_2019 + covid_died + age + gender + ethnicity + education + house_income + soc_econ, data = d_naive)
summary(cov_balance)

# Estimate naive ITT for Conservative identity
itt_naive_pid <- lm(party_id ~ treat, data = d_naive)
summary(itt_naive_pid)

# Estimate naive ITTs controlling for imbalanced covariates
itt_naive_pid_covs <- lm(party_id ~ treat + gender + ethnicity + education, data = d_naive)
summary(itt_naive_pid_covs)

##### Difference-in-differences estimation PID #####
# Subset to waves of interest
d_pid <- d[d$wave == 21 | d$wave == 22, ]

# Eliminate observations on 7th November 2021
d_pid <- d_pid[!grepl("^2021-12-07", d_pid$start_time), ]

# Assign treatment and control status
indices <- (grepl("2021-12-08", d_pid$start_time) | grepl("2021-12-09", d_pid$start_time) | grepl("2021-12-10", d_pid$start_time) | grepl("2021-12-11", d_pid$start_time) | grepl("2021-12-12", d_pid$start_time) | grepl("2021-12-13", d_pid$start_time) | grepl("2021-12-14", d_pid$start_time) | grepl("2021-12-15", d_pid$start_time))

d_pid$treat <- ifelse(d_pid$id %in% d_pid$id[indices], 1, 0)

# Check treatment and control group size
n_distinct(d_pid %>% filter(treat == 1) %>% select(id)) # 5,955 treated
n_distinct(d_pid %>% filter(treat == 0) %>% select(id)) # 33,064 control

# Define pre- and post-treatment variable
d_pid$post <- ifelse(d_pid$wave == 22, 1, 0)

# Run diff-in-diff OLS regression
did_pid <- lm(party_id ~ treat*post, data = d_pid)
summary(did_pid)

##### Table 1 - Results PID #####
stargazer(itt_naive_pid, itt_naive_pid_covs, did_pid, 
          covariate.labels = c("Treated", "Post", "Treated*Post", "Constant"),
          dep.var.caption = "",
          dep.var.labels.include = F,
          column.labels = c("Diff-in-means", "Diff-in-means", "Diff-in-diff"),
          float = T,
          header = F,
          digits = 3,
          add.lines = list(c("Covariates", "No", "Yes", "No")),
          omit = "gender|ethnicity|education",
          font.size = "small", 
          label = "tab:table1")

##### Figure 1b - SUP trend over time #####
# Re-initialise dataset
d <- cbind(d0, d1, d2, d3, d4, d5)
d <- subset(d, select = -c(4:5, 7:8, 10:11, 13:14, 16))

# Recode categorical variables
d$paper_read <- as_factor(d$paper_read)
d$vote_2019 <- as_factor(d$vote_2019)
d$covid_died <- as_factor(d$covid_died)
d$gender <- as_factor(d$gender)
d$ethnicity <- as_factor(d$ethnicity)
d$education <- as_factor(d$education)
d$house_income <- as_factor(d$house_income)
d$soc_econ <- as_factor(d$soc_econ) 

# Remove NAs from vote intention variable
d <- d[!is.na(d$vote_intention), ]

# Recode vote intention as Conservative versus non-Conservative
d$vote_intention <- ifelse(d$vote_intention == 1, 1, 0)

# Calculate the proportion of Conservative voters in each wave
sup_plot <- d %>%
  group_by(wave) %>%
  mutate(total_in_wave = n()) %>%
  group_by(wave, vote_intention) %>%
  summarise(proportion = n() / first(total_in_wave), .groups = "drop") %>%
  mutate(vote_intention = ifelse(vote_intention == 1, "Conservative", "Non-Conservative"))

# Create subsets
sup_plot_con <- sup_plot %>%
  filter(vote_intention == "Conservative")
sup_plot_nocon <- sup_plot %>%
  filter(vote_intention == "Non-Conservative")

# Plot proportion of Conservative identifiers
fig1b <- ggplot(sup_plot_con, 
                aes(x = wave, y = proportion, group = vote_intention)) +
  geom_line(color = "blue") + 
  geom_point(color = "blue") +
  geom_vline(xintercept = 10, linetype = "dashed", color = "red") +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  ylim(0.10, 0.50) +
  labs(x = "Wave",
       y = "Prop. Conservative supporters") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 10))

##### Figure 1 #####
# Print Figure 1a and Figure 1b side-by-side
fig1 <- arrangeGrob(fig1b, fig1a, ncol = 2)

ggsave(file = "fig1.jpeg", fig1, width = 12, height = 5)

##### Difference-in-means estimation SUP #####
# Subset to respondents who completed wave 22
d_naive <- d[d$wave == 22,]

# Eliminate observations on 7th November 2021
d_naive <- d_naive[!grepl("^2021-12-07", d_naive$start_time), ]

# Assign treatment and control status
indices <- (grepl("2021-12-08", d_naive$start_time) | grepl("2021-12-09", d_naive$start_time) | grepl("2021-12-10", d_naive$start_time) | grepl("2021-12-11", d_naive$start_time) | grepl("2021-12-12", d_naive$start_time) | grepl("2021-12-13", d_naive$start_time) | grepl("2021-12-14", d_naive$start_time) | grepl("2021-12-15", d_naive$start_time))

d_naive$treat <- ifelse(d_naive$id %in% d_naive$id[indices], 1, 0)

# Check treatment and control group size
n_distinct(d_naive %>% filter(treat == 1) %>% select(id)) # 5,804 treated
n_distinct(d_naive %>% filter(treat == 0) %>% select(id)) # 21,483 control

# Estimate naive ITT for Conservative support
itt_naive_sup <- lm(vote_intention ~ treat, data = d_naive)
summary(itt_naive_sup)

# Estimate naive ITTs controlling for imbalanced covariates
itt_naive_sup_covs <- lm(vote_intention ~ treat + gender + ethnicity + education, data = d_naive)
summary(itt_naive_sup_covs)

##### Difference-in-differences estimation SUP #####
# Subset to waves of interest
d_sup <- d[d$wave == 21 | d$wave == 22, ]

# Eliminate observations on 7th November 2021
d_sup <- d_sup[!grepl("^2021-12-07", d_sup$start_time), ]

# Assign treatment and control status
indices <- (grepl("2021-12-08", d_sup$start_time) | grepl("2021-12-09", d_sup$start_time) | grepl("2021-12-10", d_sup$start_time) | grepl("2021-12-11", d_sup$start_time) | grepl("2021-12-12", d_sup$start_time) | grepl("2021-12-13", d_sup$start_time) | grepl("2021-12-14", d_sup$start_time) | grepl("2021-12-15", d_sup$start_time))

d_sup$treat <- ifelse(d_sup$id %in% d_sup$id[indices], 1, 0)

# Check treatment and control group size
n_distinct(d_sup %>% filter(treat == 1) %>% select(id)) # 5,804 treated
n_distinct(d_sup %>% filter(treat == 0) %>% select(id)) # 32,707 control

# Define pre- and post-treatment variable
d_sup$post <- ifelse(d_sup$wave == 22, 1, 0)

# Run diff-in-diff OLS regression
did_sup <- lm(vote_intention ~ treat*post, data = d_sup)
summary(did_sup)


##### Table 2 - Results SUP #####
stargazer(itt_naive_sup, itt_naive_sup_covs, did_sup, 
          covariate.labels = c("Treated", "Post", "Treated*Post", "Constant"),
          dep.var.caption = "",
          dep.var.labels.include = F,
          column.labels = c("Diff-in-means", "Diff-in-means", "Diff-in-diff"),
          float = T,
          header = F,
          digits = 3,
          add.lines = list(c("Covariates", "No", "Yes", "No")),
          omit = "gender|ethnicity|education",
          font.size = "small", 
          label = "tab:table2")
##### Difference-in-means estimation PID strength #####
# Re-initialise dataset
d <- cbind(d0, d1, d2, d3, d4, d5)
d <- subset(d, select = -c(4:5, 7:8, 10:11, 13:14, 16))

# Recode categorical variables
d$paper_read <- as_factor(d$paper_read)
d$vote_2019 <- as_factor(d$vote_2019)
d$covid_died <- as_factor(d$covid_died)
d$gender <- as_factor(d$gender)
d$ethnicity <- as_factor(d$ethnicity)
d$education <- as_factor(d$education)
d$house_income <- as_factor(d$house_income)
d$soc_econ <- as_factor(d$soc_econ) 

# Remove NAs from party identification variable
d <- d[!is.na(d$party_id), ]

# Recode party identity as Conservative versus non-Conservative
d$party_id <- ifelse(d$party_id == 1, 1, 0)

# Subset to Conservative respondents
d_naive_con <- d_naive[d_naive$party_id == 1,]

# Remove don't knows from party identity strength
d_naive_con <- d_naive_con[d_naive_con$party_id_strength != 9999,]

# Check treatment and control group size
n_distinct(d_naive_con %>% filter(treat == 1) %>% select(id)) # 1,485 treated
n_distinct(d_naive_con %>% filter(treat == 0) %>% select(id)) # 6,610 control

# Assess covariate balance
cov_balance <- lm(treat  ~ left_right + pol_attention + paper_read + vote_2019 + covid_died + age + gender + ethnicity + education + house_income + soc_econ, data = d_naive_con)
summary(cov_balance)

# Estimate naive ITT for Conservative identity strength
itt_naive_strength <- lm(party_id_strength ~ treat, data = d_naive_con)
summary(itt_naive_strength)

# Estimate naive ITTs controlling for imbalanced covariates
itt_naive_strength_covs <- lm(party_id_strength ~ treat + ethnicity + education, data = d_naive_con)
summary(itt_naive_strength_covs)

##### Difference-in-differences estimation PID strength #####
# Re-initialise dataset
d_pid_con <- d[d$wave == 21 | d$wave == 22, ]

# Subset to Conservative respondents
d_pid_con <- d_pid_con[d_pid_con$party_id == 1,]

# Remove don't knows from party identity strength
d_pid_con <- d_pid_con[d_pid_con$party_id_strength != 9999,]

# Eliminate observations on 7th November 2021
d_pid_con <- d_pid_con[!grepl("^2021-12-07", d_pid_con$start_time), ]

# Assign treatment and control status
indices <- (grepl("2021-12-08", d_pid_con$start_time) | grepl("2021-12-09", d_pid_con$start_time) | grepl("2021-12-10", d_pid_con$start_time) | grepl("2021-12-11", d_pid_con$start_time) | grepl("2021-12-12", d_pid_con$start_time) | grepl("2021-12-13", d_pid_con$start_time) | grepl("2021-12-14", d_pid_con$start_time) | grepl("2021-12-15", d_pid_con$start_time))

d_pid_con$treat <- ifelse(d_pid_con$id %in% d_pid_con$id[indices], 1, 0)

# Check treatment and control group size
n_distinct(d_pid_con %>% filter(treat == 1) %>% select(id)) # 1,498 treated
n_distinct(d_pid_con %>% filter(treat == 0) %>% select(id)) # 10,818 control

# Define pre- and post-treatment variable
d_pid_con$post <- ifelse(d_pid_con$wave == 22, 1, 0)

# Run diff-in-diff OLS regression
did_strength <- lm(party_id_strength ~ treat*post, data = d_pid_con)
summary(did_strength)

##### Table 3 - Results PID strength #####
stargazer(itt_naive_strength, itt_naive_strength_covs, did_strength, 
          covariate.labels = c("Treated", "Post", "Treated*Post", "Constant"),
          dep.var.caption = "",
          dep.var.labels.include = F,
          column.labels = c("Diff-in-means", "Diff-in-means", "Diff-in-diff"),
          float = T,
          header = F,
          digits = 3,
          add.lines = list(c("Covariates", "No", "Yes", "No")),
          omit = "ethnicity|education",
          font.size = "small", 
          label = "tab:table3")

##### Standardised coefficients #####
# PID
did_pid_std <- lm(party_id ~ scale(treat)*scale(post), data = d_pid)
summary(did_pid_std)

# SUP
did_sup_std <- lm(vote_intention ~ scale(treat)*scale(post), data = d_sup)
summary(did_sup_std)

# PID strength
did_strength_std <- lm(party_id_strength ~ scale(treat)*scale(post), data = d_pid_con)
summary(did_strength_std)

##### Figure 3 - Parallel trends assumption PID #####
# Re-initialise dataset
d <- cbind(d0, d1, d2, d3, d4, d5)
d <- subset(d, select = -c(4:5, 7:8, 10:11, 13:14, 16))

# Eliminate observations on 7th November 2021
d <- d[!grepl("^2021-12-07", d$start_time), ]

# Assign treatment and control status
indices <- (grepl("2021-12-08", d$start_time) | grepl("2021-12-09", d$start_time) | grepl("2021-12-10", d$start_time) | grepl("2021-12-11", d$start_time) | grepl("2021-12-12", d$start_time) | grepl("2021-12-13", d$start_time) | grepl("2021-12-14", d$start_time) | grepl("2021-12-15", d$start_time))

d$treat <- ifelse(d$id %in% d$id[indices], 1, 0)

# Create vector of waves
waves <- 14:23

# Initialise empty list
plac_did_pid <- list()

# Loop through the waves, changing the reference category
for (current_wave in waves) {
  
  # Set reference category as previous wave
  d$wave <- relevel(as.factor(d$wave), ref = as.character(current_wave - 1))
  
  # Estimate placebo ITTs
  model <- lm_robust(party_id ~ treat * wave, data = d)
  
  # Store results
  plac_did_pid[[paste0("Wave_", current_wave)]] <- summary(model)
}

# Extract coefficients of interest
pid_coefs14 <- as.data.frame(t(plac_did_pid$Wave_14$coefficients[23, c("Estimate", "CI Lower", "CI Upper")]))
pid_coefs15 <- as.data.frame(t(plac_did_pid$Wave_15$coefficients[23, c("Estimate", "CI Lower", "CI Upper")]))
pid_coefs16 <- as.data.frame(t(plac_did_pid$Wave_16$coefficients[23, c("Estimate", "CI Lower", "CI Upper")]))
pid_coefs17 <- as.data.frame(t(plac_did_pid$Wave_17$coefficients[23, c("Estimate", "CI Lower", "CI Upper")]))
pid_coefs18 <- as.data.frame(t(plac_did_pid$Wave_18$coefficients[23, c("Estimate", "CI Lower", "CI Upper")]))
pid_coefs19 <- as.data.frame(t(plac_did_pid$Wave_19$coefficients[23, c("Estimate", "CI Lower", "CI Upper")]))
pid_coefs20 <- as.data.frame(t(plac_did_pid$Wave_20$coefficients[22, c("Estimate", "CI Lower", "CI Upper")]))
pid_coefs21 <- as.data.frame(t(plac_did_pid$Wave_21$coefficients[23, c("Estimate", "CI Lower", "CI Upper")]))
pid_coefs22 <- as.data.frame(t(plac_did_pid$Wave_22$coefficients[24, c("Estimate", "CI Lower", "CI Upper")]))
pid_coefs23 <- as.data.frame(t(plac_did_pid$Wave_23$coefficients[25, c("Estimate", "CI Lower", "CI Upper")]))

# Create data frame for plotting
plac_did_pid_plot <- rbind(pid_coefs14, pid_coefs15, pid_coefs16, pid_coefs17, pid_coefs18, pid_coefs19, pid_coefs20, pid_coefs21, pid_coefs22, pid_coefs23)

plac_did_pid_plot$wave <- 14:23

colnames(plac_did_pid_plot) <- c("estimate", "ci_low", "ci_high", "wave")

# Plot party identification placebo estimates
plac_plot_pid <- ggplot(plac_did_pid_plot, aes(x = wave, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high)) +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 22, linetype = "dotted", color = "red") +
  scale_x_continuous(breaks = c(14:23)) +
  labs(x = "Wave",
       y = "Estimate") +
  theme_minimal() +
  theme(axis.title = element_text(size = 10))

ggsave("fig3.jpeg", plac_plot_pid, width = 6, height = 5)

##### Figure 4 - Parallel trends assumption SUP #####
# Initialise empty list
plac_did_sup <- list()

# Loop through the waves, changing the reference category
for (current_wave in waves) {
  
  # Set reference category as previous wave
  d$wave <- relevel(as.factor(d$wave), ref = as.character(current_wave - 1))
  
  # Estimate placebo ITTs
  model <- lm_robust(vote_intention ~ treat * wave, data = d)
  
  # Store results
  plac_did_sup[[paste0("Wave_", current_wave)]] <- summary(model)
}

# Extract coefficients of interest
sup_coefs14 <- as.data.frame(t(plac_did_sup$Wave_14$coefficients[24, c("Estimate", "CI Lower", "CI Upper")]))
sup_coefs15 <- as.data.frame(t(plac_did_sup$Wave_15$coefficients[24, c("Estimate", "CI Lower", "CI Upper")]))
sup_coefs16 <- as.data.frame(t(plac_did_sup$Wave_16$coefficients[24, c("Estimate", "CI Lower", "CI Upper")]))
sup_coefs17 <- as.data.frame(t(plac_did_sup$Wave_17$coefficients[24, c("Estimate", "CI Lower", "CI Upper")]))
sup_coefs18 <- as.data.frame(t(plac_did_sup$Wave_18$coefficients[24, c("Estimate", "CI Lower", "CI Upper")]))
sup_coefs19 <- as.data.frame(t(plac_did_sup$Wave_19$coefficients[24, c("Estimate", "CI Lower", "CI Upper")]))
sup_coefs20 <- as.data.frame(t(plac_did_sup$Wave_20$coefficients[24, c("Estimate", "CI Lower", "CI Upper")]))
sup_coefs21 <- as.data.frame(t(plac_did_sup$Wave_21$coefficients[24, c("Estimate", "CI Lower", "CI Upper")]))
sup_coefs22 <- as.data.frame(t(plac_did_sup$Wave_22$coefficients[24, c("Estimate", "CI Lower", "CI Upper")]))
sup_coefs23 <- as.data.frame(t(plac_did_sup$Wave_23$coefficients[25, c("Estimate", "CI Lower", "CI Upper")]))

# Create data frame for plotting
plac_did_sup_plot <- rbind(sup_coefs14, sup_coefs15, sup_coefs16, sup_coefs17, sup_coefs18, sup_coefs19, sup_coefs20, sup_coefs21, sup_coefs22, sup_coefs23)

plac_did_sup_plot$wave <- 14:23

colnames(plac_did_sup_plot) <- c("estimate", "ci_low", "ci_high", "wave")

# Plot vote intention placebo estimates
plac_plot_sup <- ggplot(plac_did_sup_plot, aes(x = wave, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high)) +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 22, linetype = "dotted", color = "red") +
  scale_x_continuous(breaks = c(14:23)) +
  labs(x = "Wave",
       y = "Estimate") +
  theme_minimal() +
  theme(axis.title = element_text(size = 10))

ggsave("fig4.jpeg", plac_plot_sup, width = 6, height = 5)



