# THE IMPACT OF PARTYGATE ON IDENTIFICATION WITH THE CONSERVATIVE PARTY #
# R Version 2023.06.1+524 #

# ROBUSTNESS CHECKS #

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
library(reporttools)

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

##### Data preparation (smaller dataset) #####
d_mii <- d_raw[d_raw$wave21 == 1 | d_raw$wave22 == 1,]

# Subset to variables of interest
d_mii <- subset(d_mii, select = c(
  
  # Wave completion variables
  "id", "starttimeW22", "starttimeW21", "wave22", "wave21", 
  
  # Most important issue
  "mii_catW22", "mii_catW21",
  
  # Authoritarian-libertarian values
  "al1W21", "al2W21", "al3W21", "al4W21", "al5W21", "al1W22", "al2W22", "al3W22", "al4W22", "al5W22"))

# Eliminate respondents without id
d_mii <- d_mii[!is.na(d_mii$id),]

# Rename variables
colnames(d_mii) <- c("id", "start_time_W22", "start_time_W21", "W22", "W21", "mii_W22", "mii_W21", "al1_W21", "al2_W21", "al3_W21", "al4_W21", "al5_W21", "al1_W22", "al2_W22", "al3_W22", "al4_W22", "al5_W22")

# Create long-format dataset
d00 <- subset(d_mii, select = c("id", "start_time_W22", "start_time_W21"))
d00 <- d00 %>% pivot_longer(cols = -id, names_to = c("wave"), names_pattern = "start_time_W(.*)", values_to = "start_time")

d11 <- subset(d_mii, select = c("id", "W22", "W21"))
d11 <- d11 %>% pivot_longer(cols = -id, names_to = c("wave"), names_pattern = "W(.*)", values_to = "responded")

d22 <- subset(d_mii, select = c("id", "mii_W22", "mii_W21"))
d22 <- d22 %>% pivot_longer(cols = -id, names_to = c("wave"), names_pattern = "mii_W(.*)", values_to = "mii")

d33 <- subset(d_mii, select = c("id", "al1_W21", "al1_W22"))
d33 <- d33 %>% pivot_longer(cols = -id, names_to = c("wave"), names_pattern = "al1_W(.*)", values_to = "al1")

d44 <- subset(d_mii, select = c("id", "al2_W21", "al2_W22"))
d44 <- d44 %>% pivot_longer(cols = -id, names_to = c("wave"), names_pattern = "al2_W(.*)", values_to = "al2")

d55 <- subset(d_mii, select = c("id", "al3_W21", "al3_W22"))
d55 <- d55 %>% pivot_longer(cols = -id, names_to = c("wave"), names_pattern = "al3_W(.*)", values_to = "al3")

d66 <- subset(d_mii, select = c("id", "al4_W21", "al4_W22"))
d66 <- d66 %>% pivot_longer(cols = -id, names_to = c("wave"), names_pattern = "al4_W(.*)", values_to = "al4")

d77 <- subset(d_mii, select = c("id", "al5_W21", "al5_W22"))
d77 <- d77 %>% pivot_longer(cols = -id, names_to = c("wave"), names_pattern = "al5_W(.*)", values_to = "al5")

# Create long-format dataset
d_mii <- cbind(d00, d11, d22, d33, d44, d55, d66, d77)
d_mii <- subset(d_mii, select = -c(4:5, 7:8, 10:11, 13:14, 16:17, 19:20, 22:23))

##### Appendix B: Analysis of "compliance" #####
# Subset
d_mii_naive <- d_mii[d_mii$wave == 22,]

# Eliminate observations on 7th November 2021
d_mii_naive <- d_mii_naive[!grepl("^2021-12-07", d_mii_naive$start_time), ]

# Assign treatment and control status
indices <- (grepl("2021-12-08", d_mii_naive$start_time) | grepl("2021-12-09", d_mii_naive$start_time) | grepl("2021-12-10", d_mii_naive$start_time) | grepl("2021-12-11", d_mii_naive$start_time) | grepl("2021-12-12", d_mii_naive$start_time) | grepl("2021-12-13", d_mii_naive$start_time) | grepl("2021-12-14", d_mii_naive$start_time) | grepl("2021-12-15", d_mii_naive$start_time))

d_mii_naive$treat <- ifelse(d_mii_naive$id %in% d_mii_naive$id[indices], 1, 0)

# Recode most important issue as dummy (pol. neg)
d_mii_naive_pol <- d_mii_naive
d_mii_naive_pol$mii <- ifelse(d_mii_naive_pol$mii == 4, 1, 0)

# Predict most important issue based on treatment assignment (pol. neg)
mii_pred_pol <- lm(mii ~ treat, data = d_mii_naive_pol)

# Recode most important issue as dummy (par. neg)
d_mii_naive_par <- d_mii_naive
d_mii_naive_par$mii <- ifelse(d_mii_naive_par$mii == 5, 1, 0)

# Predict most important issue based on treatment assignment (par. neg)
mii_pred_par <- lm(mii ~ treat, data = d_mii_naive_par)

# Recode most important issue as dummy (terrorism)
d_mii_naive_ter <- d_mii_naive
d_mii_naive_ter$mii <- ifelse(d_mii_naive_ter$mii == 11, 1, 0)

# Predict most important issue based on treatment assignment (par. neg)
mii_pred_ter <- lm(mii ~ treat, data = d_mii_naive_ter)

# Table
stargazer(mii_pred_pol, mii_pred_par, mii_pred_ter,
          covariate.labels = c("Treated", "Constant"),
          dep.var.caption = c("Most important issue"),
          dep.var.labels.include = F,
          column.labels = c("Bad politics", "Bad party", "Terrorism"),
          float = T,
          header = F,
          digits = 3,
          font.size = "small", 
          label = "tab:tablea1")

##### Appendix C: Descriptive statistics #####
# Remove don't knows from continuous variables
d <- d[d$age != 9999,]
d <- d[d$left_right != 9999,]

# Subset
d_naive <- d[d$wave == 22,]

# Identify categorical variables
vars_cat <- with(d_naive, data.frame(
  "Party id" = factor(d_naive$party_id), 
  "Vote intention" = factor(d_naive$vote_intention),
  "Party id strength" = factor(d_naive$party_id_strength),
  "Gender" = factor(d_naive$gender),
  "Ethnicity" = factor(d_naive$ethnicity),
  "Education" = factor(d_naive$education),
  "Household income" = factor(d_naive$house_income),
  "Socioeconomic status" = factor(d_naive$soc_econ),
  "Newspaper read" = factor(d_naive$paper_read),
  "Vote 2019" = factor(d_naive$vote_2019),
  "Covid death" = factor(d_naive$covid_died)))

# Table
caption_cat <- "Descriptive statistics of categorical variables."
tableNominal(vars = vars_cat, cap = caption_cat, vertical = F, lab = "tab:tablea2", longtable = T)

# Identify continuous variables
vars_cont <- with(d_naive, data.frame(
  "Age" = as.numeric(d_naive$age), 
  "Left-right ideology" = as.numeric(d_naive$left_right)))

# Table
caption_cont <- "Descriptive statistics of continuous variables."
tableContinuous(vars = vars_cont, cap = caption_cont, lab = "tab:tablea3", longtable = F)

##### Appendix D: Covariate balance #####
# Re-initialize dataset
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

# Subset to respondents who completed wave 22
d_naive <- d[d$wave == 22,]

# Eliminate observations on 7th November 2021
d_naive <- d_naive[!grepl("^2021-12-07", d_naive$start_time), ]

# Assign treatment and control status
indices <- (grepl("2021-12-08", d_naive$start_time) | grepl("2021-12-09", d_naive$start_time) | grepl("2021-12-10", d_naive$start_time) | grepl("2021-12-11", d_naive$start_time) | grepl("2021-12-12", d_naive$start_time) | grepl("2021-12-13", d_naive$start_time) | grepl("2021-12-14", d_naive$start_time) | grepl("2021-12-15", d_naive$start_time))

d_naive$treat <- ifelse(d_naive$id %in% d_naive$id[indices], 1, 0)

# Assess covariate balance
cov_balance <- lm(treat  ~ left_right + pol_attention + paper_read + vote_2019 + covid_died + age + gender + ethnicity + education + house_income + soc_econ, data = d_naive)

# Make table
stargazer(cov_balance, 
          covariate.labels = c("Left-right placement", "Political attention", "Newspaper: Daily Mail / Scottish Daily Mail", "Newspaper: Mirror / Daily Record", "Newspaper: Daily Star / Daily Star of Scotland", "Nespaper: Sun", "Nespaper: Daily Telegraph", "Nespaper: Financial Times", "Nespaper: Guardian", "Nespaper: Independent", "Nespaper: Times", "Nespaper: Scotsman", "Nespaper: Herald", "Nespaper: Western Mail", "Nespaper: Local daily", "Nespaper: Other", "Nespaper: None", "2019 vote: Labour", "2019 vote: Liberal Democrat", "2019 vote: SNP", "2019 vote: Plaid Cymru", "2019 vote: UKIP", "2019 vote: Green", "2019 vote: Other", "2019 vote: Brexit Party / Reform UK", "2019 vote: Independent candidate", "2019 vote: Don't know", "Covid died: Yes", "Covid died: Don't know", "Age", "Gender: Female", "Ethnicity: Other White", "Ethnicity: White and Black Caribbean", "Ethnicity: White and Black African", "Ethnicity: White and Asian", "Ethnicity: Other Mixed", "Ethnicity: Indian", "Ethnicity: Pakistani", "Ethnicity: Bangladeshi", "Ethnicity: Other Asian", "Ethnicity: Black Caribbean", "Ethnicity: Black African", "Ethnicity: Other Black", "Ethnicity: Chinese", "Ethnicity: Other", "Ethnicity: Prefer not to say", "Education: Youth training cert. / skillseekers", "Education: Trade apprenticeship", "Education: Clerical and commercial", "Education: City and guilds cert.", "Education: City and guilds cert. advanced", "Education: ONC", "Education: CSE grades 2-5", "Education: CSE grade 1, GCE O Level, GCSE, School cert.", "Education: Scottish Ordinary / Lower cert.", "Education: GCE A Level or Higher cert.", "Education: Scottish higher cert.", "Education: Nursing qual.", "Education: Teaching qual.", "Education: University diploma", "Education: University or CNAA first degree", "Education: University or CNAA higher degree", "Education: other qual.", "Education: Don't know", "Education: Prefer not to say", "Income: 5-10k", "Income: 10-15k", "Income: 15-20k", "Income: 20-25k", "Income: 25-29k", "Income: 30-35k", "Income: 35-40k", "Income: 40-45k", "Income: 45-50k", "Income: 50-60k", "Income: 60-70k", "Income: 70-100k", "Income: 100-150k", "Income: 150k+", "Income: Don't know", "Income: Prefer not to say", "SES: Higher professional", "SES: Lower professional / managerial or higher supervisory", "SES: Intermediate", "SES: Small employer or account worker", "SES: Lower supervisory or technical", "SES: Semi-routine", "SES: routine", "Constant"),
          dep.var.caption = "Treatment",
          dep.var.labels.include = F,
          digits = 3,
          float = T,
          header = F,
          omit.stat = "all",
          label = "tab:tablea4")

##### Appendix E: Difference-in-differences estimation with logistic specification #####
# Re-initialize dataset
d <- cbind(d0, d1, d2, d3, d4, d5)
d <- subset(d, select = -c(4:5, 7:8, 10:11, 13:14, 16))

# Subset to waves of interest
d <- d[d$wave == 21 | d$wave == 22,]

# Recode party identity as Conservative versus non-Conservative
d$party_id <- ifelse(d$party_id == 1, 1, 0)

# Recode vote intention as Conservative versus non-Conservative
d$vote_intention <- ifelse(d$vote_intention == 1, 1, 0)

# Eliminate observations on 7th November 2021
d <- d[!grepl("^2021-12-07", d$start_time), ]

# Assign treatment and control status
indices <- (grepl("2021-12-08", d$start_time) | grepl("2021-12-09", d$start_time) | grepl("2021-12-10", d$start_time) | grepl("2021-12-11", d$start_time) | grepl("2021-12-12", d$start_time) | grepl("2021-12-13", d$start_time) | grepl("2021-12-14", d$start_time) | grepl("2021-12-15", d$start_time))

d$treat <- ifelse(d$id %in% d$id[indices], 1, 0)

# Define pre- and post-treatment variable
d$post <- ifelse(d$wave == 22, 1, 0)

# Logistic regression difference-in-differences estimation PID
did_pid_logistic <- glm(party_id ~ treat*post, 
                        data = d,
                        family = binomial(link = "logit"))

# Logistic regression difference-in-differences estimation SUP
did_sup_logistic <- glm(vote_intention ~ treat*post, 
                        data = d,
                        family = binomial(link = "logit"))

# Table
stargazer(did_pid_logistic, did_sup_logistic,
          covariate.labels = c("Treated", "Post", "Treated*Post", "Constant"),
          dep.var.caption = "",
          dep.var.labels.include = F,
          digits = 3,
          float = T,
          header = F,
          label = "tab:tablea5")

##### Appendix F: Difference-in-differences estimation with placebo outcome variables #####
# Remove don't knows from dependent variables
d_mii <- d_mii[d_mii$al1 != 9999,]
d_mii <- d_mii[d_mii$al2 != 9999,]
d_mii <- d_mii[d_mii$al3 != 9999,]
d_mii <- d_mii[d_mii$al4 != 9999,]
d_mii <- d_mii[d_mii$al5 != 9999,]

# Eliminate observations on 7th November 2021
d_mii <- d_mii[!grepl("^2021-12-07", d_mii$start_time), ]

# Assign treatment and control status
indices <- (grepl("2021-12-08", d_mii$start_time) | grepl("2021-12-09", d_mii$start_time) | grepl("2021-12-10", d_mii$start_time) | grepl("2021-12-11", d_mii$start_time) | grepl("2021-12-12", d_mii$start_time) | grepl("2021-12-13", d_mii$start_time) | grepl("2021-12-14", d_mii$start_time) | grepl("2021-12-15", d_mii$start_time))

d_mii$treat <- ifelse(d_mii$id %in% d_mii$id[indices], 1, 0)

# Define pre- and post-treatment variable
d_mii$post <- ifelse(d_mii$wave == 22, 1, 0)

# Run diff-in-diff OLS regression (al1)
did_al1 <- lm(al1 ~ treat*post, data = d_mii)

# Run diff-in-diff OLS regression (al2)
did_al2 <- lm(al2 ~ treat*post, data = d_mii)

# Run diff-in-diff OLS regression (al3)
did_al3 <- lm(al3 ~ treat*post, data = d_mii)

# Run diff-in-diff OLS regression (al4)
did_al4 <- lm(al4 ~ treat*post, data = d_mii)

# Run diff-in-diff OLS regression (al5)
did_al5 <- lm(al5 ~ treat*post, data = d_mii)

# Table
stargazer(did_al1, did_al2, did_al3, did_al4, did_al5,
          covariate.labels = c("Treated", "Post", "Treated*Post", "Constant"),
          dep.var.caption = "Authoritarian-libertarian values",
          dep.var.labels.include = F,
          column.labels = c("Tradition", "Death penalty", "Authority", "Censorhip", "Sentences"),
          float = T,
          header = F,
          digits = 3,
          font.size = "small", 
          label = "tab:tablea6")

##### Appendix G: Analysis of item non-response in the outcome variables #####
# Re-initialize dataset
d <- cbind(d0, d1, d2, d3, d4, d5)
d <- subset(d, select = -c(4:5, 7:8, 10:11, 13:14, 16))

# Recode categorical variables
d$party_id <- as_factor(d$party_id)
d$vote_intention <- as_factor(d$vote_intention)
d$party_id_strength <- as_factor(d$party_id_strength)
d$paper_read <- as_factor(d$paper_read)
d$vote_2019 <- as_factor(d$vote_2019)
d$covid_died <- as_factor(d$covid_died)
d$gender <- as_factor(d$gender)
d$ethnicity <- as_factor(d$ethnicity)
d$education <- as_factor(d$education)
d$house_income <- as_factor(d$house_income)
d$soc_econ <- as_factor(d$soc_econ) 

# Eliminate observations on 7th November 2021
d <- d[!grepl("^2021-12-07", d$start_time), ]

# Assign treatment and control status
indices <- (grepl("2021-12-08", d$start_time) | grepl("2021-12-09", d$start_time) | grepl("2021-12-10", d$start_time) | grepl("2021-12-11", d$start_time) | grepl("2021-12-12", d$start_time) | grepl("2021-12-13", d$start_time) | grepl("2021-12-14", d$start_time) | grepl("2021-12-15", d$start_time))

d$treat <- ifelse(d$id %in% d$id[indices], 1, 0)

# Subset to waves of interest
d_naive <- d[d$wave == 22,]

# Analyse non-response in party identification
sum(is.na(d_naive$party_id)) / nrow(d_naive) # 28.20% missingness 

# Create missingness variable
d_naive$party_id_na <- as.numeric(is.na(d_naive$party_id))

# Run regression on background covariates
na_pid <- lm(party_id_na ~ left_right + pol_attention + paper_read + vote_2019 + covid_died + age + gender + ethnicity + education + house_income + soc_econ + age, data = d_naive)
summary(na_pid)

# Table
stargazer(na_pid, 
          covariate.labels = c("Left-right placement", "Political attention", "Newspaper: Daily Mail / Scottish Daily Mail", "Newspaper: Mirror / Daily Record", "Newspaper: Daily Star / Daily Star of Scotland", "Nespaper: Sun", "Nespaper: Daily Telegraph", "Nespaper: Financial Times", "Nespaper: Guardian", "Nespaper: Independent", "Nespaper: Times", "Nespaper: Scotsman", "Nespaper: Herald", "Nespaper: Western Mail", "Nespaper: Local daily", "Nespaper: Other", "Nespaper: None", "2019 vote: Labour", "2019 vote: Liberal Democrat", "2019 vote: SNP", "2019 vote: Plaid Cymru", "2019 vote: UKIP", "2019 vote: Green", "2019 vote: Other", "2019 vote: Brexit Party / Reform UK", "2019 vote: Independent candidate", "2019 vote: Don't know", "Covid died: Yes", "Covid died: Don't know", "Age", "Gender: Female", "Ethnicity: Other White", "Ethnicity: White and Black Caribbean", "Ethnicity: White and Black African", "Ethnicity: White and Asian", "Ethnicity: Other Mixed", "Ethnicity: Indian", "Ethnicity: Pakistani", "Ethnicity: Bangladeshi", "Ethnicity: Other Asian", "Ethnicity: Black Caribbean", "Ethnicity: Black African", "Ethnicity: Other Black", "Ethnicity: Chinese", "Ethnicity: Other", "Ethnicity: Prefer not to say", "Education: Youth training cert. / skillseekers", "Education: Trade apprenticeship", "Education: Clerical and commercial", "Education: City and guilds cert.", "Education: City and guilds cert. advanced", "Education: ONC", "Education: CSE grades 2-5", "Education: CSE grade 1, GCE O Level, GCSE, School cert.", "Education: Scottish Ordinary / Lower cert.", "Education: GCE A Level or Higher cert.", "Education: Scottish higher cert.", "Education: Nursing qual.", "Education: Teaching qual.", "Education: University diploma", "Education: University or CNAA first degree", "Education: University or CNAA higher degree", "Education: other qual.", "Education: Don't know", "Education: Prefer not to say", "Income: 5-10k", "Income: 10-15k", "Income: 15-20k", "Income: 20-25k", "Income: 25-29k", "Income: 30-35k", "Income: 35-40k", "Income: 40-45k", "Income: 45-50k", "Income: 50-60k", "Income: 60-70k", "Income: 70-100k", "Income: 100-150k", "Income: 150k+", "Income: Don't know", "Income: Prefer not to say", "SES: Higher professional", "SES: Lower professional / managerial or higher supervisory", "SES: Intermediate", "SES: Small employer or account worker", "SES: Lower supervisory or technical", "SES: Semi-routine", "SES: routine", "Constant"),
          dep.var.caption = "Party id missingness",
          dep.var.labels.include = F,
          digits = 3,
          float = T,
          header = F,
          omit.stat = "all",
          label = "tab:tablea7")

# Analyse non-response in vote intention
sum(is.na(d_naive$vote_intention)) / nrow(d_naive) # 29.49% missingness 

# Create missingness variable
d_naive$vote_intention_na <- as.numeric(is.na(d_naive$vote_intention))

# Run regression on background covariates
na_sup <- lm(vote_intention_na ~ left_right + pol_attention + paper_read + vote_2019 + covid_died + age + gender + ethnicity + education + house_income + soc_econ + age, data = d_naive)
summary(na_sup)

# Table
stargazer(na_sup, 
          covariate.labels = c("Left-right placement", "Political attention", "Newspaper: Daily Mail / Scottish Daily Mail", "Newspaper: Mirror / Daily Record", "Newspaper: Daily Star / Daily Star of Scotland", "Nespaper: Sun", "Nespaper: Daily Telegraph", "Nespaper: Financial Times", "Nespaper: Guardian", "Nespaper: Independent", "Nespaper: Times", "Nespaper: Scotsman", "Nespaper: Herald", "Nespaper: Western Mail", "Nespaper: Local daily", "Nespaper: Other", "Nespaper: None", "2019 vote: Labour", "2019 vote: Liberal Democrat", "2019 vote: SNP", "2019 vote: Plaid Cymru", "2019 vote: UKIP", "2019 vote: Green", "2019 vote: Other", "2019 vote: Brexit Party / Reform UK", "2019 vote: Independent candidate", "2019 vote: Don't know", "Covid died: Yes", "Covid died: Don't know", "Age", "Gender: Female", "Ethnicity: Other White", "Ethnicity: White and Black Caribbean", "Ethnicity: White and Black African", "Ethnicity: White and Asian", "Ethnicity: Other Mixed", "Ethnicity: Indian", "Ethnicity: Pakistani", "Ethnicity: Bangladeshi", "Ethnicity: Other Asian", "Ethnicity: Black Caribbean", "Ethnicity: Black African", "Ethnicity: Other Black", "Ethnicity: Chinese", "Ethnicity: Other", "Ethnicity: Prefer not to say", "Education: Youth training cert. / skillseekers", "Education: Trade apprenticeship", "Education: Clerical and commercial", "Education: City and guilds cert.", "Education: City and guilds cert. advanced", "Education: ONC", "Education: CSE grades 2-5", "Education: CSE grade 1, GCE O Level, GCSE, School cert.", "Education: Scottish Ordinary / Lower cert.", "Education: GCE A Level or Higher cert.", "Education: Scottish higher cert.", "Education: Nursing qual.", "Education: Teaching qual.", "Education: University diploma", "Education: University or CNAA first degree", "Education: University or CNAA higher degree", "Education: other qual.", "Education: Don't know", "Education: Prefer not to say", "Income: 5-10k", "Income: 10-15k", "Income: 15-20k", "Income: 20-25k", "Income: 25-29k", "Income: 30-35k", "Income: 35-40k", "Income: 40-45k", "Income: 45-50k", "Income: 50-60k", "Income: 60-70k", "Income: 70-100k", "Income: 100-150k", "Income: 150k+", "Income: Don't know", "Income: Prefer not to say", "SES: Higher professional", "SES: Lower professional / managerial or higher supervisory", "SES: Intermediate", "SES: Small employer or account worker", "SES: Lower supervisory or technical", "SES: Semi-routine", "SES: routine", "Constant"),
          dep.var.caption = "Vote intention missingness",
          dep.var.labels.include = F,
          digits = 3,
          float = T,
          header = F,
          omit.stat = "all",
          label = "tab:tablea8")