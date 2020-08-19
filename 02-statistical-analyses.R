library(tidyverse)
library(forcats)
library(fields)
library(factoextra)
source("C:/Users/vinim/Documents/MEGA/Documents/PhD/Year-1/Pilot/pragmatic_spr-master/02_spr_pilot_minimal/results/rscripts/helpers.R")
library(brms)
library(bayesplot)
options(mc.cores = parallel::detectCores ())

# Import data
d <- read_csv("./data/results-25-11.csv")

# Check number of data points per participant
# table(d$submission_id, d$trial_number)

# Remove incomplete submissions and practice trials
d <- d %>% 
  filter(submission_id != 7950, submission_id != 7954) %>% 
  filter(trial_name != "practice_task_three")

# Convert variable 'trial_type' to factor
d$trial_type <- as.factor(d$trial_type)

# Clean data set
# Remove unnecessary variables
d2 <- d %>%
  filter(is.na(trial_type)) %>%
  select(-trial_type, -property_1, -property_2, -shape_1, -shape_2, -response_1, -response_2, -response_3, -display, -experiment_id) %>% 
  gather(Region, RT, -submission_id, -quantifier, -picture_type, -condition, -response, -listNumber,
         -trial_number, -picture, -trial_name)

# Convert variable 'Region' to factor
d2$Region = factor(x = d2$Region,
                   levels = c("QUANT", "der", "SHAPE", "auf",
                              "dem", "Bild", "sind", "CRIT_3", "der_1",
                              "Box"))

# Convert multiple variables to factor
d2$submission_id <- as.factor(d2$submission_id)
d2$quantifier <- as.factor(d2$quantifier)
d2$condition <- as.factor(d2$condition)

# Check for outliers
# Check for outlier trials (= RTs larger/ smaller than 2.5 SD of the mean)
# Check for outlier participants (= total number of outlier trials larger than a third of all trials)
d2 <- d2 %>% group_by(Region, quantifier, condition) %>%
  mutate(outlier_trial = RT < (mean(RT) - 2.5*sd(RT)) | RT > (mean(RT) + 2.5*sd(RT)) ) %>%
  ungroup() %>% 
  group_by(submission_id) %>% 
  mutate(outlier_participant = mean(outlier_trial, na.rm = TRUE) > 0.3) %>% 
  ungroup() 

# Show outliers
show(paste0("Excluded trials: ", sum(d2$outlier_trial, na.rm = TRUE) ))
show(paste0("Excluded participants: ", sum(d2$outlier_participant) ))

# Save data set with removed outliers
d2 <- d2 %>% filter(outlier_trial == FALSE) %>% 
  select(-outlier_trial, -outlier_participant)

# Create variable 'logCon' (= logical conditions)
d2Logic <- d2 %>%
  filter(trial_name != "practice_task_three") %>% 
  mutate(logCon = case_when(quantifier == "some" & condition == "unbiased" ~ "Einige (Unbiased)",
                            quantifier == "some" & condition == "biased" ~ "Einige (Biased)",
                            quantifier == "some" & condition == "false" ~ "Einige (False)",
                            quantifier == "some" & condition == "underspecified" ~ "Einige (Infelict)",
                            quantifier == "all" & condition == "unbiased" ~ "Alle (Unbiased)",
                            quantifier == "all" & condition == "biased" ~ "Alle (Biased)",
                            quantifier == "all" & condition == "false" ~ "Alle (False)",
                            TRUE ~ "other")) %>% 
  select(-quantifier, -condition) %>% 
  filter(picture_type != "A" | logCon != "Einige (False)")

# Convert variable 'logCon' to factor
d2Logic$logCon <- factor(x = d2Logic$logCon, levels = c("Einige (Biased)", "Einige (Unbiased)", "Einige (Infelict)", "Alle (Biased)", "Alle (Unbiased)", "Einige (False)", "Alle (False)"))

# Extract picture labels to be used in the random structure of the model
d2Logic$picture <- str_sub(d2Logic$picture, 12)
d2Logic$picture <- gsub("-", "_", d2Logic$picture)
d2Logic$picture <- gsub("/", "_", d2Logic$picture)
d2Logic$picture <- str_sub(d2Logic$picture, end = -5)

# Filter unique item labels
picture_levels <- sort(unique(d2Logic$picture))

# Save unique item labels as levels of the variable 'picture'
levels(d2Logic$picture) <- picture_levels

# Reorder levels of variable 'logCon'
# Set "Einige (Biased)" as reference level
d2Logic$logCon = factor(d2Logic$logCon, ordered = F, levels = c("Einige (Biased)", "Einige (Unbiased)", "Einige (Infelict)", "Einige (False)", "Alle (Biased)", "Alle (Unbiased)", "Alle (False)"))

# Convert variable 'picture' to factor
d2Logic$picture = factor(x = d2Logic$picture, levels = picture_levels)

# Run statistical model predicting RTs at the SHAPE region as a function of the logical conditions 
RT_SHAPE_SOME = brm(log(RT) ~ logCon +
                          (1 + logCon | submission_id) +  
                          (1 | picture), 
                        filter(d2Logic, Region == "SHAPE"))
  
# Save output (model object)
saveRDS(RT_SHAPE_SOME, "RT_SHAPE_SOME.RDS")


# Reorder levels of variable 'logCon'
# Set "Alle (Biased)" as reference level
d2Logic$logCon = factor(d2Logic$logCon, ordered = F, levels = c("Alle (Biased)", "Alle (Unbiased)", "Alle (False)", "Einige (Biased)", "Einige (Unbiased)", "Einige (Infelict)", "Einige (False)"))

# Run statistical model predicting RTs at the SHAPE region as a function of the logical conditions 
RT_SHAPE_ALL = brm(log(RT) ~ logCon +
                      (1 + logCon | submission_id) +  
                      (1 | picture), 
                    filter(d2Logic, Region == "SHAPE"))

# Save output (model object)
saveRDS(RT_SHAPE_ALL, "RT_SHAPE_ALL.RDS")

# Load model object (model with "Einige (Biased)" as reference)
# RT_SHAPE_SOME <- readRDS("RT_SHAPE_SOME.RDS")

# Load model object (model with "Alle (Biased)" as reference)
# RT_SHAPE_ALL <- readRDS("RT_SHAPE_ALL.RDS")
  
# Extract summary of samples (model with "Einige (Biased)" as reference)
# Show probabilities of relevant comparisons
samples_summary_SHAPE_SOME <-  posterior_samples(RT_SHAPE_SOME) %>%
summarize(ProbabilityEinigeBiasedFasterEinigeUnbiased = sum((b_logConEinigeUnbiased > 0)) / n() ,
          ProbabilityEinigeBiasedFasterEinigeInfelict = sum((b_logConEinigeInfelict > 0)) / n(),
          ProbabilityEinigeBiasedFasterEinigeFalse = sum((b_logConEinigeFalse > 0)) / n()) %>%
  gather(key = "Hypothesis", value = "Probability") %>%
  mutate(Hypothesis = recode(Hypothesis, "ProbabilityEinigeBiasedFasterEinigeUnbiased" = "Unbiased > Biased",
                              "ProbabilityEinigeBiasedFasterEinigeInfelict" = "Infelict > Biased",
                             "ProbabilityEinigeBiasedFasterEinigeFalse" = "False > Biased"))

# Extract summary of samples (model with "Alle (Biased)" as reference)
# Show probabilities of relevant comparisons
samples_summary_SHAPE_ALL <-  posterior_samples(RT_SHAPE_ALL) %>%
summarize(ProbabilityAlleBiasedFasterAlleUnbiased = sum((b_logConAlleUnbiased > 0)) / n() ,
          ProbabilityAlleBiasedFasterAlleFalse = sum((b_logConAlleFalse > 0)) / n() ) %>%
  gather(key = "Hypothesis", value = "Probability") %>%
  mutate(Hypothesis = recode(Hypothesis, "ProbabilityAlleBiasedFasterAlleUnbiased" = "Unbiased > Biased",
                              "ProbabilityAlleBiasedFasterAlleFalse" = "False > Biased"))

# Produce clean model summary (model estimates, credibility interval)
# (model with "Einige (Biased)" as reference)
RT_SHAPE_SOME_tidy <- broom.mixed::tidy(RT_SHAPE_SOME)

# Filter relevant information (fixed effects)
RT_SHAPE_SOME_tidy2 <- filter(RT_SHAPE_SOME_tidy, effect == "fixed") %>% 
  select(-effect, -component, -group, -std.error) %>% 
  mutate_if(is.numeric, ~ round(., 2))


# Produce clean model summary (model estimates, credibility interval)
# (model with "Alle (Biased)" as reference)
RT_SHAPE_ALL_tidy <- broom.mixed::tidy(RT_SHAPE_ALL)

# Filter relevant information (fixed effects)
RT_SHAPE_ALL_tidy2 <- filter(RT_SHAPE_ALL_tidy, effect == "fixed") %>% 
  select(-effect, -component, -group, -std.error) %>% 
  mutate_if(is.numeric, ~ round(., 2))
