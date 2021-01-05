library(plyr)
library(tidyverse)
library(bootstrap)
library(png)
library(magick)
library(ggrepel)
library(ggpubr)
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  mean(x,na.rm=na.rm) - quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) - mean(x,na.rm=na.rm)}


# d <- read_csv("results-25-11-fixed.csv", locale = locale(encoding = 'ISO-8859-1'))
d <- read_csv("results-25-11.csv", locale = locale(encoding = 'ISO-8859-1'))

d2 <- d %>%
  filter(trial_type == "dropdown_sentence_completion") %>% 
  select(-experiment_id, -quantifier, -trial_type, -condition, -response, -QUANT, -der, -SHAPE, -auf, -dem, -Bild, -sind, -CRIT_3, 
         -der_2, -Box, -trial_name, -choice_options_1)

d2 <- d2 %>% 
  filter(submission_id != 7950, submission_id != 7954) %>% 
  drop_na()

someBias1 <- d2 %>% 
  group_by(display) %>% 
  count(response_1) %>% 
  mutate(condProb = n / sum(n))

someBias1 <- rename(someBias1, "QUANT" = "response_1")

d2$choice_options_2 <- as.factor(d2$choice_options_2)

d2$choice_options_2 <- factor(plyr::revalue(d2$choice_options_2, c("Quadrate|Kreise" = "Quadrate|Kreise", "Quadrate|Kreise|Dreiecke" = "Quadrate|Kreise", "Quadrate|Dreiecke" = "Quadrate|Dreiecke", "Kreise|Dreiecke" = "Kreise|Dreiecke")), ordered = F)

someBias2_1_homo_1_hetero <- d2 %>%
  filter(display == "1-homo-1-hetero") %>% 
  group_by(response_1) %>%
  mutate(eval = case_when(property_1 == "mixed" & response_1 == "Einige" ~ shape_1,
                          property_2 == "mixed" & response_1 == "Einige" ~ shape_2,
                          property_1 != "mixed" & response_1 == "Alle" ~ shape_1,
                          property_2 != "mixed" & response_1 == "Alle" ~ shape_2)) %>%
  mutate(SHAPE = case_when(response_2 == eval ~ "expec",
                          response_2 != eval ~ "non-expec")) %>%
  count(SHAPE)

someBias2_2_hetero <- d2 %>%
  filter(display == "2-hetero") %>% 
  group_by(response_1) %>%
  mutate(SHAPE = case_when(response_1 == "Einige" ~ "expec",
                           response_1 != "Einige" ~ "non-expec")) %>%
  count(SHAPE)

someBias2_2_homo <- d2 %>%
  filter(display == "2-homo") %>% 
  group_by(response_1) %>%
  mutate(SHAPE = case_when(response_1 == "Alle" ~ "expec",
                           response_1 != "Alle" ~ "non-expec")) %>%
  count(SHAPE)

someBias2_unf <- bind_rows(someBias2_1_homo_1_hetero, someBias2_2_hetero, someBias2_2_homo, .id = "display")

someBias2_unf$display <- revalue(someBias2_unf$display, c("1" = "1-homo-1-hetero", "2" = "2-hetero", "3" = "2-homo"))
  
someBias2 <- someBias2_unf %>%
  group_by(display, response_1) %>%
  mutate(condProb = n / sum(n))

someBias2 <- rename(someBias2, "QUANT" = "response_1")

d2$choice_options_3 <- as.factor(d2$choice_options_3)

someBias3_1_homo_1_hetero <- d2 %>%
  filter(display == "1-homo-1-hetero") %>% 
  group_by(response_1) %>%
  mutate(eval_shape = case_when(property_1 == "mixed" & response_1 == "Einige" ~ shape_1,
                          property_2 == "mixed" & response_1 == "Einige" ~ shape_2,
                          property_1 != "mixed" & response_1 == "Alle" ~ shape_1,
                          property_2 != "mixed" & response_1 == "Alle" ~ shape_2)) %>%
  mutate(SHAPE = case_when(response_2 == eval_shape ~ "expec",
                           response_2 != eval_shape ~ "non-expec")) %>%
  mutate(eval_crit_3 = case_when(property_1 != "mixed" & response_1 == "Alle" ~ property_1,
                                 property_2 != "mixed" & response_1 == "Alle" ~ property_2,
                                 property_1 == "mixed" & response_1 == "Einige" ~ property_2,
                                 property_2 == "mixed" & response_1 == "Einige" ~ property_1)) %>%
  mutate(CRIT_3 = case_when(response_3 == eval_crit_3 & response_1 == "Alle" ~ "expec",
                            response_3 != eval_crit_3 & response_1 == "Alle" ~ "non-expec",
                            response_3 == eval_crit_3 & response_1 == "Einige" ~ "non-expec",
                            response_3 != eval_crit_3 & response_1 == "Einige" ~ "expec")) %>%
  group_by(response_1, SHAPE) %>%
  count(CRIT_3)

someBias3_2_hetero <- d2 %>%
  filter(display == "2-hetero") %>% 
  group_by(response_1) %>%
  mutate(SHAPE = case_when(response_1 == "Einige" ~ "expec",
                           response_1 != "Einige" ~ "non-expec")) %>%
  mutate(CRIT_3 = case_when(response_1 == "Alle" ~ "non-expec",
                            response_1 == "Einige" ~ "expec")) %>%
  group_by(response_1, SHAPE) %>%
  count(CRIT_3)

someBias3_2_homo <- d2 %>%
  filter(display == "2-homo") %>% 
  group_by(response_1) %>%
  mutate(SHAPE = case_when(response_1 == "Alle" ~ "expec",
                           response_1 != "Alle" ~ "non-expec")) %>%
  mutate(CRIT_3 = case_when(response_1 == "Einige" ~ "non-expec",
                            response_1 == "Alle" ~ "expec")) %>%
  group_by(response_1, SHAPE) %>%
  count(CRIT_3)

someBias3_unf <- bind_rows(someBias3_1_homo_1_hetero, someBias3_2_hetero, someBias3_2_homo, .id = "display")

someBias3_unf$display <- revalue(someBias3_unf$display, c("1" = "1-homo-1-hetero", "2" = "2-hetero", "3" = "2-homo"))

someBias3 <- someBias3_unf %>%
  group_by(display, response_1, SHAPE) %>% 
  mutate(condProb = n / sum(n))

someBias3 <- rename(someBias3, "QUANT" = "response_1")

#########################
##### Plot responses ####
#########################

ggplot() +
  geom_col(data = someBias3, aes(SHAPE, n, fill = CRIT_3), position = "dodge", color = "black") +
  facet_grid(display ~ QUANT, scales = "free") +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.background = element_rect(size = .5, linetype = "solid"),
        plot.title = element_text(hjust = 0.5), panel.grid.major.x = element_blank(),
        strip.text.y = element_blank(), legend.position = "bottom", 
        plot.margin = margin(t = 5.5, r = 88, b = 5.5, l = 5.5, "pt"),
        strip.text.x = element_text(angle = 360, size = 12, face = "bold", margin = margin(t = 0, r = 0, b = 15, l = 0)),
        panel.spacing.y = unit(1.5, "lines")) +
  scale_fill_brewer(palette = "Reds", guide = guide_legend(title = "Property"),
                    breaks = c("expec", "non-expec"),
                    labels = c("Congruent", "Incongruent")) +
  scale_x_discrete(breaks = c("expec", "non-expec"),
                   labels = c("Congruent", "Incongruent"))


###############################################################
##### The section below is where the RT data was wrangled  ####
##### and plotted alongside the production data            ####
##### The wrangling of the RTs up until the                ####
##### computation of the relevant group means per          ####
##### sentence region is left here for convenience         ####
###############################################################

d <- d %>% 
  filter(submission_id != 7950, submission_id != 7954)

d$trial_type <- as.factor(d$trial_type)

dRT <- d %>%
  filter(is.na(trial_type)) %>%
  filter(trial_name != "practice_task_three") %>% 
  select(-choice_options_1, -choice_options_2, -choice_options_3, -trial_type, -property_1, -property_2, -shape_1, -shape_2, -response_1, -response_2, -response_3, -display, -experiment_id) %>% 
  gather(Region, RT, -submission_id, -quantifier, -picture_type, -condition, -response, -listNumber,
         -trial_number, -picture, -trial_name)


dRT$Region = factor(x = dRT$Region,
                   levels = c("QUANT", "der", "SHAPE", "auf",
                              "dem", "Bild", "sind", "CRIT_3", "der_1",
                              "Box"))

dRT$submission_id <- as.factor(dRT$submission_id)
dRT$quantifier <- as.factor(dRT$quantifier)
dRT$condition <- as.factor(dRT$condition)

dRT <- dRT %>% group_by(Region, quantifier, condition) %>%
  mutate(outlier_trial = RT < (mean(RT) - 2.5*sd(RT)) | RT > (mean(RT) + 2.5*sd(RT)) ) %>%
  ungroup() %>% 
  group_by(submission_id) %>% 
  mutate(outlier_participant = mean(outlier_trial, na.rm = TRUE) > 0.3) %>% 
  ungroup() 

show(paste0("Excluded trials: ", sum(dRT$outlier_trial, na.rm = TRUE) ))
show(paste0("Excluded participants: ", sum(dRT$outlier_participant) ))

dRT <- dRT %>% filter(outlier_trial == FALSE) %>% 
  select(-outlier_trial, -outlier_participant)

dRT_logic <- dRT %>% 
  mutate(logCon = case_when(quantifier == "some" & condition == "unbiased" ~ "Einige (Unbiased)",
                            quantifier == "some" & condition == "biased" ~ "Einige (Biased)",
                            quantifier == "some" & condition == "false" ~ "Einige (False)",
                            quantifier == "some" & condition == "underspecified" ~ "Einige (Infelict)",
                            quantifier == "all" & condition == "unbiased" ~ "Alle (Unbiased)",
                            quantifier == "all" & condition == "biased" ~ "Alle (Biased)",
                            quantifier == "all" & condition == "false" ~ "Alle (False)",
                            TRUE ~ "other")) %>% 
  select(-quantifier, -condition)

dRT_logic$Region <-  factor(x = dRT_logic$Region, 
                          levels = c("QUANT", "der", "SHAPE", "auf", "dem", "Bild", 
                                     "sind", "CRIT_3", "der_1", "Box"))

# Reorder logical conditions
dRT_logic$logCon  <- factor(x = dRT_logic$logCon, levels = c("Einige (Biased)", "Einige (Unbiased)", "Einige (Infelict)", "Alle (Biased)", "Alle (Unbiased)", "Einige (False)", "Alle (False)"))

dRT_logic$response <- as.factor(dRT_logic$response)

dRT_logic2 <-  dRT_logic %>%
  filter(((response == "7" | response == "6" | response == "5") & (logCon == "Einige (Unbiased)" | logCon == "Einige (Biased)" | logCon == "Alle (Unbiased)" | logCon == "Alle (Biased)")) | ((response == "1" | response == "2" | response == "3") & (logCon == "Einige (False)" | logCon == "Alle (False)")) | logCon == "Einige (Infelict)") %>% 
  filter(Region %in% c("QUANT", "der", "SHAPE", "auf", "dem", "Bild",
                       "sind", "CRIT_3")) %>%
  group_by(Region, logCon) %>%
  summarise(Mean = mean(RT), CILow = ci.low(RT), CIHigh = ci.high(RT)) %>%
  mutate(YMin = Mean-CILow, YMax = Mean+CIHigh)