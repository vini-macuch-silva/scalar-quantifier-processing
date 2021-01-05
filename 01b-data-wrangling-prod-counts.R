library(plyr)
library(tidyverse)
library(bootstrap)
library(png)
library(magick)
library(ggrepel)
library(ggpubr)

set.seed(1969)

# read data
d <- read_csv("results-25-11-fixed.csv", locale = locale(encoding = 'ISO-8859-1'))

d1 <- d %>% 
  filter(is.na(trial_type))

d2 <- d %>%
  filter(trial_type == "dropdown_sentence_completion") %>% 
  # select relevant columns
  select(submission_id, trial_number, display, picture_type, 
         picture, listNumber, property_1, property_2, shape_1, shape_2,
         choice_options_1, choice_options_2, choice_options_3,
         response_1, response_2, response_3) %>% 
  # remove lines with NAs in some fields (why are they here?) 
  filter(! is.na(property_1)) %>% 
  # remove subjects that Vini excluded (I don't know why, though)
  filter(submission_id != 7950, submission_id != 7954)

# more concise / more informative category labels
data_prod <- d2 %>% 
  mutate(
    # type of property contrast / scene
    picture_type = case_when(
      picture_type == "A" ~ "size",
      picture_type == "B" ~ "position",
      picture_type == "C" ~ "color"
    ),
    # logical condition
    display = case_when(
      display == "1-homo-1-hetero" ~ "hom-het",
      display == "2-homo" ~ "hom-hom",
      display == "2-hetero" ~ "het-het"
    )
  ) %>% 
  rename(condition = display)

# focus on the 'hom-het' condition
data_prod_homhet <- data_prod %>% 
  filter(condition == "hom-het") %>% 
  mutate(
    response_category_1 = ifelse(response_1 == "Alle", "all", "some"),
    response_category_2 = ifelse(response_2 == ifelse(property_1 == "mixed", shape_1, shape_2), "het", "hom"),
    response_category_3 = ifelse(response_3 == ifelse(property_1 == "mixed", property_2, property_1), "hom", "het"),
    response_category_ex_1 = response_category_1,
    response_category_ex_2 = ifelse(response_category_2 == "hom", "triangles", "circles"),
    response_category_ex_3 = ifelse(response_category_3 == "hom", "black", "white"),
    truth_value = case_when(
      response_category_ex_1 == "all" & 
        response_category_ex_2 == "triangles" & 
        response_category_ex_3 == "black"  ~ 'true',
      response_category_ex_1 == "some" & 
        response_category_ex_2 == "circles" ~ 'true',
      response_category_ex_1 == "some" & 
        response_category_ex_2 == "triangles" & 
        response_category_ex_3 == "black"  ~ 'true (underinformative)',
      TRUE ~ 'false'
    )
  )


# focus on the 'het-het' condition
data_prod_hethet <- data_prod %>% 
  filter(condition == "het-het") %>% 
  mutate(
    first_shape    = map_chr(str_split(choice_options_2, "\\|"), function(i) i[1]),
    first_property = map_chr(str_split(choice_options_3, "\\|"), function(i) i[1]),
    response_category_1 = ifelse(response_1 == "Alle", "all", "some"),
    response_category_2 = ifelse(response_2 == first_shape, "first", "second"),
    response_category_3 = ifelse(response_3 == first_property, "first", "second"),
    response_category_ex_1 = response_category_1,
    response_category_ex_2 = ifelse(response_category_2 == "first", "triangles", "circles"),
    response_category_ex_3 = ifelse(response_category_3 == "first", "black", "white"),
    truth_value = case_when(
      response_category_1 == "some" ~ 'true',
      TRUE ~ 'false'
    )
  ) %>% View()
  select(-first_shape, -first_property)

  data_prod %>% 
    filter(condition == "het-het") %>% 
    mutate(
      t = map_chr(str_split(choice_options_2, "\\|"), function(i) i[1])
    ) %>% pull(t) %>% table()
  
  
# focus on the 'hom-hom' condition
data_prod_homhom <- data_prod %>% 
  filter(condition == "hom-hom") %>% 
  mutate(
    response_category_1 = ifelse(response_1 == "Alle", "all", "some"),
    response_category_2 = ifelse(response_2 == shape_1, "first", "second"),
    response_category_3 = ifelse(response_category_2 == "first", response_3 == property_1, response_3 == property_2),
    response_category_3 = ifelse(response_category_3, "match", "mismatch"),
    response_category_ex_1 = response_category_1,
    response_category_ex_2 = ifelse(response_category_2 == "first", "triangles", "circles"),
    response_category_ex_3 = case_when(
      response_category_ex_2 == "triangles" & response_category_3 == "match" ~ "black",
      response_category_ex_2 == "circles"   & response_category_3 == "match" ~ "white",
      response_category_ex_2 == "triangles" & response_category_3 == "mismatch" ~ "white",
      response_category_ex_2 == "circles"   & response_category_3 == "mismatch" ~ "black"
    ),
    truth_value = case_when(
      response_category_1 == "all" & response_category_3 == "match"  ~ 'true',
      response_category_1 == "some" & response_category_3 == "match"  ~ 'true (underinformative)',
      TRUE ~ 'false'
    )
  )

# bringing it back together

data_prod <- 
  rbind(
    data_prod_hethet,
    data_prod_homhom,
    data_prod_homhet
  )

# get counts

# focus on initial position

counts_initial <- data_prod %>% 
  count(condition, response_category_ex_1) %>% 
  rename(choice = response_category_ex_1) %>% 
  group_by(condition) %>% 
  mutate(
    position = 'initial',
    proportion = n / sum(n)
  ) %>% 
  ungroup() %>% 
  select(
    condition, position, choice, n, proportion
  )

# focus on quantifier position

counts_quantifier <- data_prod %>% 
  count(condition, response_category_ex_1, response_category_ex_2) %>% 
  rename(
    position = response_category_ex_1,
    choice = response_category_ex_2
  ) %>% 
  group_by(condition, position) %>% 
  mutate(
    proportion = n / sum(n)
  ) %>% 
  ungroup() %>% 
  select(
    condition, position, choice, n, proportion
  )

# focus on shape position

counts_shape <- data_prod %>% 
  mutate(
    position = str_c(response_category_ex_1, " ", response_category_ex_2)
  ) %>% 
  count(condition, position, response_category_ex_3) %>% 
  rename(
    choice = response_category_ex_3
  ) %>% 
  group_by(condition, position) %>% 
  mutate(
    proportion = n / sum(n)
  ) %>% 
  ungroup() %>% 
  select(
    condition, position, choice, n, proportion
  )

# complete "sentence"

counts_sentence <- data_prod %>% 
  mutate(
    position = str_c(response_category_ex_1, " of the ", response_category_ex_2, " are ", response_category_ex_3)
  ) %>% 
  count(condition, position, truth_value) %>% 
  group_by(condition, position, truth_value) %>% 
  mutate(
    choice = "---",
    proportion = "---"
  ) %>% 
  ungroup() %>% 
  select(
    condition, position, choice, n, proportion, truth_value
  )
  

counts <- rbind(
  counts_initial %>% mutate(truth_value = "---"),
  counts_quantifier %>% mutate(truth_value = "---"),
  counts_shape %>% mutate(truth_value = "---"),
  counts_sentence
) %>% 
  arrange(condition)
