# Set up ----
rm(list=ls()) # clear workspace
library(here) # for convenient file paths
library(tidyverse) # load package
library(tidylog)
library(googlesheets4)
library(dplyr)
library(psych)
here::i_am("analysis/02_check_stumpers_reliability.R") # sets here() to main project folder

# Load included & coded trials ----
coder1 <- read_csv(here("data/annotations/garden2a_stumpers_coder1.csv")) %>%
  rename(coder1_category = response_category, coder1_comments = coder_comments)
# check expected labels
table(coder1$coder1_category)
sum(is.na(coder1$coder1_category))

coder2 <- read_csv(here("data/annotations/garden2a_stumpers_coder2.csv")) %>%
  rename(coder2_category = response_category, coder2_comments = coder_comments)
# check expected labels
table(coder2$coder2_category)
sum(is.na(coder2$coder2_category))

# Merge coders
data <- coder2 %>%
  left_join(coder1 %>% select(response_uuid, question_id, coder1_category, coder1_comments)) %>%
  mutate(coder1_accuracy = coder1_category=="target_solution",
         coder2_accuracy = coder2_category=="target_solution")

# 2-category agreement ----
accuracy <- data %>% select(coder1_accuracy, coder2_accuracy)

# Kappa
cohen.kappa(as.matrix(accuracy))

# Percent agreement
data %>%
  mutate(match = coder1_accuracy == coder2_accuracy) %>%
  count(match) %>% mutate(p = n/sum(n))

# 4-category ----
ratings <- data %>% select(coder1_category, coder2_category)

#calculate Cohen's Kappa
(kappa <- cohen.kappa(as.matrix(ratings)))

# Print disagreement table
kappa$agree

# Export disagreements ----
disagreements <- data %>% 
  filter(coder1_category != coder2_category)

disagreements %>% select(-coder_id) %>%
  write_csv(here("data", 
  "annotations", paste("garden2a_stumpers_disagreements_", today(), ".csv", sep="")))

# Import final decisions and merge ----

resolved <- read_csv(here("data/annotations/garden2a_stumpers_disagreements_resolved.csv")) %>%
  rename(response_category = janie_category) %>%
  select(response_uuid, question_id, response_category) %>%
  mutate(coder_id = "resolved")

keep <- anti_join(coder1, resolved, by=c('response_uuid', 'question_id')) %>%
  select(-coder1_comments) %>%
  rename(response_category = coder1_category)

change = semi_join(coder1, resolved, by=c('response_uuid', 'question_id')) %>%
  select(-coder1_category, -coder1_comments, -coder_id) %>%
  left_join(resolved, by=c("response_uuid", "question_id"))

final <- rbind(keep, change)

## verify and export ----

# response categories
table(final$response_category)

# number of sessions
length(table(final$response_uuid))

# coder ID
table(final$coder_id)

# items
table(final$question_set)
table(final$question_id)

write_csv(final, here("data/annotations/garden2a_stumpers_final_coded.csv"))
