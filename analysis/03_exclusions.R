# Set up ----
rm(list=ls()) # clear workspace
library(here) # for convenient file paths
library(tidyverse)
library(tidylog)
here::i_am("analysis/03_exclusions.R") # sets here() to main project folder

# All sessions ====
all_sessions <- read_csv(here("data/tidy_identifiable/garden2a_sessions_identifiable.csv"))

# Sessions recorded vs usable
sprintf('Total sessions recorded: %i', nrow(all_sessions))
# all_sessions %>% count(stumper_usable, sort=T)
all_sessions %>% count(balldrop_usable, sort=T)

# Stumpers ====

## Load data sheets, check for missing data ----
# read in trial metadata for usable sessions
s.meta <- read_csv(here("data/tidy_anonymized/garden2a_stumpers_trial-metadata.csv")) %>%
  mutate(trial_number = trial_num + 1) # 1-indexed
s.meta %>% count(response_uuid) %>% 
  count(n) %>%
  rename(n_trials_logged = n, n_subjects = nn)

# read in transcriptions; these may possibly include un-usable trials & sessions
s.transcript <- read_csv(here("data/annotations/garden2a_stumpers_transcriptions.csv"))
# preview trial exclusion reasons
s.transcript %>% count(trial_exclusion_reason, sort=T)
# preview top transcriber comments
s.transcript %>% count(transcriber_comments, sort=T) %>% head()

# read in response coding; these may possibly include un-usable trials & sessions
s.coding <- read_csv(here("data/annotations/garden2a_stumpers_final_coded.csv"))
# preview number of coded trials available
s.coding %>% count(response_uuid) %>%
  count(n) %>%
  rename(n_trials_coded = n, n_subjects = nn)

## Tabulate exclusions ----

# First, exclude sessions that do not meet usable criteria
all_sessions %>% count(stumper_usable, sort=T)
participant_exclusions <- all_sessions %>%
  filter(stumper_usable != "yes")
sprintf('Total Stumper sessions excluded because not usable: %i', nrow(participant_exclusions))

# Next, inspect usable sessions
usable_sessions_trials <- s.transcript %>%
  filter(!response_uuid %in% participant_exclusions$response__uuid)
# How many trials available per child?
usable_sessions_trials %>% count(response_uuid) %>% count(n)

# Do any of the usable sessions have fewer than 6 valid trials?
participant_trial_counts <- usable_sessions_trials %>%
  group_by(response_uuid) %>%
  summarize(
    total_trials = n(),
    valid_trials = sum(trial_exclusion_reason == "include", na.rm = TRUE)
  )
participant_trial_counts %>% count(valid_trials)

# Why?
# s.transcript %>%
#   # First filter to just participants with < 6 valid trials
#   filter(response_uuid %in% participant_trial_counts$response_uuid,
#          trial_exclusion_reason != "include") %>%
#   # Count occurrences of each exclusion reason
#   group_by(response_uuid, trial_exclusion_reason) %>%
#   summarize(count = n()) %>%
#   arrange(desc(count))

# See the specific reasons for excluded trials for these participants
# s.transcript %>%
#   filter(response_uuid %in% participant_trial_counts$response_uuid) %>%
#   group_by(response_uuid, trial_exclusion_reason) %>%
#   summarize(count = n(), .groups = 'drop')

## export trial-level dataset with all exclusion decisions ----
stumpers <- all_sessions %>% select(response_uuid = response__uuid,
                                    response_video_privacy = response__video_privacy,
                                    response_databrary = response__databrary,
                                    gender = child__gender,
                                    languages = child__language_list) %>%
  left_join(s.meta) %>% 
  left_join(usable_sessions_trials %>% 
              select(response_uuid, question_id, transcriber_id, 
                     response_transcript, feedback_transcript, transcriber_comments, trial_exclusion_reason, 
                     parentCode_judgedBy = `who_judged_response_similarity_during_feedback?`)) %>%
  left_join(s.coding %>%
              select(response_uuid, question_id, coder_id,
                     response_category)) %>%
  unique()

# export 
head(stumpers)
write_csv(stumpers, here("data/tidy_anonymized/garden2a_stumpers_trial-responses.csv"))

## Participant count ----
# how many valid trials per subject? 
s.included.summary <- s.transcript %>% 
  filter(trial_exclusion_reason == "include") %>%
  count(response_uuid)
s.included.summary %>%
  count(n) %>% 
  rename(n_included_trials = n, n_subjects = nn)

sprintf("Stumpers kids with at least 6 transcribed trials: %i",
        filter(s.included.summary, n>=6) %>% nrow()
)


## export all included trials and kids ----

# 1. first get metadata for ALL sessions
all_metadata <- all_sessions %>% 
 select(response_uuid = response__uuid,
        child_hashed_id = child__hashed_id,
        child_gender = child__gender,
        age_months, child__language_list)

# then use this in the main pipeline
included_responses <- s.transcript %>% 
  filter(trial_exclusion_reason == "include",
         response_uuid %in% usable_sessions_trials$response_uuid) %>% 
  count(response_uuid)  %>%
  filter(n >= 6) %>%
  pull(response_uuid)

str(included_responses)

included_stumper_trials <- s.transcript %>%
 filter(trial_exclusion_reason == "include",
        response_uuid %in% included_responses) %>%
  rename(trial_num = trial_number) %>%
 left_join(all_metadata,
           select(child_language_list),
           by = "response_uuid") %>%
 left_join(s.coding %>% 
             select(response_uuid, question_id, response_category),
           by = c("response_uuid", "question_id")) %>%
 left_join(s.meta %>%
             select(response_uuid, question_id, parentCode),
           by = c("response_uuid", "question_id"))

str(included_stumper_trials)

# Any trials not coded?
included_stumper_trials %>% filter(is.na(response_category)) %>% 
  count(response_uuid)

included_stumper_trials %>% 
  filter(!is.na(response_category)) %>% 
  write_csv(here("results", "csv/stumpers_trial-responses-included.csv"))


# included ss demographics ----
# language demographics
# Count monolingual vs multilingual
(language_stats <- included_stumper_trials %>%
  mutate(num_languages = str_count(child__language_list, "\\s+") + 1) %>%
  group_by(response_uuid) %>%
  slice(1) %>%  # Take one row per participant
  ungroup() %>%
  summarize(
    monolingual = sum(num_languages == 1, na.rm=TRUE),
    multilingual = sum(num_languages > 1, na.rm=TRUE),
    total = n()
  ) %>%
  mutate(
    mono_prop = round(monolingual/total * 100, 1),
    multi_prop = round(multilingual/total * 100, 1)
  ))

# Most common additional languages
(language_counts <- included_stumper_trials %>%
  select(response_uuid, child__language_list) %>%
  distinct() %>%  # Take one row per participant
  mutate(languages = child__language_list) %>%
  separate_rows(languages, sep = "\\s+") %>%
  count(languages, sort = TRUE))

# gender demographics
included_stumper_trials %>% select(response_uuid, child_gender) %>%
  distinct() %>%
  count(child_gender)

# age demographics
included_stumper_trials %>% 
  select(child_hashed_id, age_months) %>% 
  distinct() %>%
  summarize(
    n = n(),
    m_age = round(mean(age_months, na.rm=T), 2),
    sd_age = round(sd(age_months, na.rm=T), 2),
    min_age = round(min(age_months, na.rm=T), 2),
    max_age = round(max(age_months, na.rm=T), 2))

# race/ethnicity
demographic <- read_csv(here("data/How-can-that-be---psychds/data/demographic/How-can-that-be-_all-demographic-snapshots.csv")) 

# For individual race categories
(race_totals <- demographic %>%
  filter(response__uuid %in% included_responses) %>%
  select(response__uuid, demographic__us_race_ethnicity_identification) %>%
  mutate(
    White = grepl("White", demographic__us_race_ethnicity_identification),
    Asian = grepl("Asian", demographic__us_race_ethnicity_identification),
    Hispanic = grepl("Hispanic", demographic__us_race_ethnicity_identification),
    Black = grepl("Black", demographic__us_race_ethnicity_identification),
    MiEa = grepl("Middle Eastern", demographic__us_race_ethnicity_identification),
    PaIs = grepl("Pacific Islander", demographic__us_race_ethnicity_identification),
  ) %>%
  summarize(
    White = round(mean(White, na.rm=TRUE) * 100, 1),
    Asian = round(mean(Asian, na.rm=TRUE) * 100, 1),
    Black = round(mean(Black, na.rm=TRUE) * 100, 1),
    Hispanic = round(mean(Hispanic, na.rm=TRUE) * 100, 1),
    MiEa = round(mean(MiEa, na.rm=TRUE) * 100, 1),
    PaIs = round(mean(PaIs, na.rm=TRUE) * 100, 1),
  ))



# Balldrop ====

## Initial ----
b.meta <- read_csv(here("data/tidy_anonymized/garden2a_balldrop_trial-metadata.csv"))
b.meta %>% count(response_uuid, trial_id) %>% 
  count(response_uuid) %>%
  count(n) %>%
  rename(n_trials = n, n_subjects = nn)

## Transcriptions ----
b.transcript <- read_csv(here("data/annotations/garden2a_balldrop_transcriptions.csv"))
b.transcript %>% count(response_uuid, trial_number) %>% 
  count(response_uuid) %>%
  count(n) %>%
  rename(n_trials = n, n_subjects = nn)

## combine everything ----
b.transcript.analyze <- b.transcript %>% 
  mutate(trial_id = paste(trial_type, trial_label, sep="")) %>%
  select(response_uuid, trial_id, 
         trial_exclusion_reason, response_transcript, 
         transcriber_id, transcriber_comments = coder_comments)
  
balldrop <- all_sessions %>% select(response_uuid = response__uuid,
                                    response_video_privacy = response__video_privacy,
                                    response_databrary = response__databrary,
                                    age_months,
                                    gender = child__gender) %>%
  left_join(b.meta %>% 
              select(response_uuid, trial_id, 
                     test_trial_number=trialnum, correct_ever, n_incorrect_attempts) %>%
              distinct()%>% 
              filter(!trial_id %in% c("tutorial1", "tutorial2"))) %>% 
  left_join(b.transcript.analyze)

### export all coded trials ----

head(balldrop)
write_csv(balldrop, here("data/tidy_anonymized/garden2a_balldrop_combined-data.csv"))

## Participant count ----
# all usable or uncoded sessions
balldrop %>% count(response_uuid) %>% 
  count(n) %>% 
  rename(n_trials = n, n_subjects = nn)

# trial inclusion decisions
b.transcript %>% count(trial_exclusion_reason)

# how many valid trials per subject? 
b.included.summary <- b.transcript %>% 
  filter(trial_exclusion_reason == "include") %>%
  count(response_uuid)
b.included.summary %>%
  count(n) %>% 
  rename(n_included_trials = n, n_subjects = nn)

# who had incomplete data? should we exclude?
incomplete_balldrop_uuid <- b.included.summary %>% 
  filter(n < 7) %>%
  pull(response_uuid)

(partial_balldrop <- b.transcript %>% 
  filter(response_uuid %in% incomplete_balldrop_uuid,
         trial_exclusion_reason == "include") %>%
  count(response_uuid, trial_type) %>%
  pivot_wider(names_from = trial_type, 
              values_from = n))

# ah, ok. we can include if 3 test trials and 1 each of infer and predict
partial_balldrop_uuid <- partial_balldrop %>%
  filter(infer >0, predict>0, test==3) %>%
  pull(response_uuid)




### export all included trials and kids ----
# all 7 trials
included_balldrop_uuid <- 
  balldrop %>% count(response_uuid) %>%
  filter(n == 7) %>%
  pull(response_uuid) %>%
  unique()  

included_balldrop_trials <- balldrop %>% 
  filter(trial_exclusion_reason == "include",
         response_uuid %in% 
           cbind(included_balldrop_uuid, partial_balldrop_uuid))

write_csv(included_balldrop_trials, here("results/csv/balldrop_data.csv"))