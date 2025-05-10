# This script prepares the CHS downloaded data for trial-level exclusion and response coding
# Expected outputs are two google sheets, each containing trial-level metadata (timing, prompt, etc.) for RAs to do exclusion coding & transcription from.
# (1) kid-stumpers
# This script will also export two anonymized csvs without timing information that will be used as inputs for analysis
# (1) data/tidy_anonymized/garden2a_stumpers_trial-metadata.csv
# (2) data/annotations/garden2a_stumpers_transcriptions.csv
# (3) data/annotations/garden2a_stumpers_coder1.csv
# (3) data/annotations/garden2a_stumpers_coder2.csv
# (4) data/tidy_anonymized/garden2a_balldrop_trial-metadata.csv
# (4) data/annotations/garden2a_balldrop_transcriptions.csv

# It expects as input: 
# (1) a folder with one framedata csv per session, ID = responseUUID, childHASHID, participantHASHID
# filenames follow the convention: "[study-name]_[responseUUID]_frames.csv"
# (2) a csv of demographic data. NOTE this contains identifiable UUIDs in addition to hashed childIDS. 
# this filename is [study-name]_all-responses-identifiable.csv
# (3) a google sheet with RA annotations for each session.

# SETUP ----
rm(list=ls()) # clear workspace
library(tidyverse) # load package
library(lubridate) # for handling times
library(here) # for convenient file paths
library(googlesheets4)
library(dplyr)
library(glue)
library(purrr)
library(stringr)
here::i_am("analysis/01_preprocess.R") # sets here() to main project folder
USER_EMAIL <- "junyichu10@gmail.com" # your email for google sheets
gs4_auth(
  scopes = c("https://www.googleapis.com/auth/spreadsheets",
             "https://www.googleapis.com/auth/drive"),
  email = USER_EMAIL
)

## Parameters ----
make_reliability_coding = FALSE # set to TRUE to re-generate reliability coding sheet

## Filepaths ----
# where input files come from
# When downloading, check all boxes except parent/child names
PATH_INPUT_DATA <- here('data/How-can-that-be---psychds/data')
PATH_FRAMEDATA_FOLDER <- here(PATH_INPUT_DATA, 'framedata-per-response')
PATH_RESPONSE_SUMMARY <- here(PATH_INPUT_DATA, 'overview/study-2eddfdb3_all-responses_identifiable-true_data.csv')
# RA annotation spreadsheet
annotations_url <- 'https://docs.google.com/spreadsheets/d/1oUuu7pdz-2fWymzzO30nQ0PCzLoVcrcMyKYZz-AX0qM/edit?gid=1942452427#gid=1942452427'


# where output files go
PATH_OUTPUT_DATA <- here('data')
sheet_url_balldrop <- "https://docs.google.com/spreadsheets/d/10X2iicpqWG0NZmgToqs7PJYGohOCeYxmKQ3dThIC6EE/edit?gid=735246224#gid=735246224"
sheet_url_stumper <- "https://docs.google.com/spreadsheets/d/1_GZk2-MgkrNgomXO0vFK6dcX5vkOzxDR_B-HramLhkU/edit#gid=0"


# Session data ----
df.response_summary <- read_csv(here(PATH_RESPONSE_SUMMARY), show_col_types = FALSE) %>%
  select(response__uuid:child__additional_information) %>%
  select(-`consent__time...16`) %>%
  rename(consent_time = `consent__time...14`) %>%
  # compute age in months
  mutate(age_months = interval(ymd(child__birthday), ymd(date(response__date_created))) %/% months(1))

## import RA-coded usability ----
df.participant_tracker <- read_sheet(
  annotations_url,
  sheet = "1. Participants",
  skip=2, # skip description rows
  trim_ws=TRUE,
  col_names=TRUE,
  na=c("NA", "N/A", "n/a", "")
) %>%
  # Clean up column names but keep all columns
  rename_with(~ tolower(gsub("\\s+", "_", .x))) %>%
  mutate(across(c(ends_with("uuid"), ends_with("_id"), 
                  "ok_to_pay?",
                  "transcriber_id",
                  "balldrop_usable", "stumper_usable", "transcribed", "transcription_checked"),as.factor))

usable.sessions <- df.participant_tracker %>%
  filter(`ok_to_pay?`=="paid" | `ok_to_pay?`=="TO PAY", 
         (stumper_usable=="yes" | is.na(stumper_usable) | balldrop_usable =="yes" | is.na(balldrop_usable))
  ) %>%
  pull(response__uuid)

## export session data ----
df.response_summary %>%
  left_join(df.participant_tracker %>% select(
    response__uuid, balldrop_usable, stumper_usable, usable_comment)) %>%
  write_csv(here(PATH_OUTPUT_DATA, 
                 'tidy_identifiable/garden2a_sessions_identifiable.csv'))


### summary ----
# Paid sessions
df.participant_tracker %>% count(`ok_to_pay?`)
# Usable sessions
df.participant_tracker %>% count(balldrop_usable, stumper_usable) %>%
  arrange(-n)
# Ages of usable sessions
df.participant_tracker %>%
  filter(stumper_usable=="yes" | is.na(stumper_usable) | balldrop_usable =="yes" | is.na(balldrop_usable)) %>%
  summarise(n(),
            across(.cols=c(child_age),
                   .fns=list(~ mean(.x, na.rm = TRUE),
                          ~ sd(.x, na.rm = TRUE),
                          ~ min(.x, na.rm = TRUE),
                          ~ max(.x, na.rm = TRUE)),
                   .names="{.col}_{.fn}"))

# Read in frame csvs ====
framedata_filenames <- list.files(PATH_FRAMEDATA_FOLDER, pattern="*.csv", full.names=F)

read_frame_data <- function(filename, directory) {
  read_csv(here(directory, filename), show_col_types = FALSE) # quiet display messages
  # add in other pre-processing steps here if desired
}

df.frames.all <- lapply(framedata_filenames, read_frame_data, directory=PATH_FRAMEDATA_FOLDER) %>%
  reduce(bind_rows) %>%
  filter(response_uuid %in% usable.sessions) # exclude the same duplicates and scammers


# BALL DROP GAME ----

## assign trial numbers ----
# Figure out trial order of testA, testB, testC
# first check what the frame_ids are called
# df.frames.all %>% filter(str_ends(frame_id, "procedure")) %>% count(frame_id)

# Ok, all follow a consistent pattern: 50-balldrop-testX-procedure, 52.., 54..
df.trialnumbers <- df.frames.all %>%
  filter(str_ends(frame_id, "procedure"), key=="sessionStreamTime") %>%
  select(child_hashed_id, frame_id, sessionStreamTime=value) %>%
  separate(frame_id, into=c('framenum', NA, 'trial_id', NA), sep='-') %>%
  mutate(trialnum = (as.numeric(framenum) - 48)/2) %>%
  select(-framenum) %>% unique()

# First find all trial attempts and reshape into a wide format
df.balldrop.choices <- df.frames.all %>% 
  filter(is.na(event_number), # ignore timing information
         str_detect(frame_id, 'attempt') | str_detect(frame_id, 'puzzle')) %>% 
  select(-event_number) %>%
  separate_wider_delim(cols=frame_id, delim='drop-', 
                       names=c(NA, 'question_id'),
                       too_many = 'merge') %>% 
  separate_wider_delim(cols=question_id, delim="-",
                       names=c('trial_id', 'attempt_id', 'repeat_id'),
                       too_many = 'merge',
                       too_few = 'align_start') %>%
  filter(key %in% c('frameDuration', 'selectedImage', 'correctImageSelected')) %>%
  # test trials have attempt_id = attemptN where N=1,2,or 3
  # other trials have only one attempt_id='puzzle'
  mutate(attempt_id = as.integer(str_sub(attempt_id, -1, -1)),
         repeat_id = as.integer(str_sub(repeat_id, -1,-1))
         ) %>%
  pivot_wider(names_from = 'key', values_from = 'value') %>%
  # Recode accuracy scores
  select(-correctImageSelected) %>%
  mutate(accuracy = case_when(is.na(selectedImage) ~ NA,
                                          trial_id=="testA" & selectedImage =="1" ~ 1,
                                          trial_id=="testB" & selectedImage =="2" ~ 1,
                                          trial_id=="testC" & selectedImage =="5" ~ 1,
                                          trial_id=="inferA" & selectedImage == "chute2" ~ 1,
                                          trial_id=="inferB" & selectedImage == "chute4" ~ 1,
                                          trial_id=="predictA" & selectedImage == "cup2" ~ 1,
                                          trial_id=="predictB" & selectedImage == "cup3" ~ 1,
                                          .default=0)) %>%
  # score if ever correct
  group_by(response_uuid, trial_id) %>% 
  mutate(correct_ever = max(accuracy, na.rm=T),
         n_incorrect_attempts = sum(accuracy==0, na.rm=T)) %>% 
  ungroup() %>%
  left_join(df.trialnumbers, by=c('child_hashed_id', 'trial_id'))

# check attempt counting
table(df.balldrop.choices$trial_id, df.balldrop.choices$n_incorrect_attempts)

# check accuracy coding
df.balldrop.choices %>% count(trial_id,  accuracy, selectedImage)

## export physics trial metadata ====
df.balldrop.choices %>% left_join(
  select(df.response_summary, response_uuid= response__uuid, age_months)) %>%
  write_csv(here(PATH_OUTPUT_DATA, 'tidy_anonymized/garden2a_balldrop_trial-metadata.csv'))

## generate balldrop transcription sheet----
 
ms_to_mmss <- function(ms) {
  # converts milliseconds to 'MM:SS' format
  # Ensure 'ms' is numeric
  ms <- as.numeric(ms)
  
  # initialize result vector with NA_character_
  result <- rep(NA_character_, length(ms))
  
  # identify indices where 'ms' is not NA
  valid_indices <- which(!is.na(ms))
  
  # process only non-NA values
  if (length(valid_indices) > 0) {
    ms_valid <- ms[valid_indices]
    
    # calculate minutes and seconds
    minutes <- floor(ms_valid / 60000)
    seconds <- floor((ms_valid %% 60000) / 1000)
    
    # format into 'MM:SS'
    result[valid_indices] <- sprintf("%02d:%02d", minutes, seconds)
  }
  
  return(result)
}


# first, create a mapping of trial_id to trial number (from 00_prepare_for_coding)
trial_mapping <- df.balldrop.choices %>%
  select(response_uuid, trial_id, trialnum) %>%
  distinct() %>%
  mutate(
    trial_type = str_extract(trial_id, "^[^A-C]+"),
    trial_letter = str_extract(trial_id, "[ABC]$")
  )

# now the main code with attempt timing
df.timing.balldrop <- df.frames.all %>%
  filter(str_detect(frame_id, "balldrop")) %>% 
  filter(!str_detect(frame_id, "tutorial")) %>%
  mutate(
    trial_info = case_when(
      str_detect(frame_id, "balldrop-test[ABC]") ~ str_extract(frame_id, "test[ABC]"),
      str_detect(frame_id, "balldrop-predict[AB]") ~ str_extract(frame_id, "predict[AB]"),
      str_detect(frame_id, "balldrop-infer[AB]") ~ str_extract(frame_id, "infer[AB]"),
      TRUE ~ NA_character_
    ),
    # extract attempt number if present
    attempt_num = as.numeric(str_extract(frame_id, "(?<=attempt)\\d+"))
  ) %>%
  filter(!is.na(trial_info)) %>%
  mutate(
    trial_type = str_extract(trial_info, "^[^A-C]+"),
    trial_letter = str_extract(trial_info, "[ABC]$")
  ) %>%
  group_by(response_uuid, trial_type, trial_letter) %>%
  reframe(
    # video duration from min/max session stream time
    video_duration = {
      times <- as.numeric(value[key == "sessionStreamTime"])
      if(length(times) >= 2) max(times, na.rm = TRUE) - min(times, na.rm = TRUE) else 0
    },
    transcriber_id = NA_character_,
    trial_label = trial_letter,
    # overall trial start/stop times
    trial_start_time = {
      times <- as.numeric(value[key == "sessionStreamTime"])
      if(length(times) > 0) ms_to_mmss(min(times, na.rm = TRUE)) else "N/A"
    },
    trial_stop_time = {
      times <- as.numeric(value[key == "sessionStreamTime"])
      if(length(times) > 0) ms_to_mmss(max(times, na.rm = TRUE)) else "N/A"
    },
    # get attempt start times in simplified format
    attempt_starts = {
      attempts <- unique(attempt_num[!is.na(attempt_num)])
      attempt_times <- sapply(attempts, function(a) {
        ms_to_mmss(min(as.numeric(value[!is.na(attempt_num) & attempt_num == a & 
                                          key == "sessionStreamTime"]), na.rm = TRUE))
      })
      paste(attempt_times, collapse = ", ")
    },
    # count attempts
    trial_attempts = case_when(
      trial_type == "test" ~ n_distinct(str_extract(frame_id[str_detect(frame_id, "-attempt\\d+")], "attempt\\d+")),
      TRUE ~ 1
    ),
    response_transcript = NA_character_,
    trial_exclusion_reason = NA_character_
  ) %>%
  mutate(
    video_duration = ms_to_mmss(video_duration)
  ) %>%
  distinct() %>%
  left_join(trial_mapping, by=c("response_uuid", "trial_type", "trial_letter")) %>%
  select(response_uuid, video_duration, transcriber_id, 
         trial_number = trialnum, trial_type, trial_label,
         trial_start_time, trial_stop_time, attempt_starts, trial_attempts,
         response_transcript, trial_exclusion_reason) %>%
  arrange(response_uuid, trial_start_time)


### send to googlesheets ----

# get the existing data to find column positions
existing_data <- read_sheet(sheet_url_balldrop, sheet = "02_transcriptions")

# find the column positions for each of our variables
col_positions <- match(c("response_uuid", "video_duration", "transcriber_id", 
                         "trial_number", "trial_type", "trial_label", 
                         "trial_start_time", "trial_stop_time", "attempt_starts",
                         "trial_attempts", "response_transcript",
                         "trial_exclusion_reason"), names(existing_data))

# while we're at it, lets add trial no. labels for predict and infer
df.timing.balldrop <- df.timing.balldrop %>%
  group_by(response_uuid) %>%
  mutate(trial_number = row_number()) %>%
  ungroup()

# Write to Google Sheets
range_write(df.timing.balldrop,
            ss = sheet_url_balldrop,
            sheet = 1,
            range = cell_rows(c(3, NA)),
            col_names = FALSE,
            reformat = FALSE)


## Import transcription / exclusion coding ----
df.balldrop.transcripts <- read_sheet(
  annotations_url,
  sheet = "5. Balldrop trial exclusions",
  skip=1, # skip description row
  trim_ws=TRUE,
  col_names=TRUE,
  na=c("NA", "N/A", "n/a", "")
) %>%
  # Clean up column names but keep all columns
  rename_with(~ tolower(gsub("\\s+", "_", .x)))

head(df.balldrop.transcripts)
df.balldrop.transcripts %>% 
  write_csv(here(PATH_OUTPUT_DATA, 
            "annotations/garden2a_balldrop_transcriptions.csv"))

# STUMPERS ----

## Extract trial metadata from frames----
df.stumper.trials <- df.frames.all %>%
  filter(str_detect(frame_id, "trial")) %>%
  separate_wider_delim(cols=frame_id, delim='-',
                       names=c('frame_num', NA, 'trial_num', 'page'),
                       too_many = 'merge') %>%
  mutate(variable = case_when(
    page == 'question' & key == 'audioPlayed' ~ 'audioPlayed',
    page == 'question' & key == 'frameDuration' ~ 'questionAndResponseDuration',
    page == 'feedback' & key == 'selectedImage' ~ 'parentCode')) %>%
  filter(!is.na(variable)) %>%
  select(-frame_num, -page, -event_number, -key) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(question_id = str_sub(str_split_i(audioPlayed, "wav/", 2), 1, -5)) %>%
  select(-audioPlayed) %>%
  separate(question_id, into=c('question_set', 'question_id', NA), sep='_')

## export stumpers trial metadata ----
df.stumper.trials %>% 
  left_join(select(df.response_summary, 
                   response_uuid= response__uuid, 
                   age_months)) %>%
  write_csv(here(PATH_OUTPUT_DATA, 'tidy_anonymized/garden2a_stumpers_trial-metadata.csv'))


## generate stumpers transcription sheet for RAs ----

### Extract timing data ----
question_start_events <- df.frames.all %>%
  filter(str_detect(frame_id, "trial")) %>%
  filter(str_detect(frame_id, "trial")) %>%
  separate_wider_delim(cols=frame_id, delim='-',
                       names=c('frame_num', NA, 'trial_num', 'page'),
                       too_many = 'merge') %>%
  filter(str_detect(page, "question") & str_detect(value, "replayAudio") | str_detect(value, "startAudio"))

df.timing.stumper <- df.frames.all %>%
  filter(str_detect(frame_id, "trial")) %>% # stumpers only
  separate_wider_delim(cols=frame_id, delim='-',
                       names=c('frame_num', NA, 'trial_num', 'page'),
                       too_many = 'merge') %>%
  separate_wider_delim(cols=page, delim='-', # separate out restart  number
                       names=c('page', NA, 'frame_restart_num'),
                       too_few = 'align_start') %>%
  mutate(frame_restart_num = ifelse(is.na(frame_restart_num), 1, as.integer(frame_restart_num)+1)) %>%
  filter(!is.na(event_number)) %>% # only keep timing events
  pivot_wider(names_from = key, values_from = value)

# Get and clean up coding-relevant timing data
df.timing.cleaned <- df.timing.stumper %>%
  mutate(event_name = case_when( # we care about 7 timestamps per trial
    str_detect(page, 'intro') & eventType=="exp-lookit-video:videoStarted"~ 'time_trial_start', # trials begin when intro video starts
    str_detect(page, 'question') & eventType=="exp-lookit-images-audio:startAudio"~ 'time_question_start', # when question starts playing. Multiple instances, if replayed
    str_detect(page, 'question') & eventType=="exp-lookit-images-audio:finishAudio"~ 'time_question_stop', # when question ends. Multiple instances, if replayed
    str_detect(page, 'question') & eventType=="exp-lookit-images-audio:trialComplete"~ 'time_submit_response', # when click next to launch feedback frame
    str_detect(page, 'feedback') & eventType=="exp-lookit-images-audio:startAudio"~ 'time_solution_start',  # when solution starts playing, multiple instances if replayed
    str_detect(page, 'feedback') & eventType=="exp-lookit-images-audio:clickImage"~ 'time_parent_response', # when parent clicks response coding selection.
    str_detect(page, 'feedback') & eventType=="exp-lookit-images-audio:trialComplete"~ 'time_trial_end' # trial ends when feedback frame ends
    )) %>% 
  filter(!is.na(event_name)) %>% # ignore other events
  select(-frame_num, -event_number, -page, -eventType, -timestamp, -streamTime, -currentTime, -imageId) %>% # remove unneeded columns
  group_by(response_uuid, child_hashed_id, trial_num, frame_restart_num) %>%
  pivot_wider(names_from = event_name, values_from = sessionStreamTime, values_fn = list)
  

# Helper function to parse time strings into numeric milliseconds
parse_time_values <- function(x) {
  # Identify entries that are NA or "N/A"
  x_na <- is.na(x) | x == "N/A"
  
  # Replace "N/A" with NA
  x[x_na] <- NA
  
  # Convert to character and clean strings
  x_char <- as.character(x)
  x_char <- str_trim(x_char)
  x_char <- str_replace_all(x_char, '"', '')          # Remove quotes
  x_char <- str_replace_all(x_char, "^c\\(|\\)$", "") # Remove 'c(' and ')'
  
  # Split strings by comma to handle multiple times
  x_list <- str_split(x_char, ",")
  
  # Convert each element to numeric
  x_numeric_list <- map(x_list, ~ {
    nums <- suppressWarnings(as.numeric(str_trim(.x)))
    # Return NA if all conversions result in NA
    if (all(is.na(nums))) {
      NA_real_
    } else {
      nums
    }
  })
  
  # Assign NA to entries that were originally NA or "N/A"
  x_numeric_list[x_na] <- list(NA_real_)
  
  return(x_numeric_list)
}

# Helper function to get the first time from a list of times
get_first_time <- function(times) {
  if (is.null(times) || length(times) == 0 || all(is.na(times))) {
    return(NA_real_)
  } else {
    return(times[1])
  }
}

# Process and clean the data

df.stumper.trials.merged <- df.stumper.trials %>%
  ungroup() %>%
  # Join with the timing data
  left_join(
    df.timing.cleaned %>% ungroup() %>%
      select(response_uuid, trial_num, frame_restart_num, starts_with("time")),
    by = c("response_uuid", "trial_num")
  ) %>%
  # Rename columns for clarity
  rename(
    restart_num = frame_restart_num,
    question_label = question_id,
    riddle_stop_time = time_question_stop,
    feedback_start_time = time_solution_start
  ) %>%
  # Remove unnecessary columns
  select(
    -parentCode, 
    -time_parent_response, 
    -restart_num, 
    -questionAndResponseDuration
  ) %>%
  # Parse time columns into numeric milliseconds (list-columns)
  mutate(
    time_trial_start = parse_time_values(time_trial_start),
    time_question_start = parse_time_values(time_question_start),
    riddle_stop_time = parse_time_values(riddle_stop_time),
    time_submit_response = parse_time_values(time_submit_response),
    feedback_start_time = parse_time_values(feedback_start_time),
    time_trial_end = parse_time_values(time_trial_end)
  ) %>%
  # Extract numeric values from list-columns
  mutate(
    # Extract the first time_trial_start (numeric)
    time_trial_start_numeric = map_dbl(time_trial_start, ~ {
      times <- .x
      if (is.null(times) || length(times) == 0 || all(is.na(times))) {
        NA_real_
      } else {
        times[1]
      }
    })
  ) %>%
  # Calculate riddle replays and start/stop times
  mutate(
    # Calculate the number of replays by counting the number of times the question was started
    riddle_replays = map_int(time_question_start, ~ {
      times <- .x
      if (is.null(times) || length(times) == 0 || all(is.na(times))) {
        0
      } else {
        max(0, length(times) - 1)
      }
    }),
    # Get all start times as a character string in 'MM:SS' format
    riddle_start_time = map_chr(time_question_start, ~ {
      times <- .x
      if (is.null(times) || length(times) == 0 || all(is.na(times))) {
        "N/A"
      } else {
        paste(ms_to_mmss(times), collapse = ", ")
      }
    }),
    # Get all stop times as a character string in 'MM:SS' format
    riddle_stop_time = map_chr(riddle_stop_time, ~ {
      times <- .x
      if (is.null(times) || length(times) == 0 || all(is.na(times))) {
        "N/A"
      } else {
        paste(ms_to_mmss(times), collapse = ", ")
      }
    })
  ) %>%
  # Calculate the duration of the video segment using numeric times
  mutate(
    trial_duration = ifelse(
      is.na(time_trial_end) | is.na(time_trial_start_numeric), 
      NA_real_, 
      as.numeric(time_trial_end) - time_trial_start_numeric
    )
  ) %>%
  # Convert numeric times back to 'MM:SS' format or 'N/A' if missing
  mutate(
    time_trial_start = ifelse(
      is.na(time_trial_start), 
      "N/A", 
      ms_to_mmss(map_dbl(time_trial_start, get_first_time))
    ),
    time_submit_response = ifelse(
      is.na(time_submit_response), 
      "N/A", 
      ms_to_mmss(map_dbl(time_submit_response, get_first_time))
    ),
    feedback_start_time = ifelse(
      is.na(feedback_start_time), 
      "N/A", 
      ms_to_mmss(map_dbl(feedback_start_time, get_first_time))
    ),
    # Keep time_trial_end from df.timing.cleaned
    time_trial_end = ifelse(
      is.na(time_trial_end), 
      "N/A", 
      ms_to_mmss(time_trial_end)
    ),
    # Format video duration
    trial_duration = ifelse(
      is.na(trial_duration), 
      "N/A", 
      ms_to_mmss(trial_duration)
    )
  ) %>%
  # Remove temporary columns
  select(-time_trial_start_numeric)

# connect to emty coding sheet
sheet_id <- as_sheets_id(sheet_url_stumper)

# read the existing data from the google sheet without column names
existing_data <- read_sheet(sheet_id, sheet = "02_transcriptions", col_names = FALSE)

# extract column names from the second row
col_names <- as.character(unlist(existing_data[2, ]))

# Standardize column names
col_names <- tolower(trimws(col_names))
col_names <- gsub("\\s+", "_", col_names)
col_names <- gsub("[^[:alnum:]_]", "", col_names)  # Remove special characters

# remove first two rows (headers) from existing_data
existing_data <- existing_data[-c(1,2), ]

# set column names to col_names
colnames(existing_data) <- col_names

# rename trial_num to trial_number in your DataFrame
df.stumper.trials.merged <- df.stumper.trials.merged %>%
  rename(trial_number = trial_num,
         trial_end_time = time_trial_end)

# standardize column names in your DataFrame
names(df.stumper.trials.merged) <- tolower(trimws(names(df.stumper.trials.merged)))
names(df.stumper.trials.merged) <- gsub("\\s+", "_", names(df.stumper.trials.merged))
names(df.stumper.trials.merged) <- gsub("[^[:alnum:]_]", "", names(df.stumper.trials.merged))

# convert list-columns to character strings
df.stumper.trials.merged <- df.stumper.trials.merged %>%
  mutate(
    riddle_start_time = map_chr(riddle_start_time, ~ {
      if (is.null(.x) || all(is.na(.x))) {
        "N/A"
      } else {
        paste(.x, collapse = ", ")
      }
    }),
    riddle_stop_time = map_chr(riddle_stop_time, ~ {
      if (is.null(.x) || all(is.na(.x))) {
        "N/A"
      } else {
        paste(.x, collapse = ", ")
      }
    }),
    feedback_start_time = map_chr(feedback_start_time, ~ {
      if (is.null(.x) || all(is.na(.x))) {
        "N/A"
      } else {
        paste(.x, collapse = ", ")
      }
    })
  )

# prepare data for Google Sheets
df_to_write <- df.stumper.trials.merged %>%
  select(any_of(col_names))

# add missing columns as NA
missing_cols <- setdiff(col_names, names(df_to_write))
for (col in missing_cols) {
  df_to_write[[col]] <- NA
}

# reorder columns to match the sheet
df_to_write <- df_to_write %>% select(all_of(col_names))

# ensure riddle_replays is numeric
df_to_write$riddle_replays <- as.numeric(df_to_write$riddle_replays)

# specify the starting row directly
start_row <- 3

# specify the range to start writing
range <- glue::glue("A{start_row}")

# write data to Google Sheets without overwriting headers
tryCatch({
  range_write(sheet_id, df_to_write, sheet = "02_transcriptions", range = range, col_names = FALSE)
  print("Data has been successfully written to the Google Sheet.")
}, error = function(e) {
  print(paste("An error occurred:", e$message))
})

## Import Transcription / trial exclusion ----
df.stumpers.transcripts <- read_sheet(
  annotations_url,
  sheet = "2. Trial transcriptions",
  skip=1, # skip description row
  trim_ws=TRUE,
  col_names=TRUE,
  na=c("NA", "N/A", "n/a", "")
) %>%
  # Clean up column names but keep all columns
  rename_with(~ tolower(gsub("\\s+", "_", .x))) %>%
  mutate(trial_number= as.integer(trial_number),
         trial_exclusion_reason = as.factor(trial_exclusion_reason))

## Import Response Coding ----
df.stumpers.coding <- read_sheet(
  annotations_url,
  sheet = "3. Response coding",
  skip=1,
  trim_ws=TRUE,
  col_names=TRUE,
  na=c("NA", "N/A", "n/a", "")
)

## summary
df.stumpers.transcripts %>% 
  filter(response_uuid %in% usable.sessions) %>%
  count(trial_exclusion_reason) %>%
  arrange(-n)

## export
df.stumpers.transcripts %>% 
  write_csv(here(PATH_OUTPUT_DATA, "annotations/garden2a_stumpers_transcriptions.csv"))

## Import & store response coding ----
df.stumpers.coder1 <- read_sheet(
  annotations_url,
  sheet = "3. Response coding",
  skip=1, # skip description row
  trim_ws=TRUE,
  col_names=TRUE,
  na=c("NA", "N/A", "n/a", "")
) %>%
  # Clean up column names but keep all columns
  rename_with(~ tolower(gsub("\\s+", "_", .x)))

## export
df.stumpers.coder1 %>%
  left_join(select(df.participant_tracker, 
                   response_uuid=response__uuid,
                   child_hashed_id = child__hash_id,
                   child__gender)) %>% 
  write_csv(here(PATH_OUTPUT_DATA,
                 "annotations/garden2a_stumpers_coder1.csv"))

## Make reliability coding sheet ----

head(df.stumpers.coder1)

if (make_reliability_coding) {
  set.seed(123) # for reproducibility
  # sample 25% of trials from each question
  ntrials_to_recode = floor(nrow(df.stumpers.coder1) * 0.25 / 9) 
  sampled_trials <- df.stumpers.coder1 %>%
    group_by(question_id) %>%
    sample_n(ntrials_to_recode) %>%
    mutate(coder_id = "Julio") %>%
    select(response_uuid, coder_id, question_set, question_id, 
           response_transcript, transcriber_comments)
  
  # Push reliability trials to be coded ---- 
  # export for version control
  write_csv(sampled_trials, here(PATH_OUTPUT_DATA, "annotations/garden2a_stumpers_trials-for-reliability-coding.csv"))
}
## Import & store response reliability coding ----

df.stumpers.coder2 <- read_sheet(
  annotations_url,
  sheet = "4. Response reliability coding",
  skip=1, # skip description row
  trim_ws=TRUE,
  col_names=TRUE,
  na=c("NA", "N/A", "n/a", "")
) %>%
  # Clean up column names but keep all columns
  rename_with(~ tolower(gsub("\\s+", "_", .x)))

## export
write_csv(df.stumpers.coder2, 
          here(PATH_OUTPUT_DATA,"annotations/garden2a_stumpers_coder2.csv"))
