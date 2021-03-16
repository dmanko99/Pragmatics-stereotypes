library(jsonlite)
library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Helper script: take a sentence, plus a critical word in the sentence
# and return a list in the format used for 245B template RT studies.

make_wordsSubArray <- function(crit_sentence, crit_word) {
  
  wordsList <- str_split(crit_sentence, " ")
  outputDF <- setNames(data.frame(wordsList), c("form")) %>%
    mutate(region = case_when(form == "FIRST" ~ "name",
                              as.character(form) == as.character(crit_word) ~ "crit",
                              TRUE ~ "filler")) %>%
    mutate(lbr_before = case_when(as.character(form) == as.character(crit_word) ~ "true", 
                                  TRUE ~ "false")) %>%
    #mutate(lbr_before = case_when(form == crit_word ~ "true",
    #                               TRUE ~ "false")) %>%
    mutate(lbr_after = case_when(lead(lbr_before) == "true" ~ "true",
                                  TRUE ~ "false")) %>%
    mutate(form_order = row_number())
  
  return(outputDF)
  
}

# Test the output.
# make_wordsSubArray("FIRST is the subject of a test sentence.", "test")

#----------------------------
### WORKING WITH CRITICAL STIMS
# Read in the stims

critstims <- read_csv("RT_criticals.csv")

# Make list of lists of word features (for each critical sentence)

wordsList = c()

for (i in 1:nrow(critstims)) {
  words <- make_wordsSubArray(critstims[i, "crit_sentence"],
                              critstims[i, "crit_word"])
  wordsList[[i]] = words
}

rm(words)

critstims$words <- wordsList

# Note to Dean: renaming variables here according to names in the .js file,
# But can we change 'symmetry' to 'bias' in the experiment script?
# Then you can get rid of 'symmetry = bias' in the call to 'rename' below.

critstims_toJSON <- critstims %>%
  rename(question = comp_question, correct_answer = answer,
         trial_id = id, intro = intro_sentence) %>%
  mutate(type = "critical") %>%
  select(words, intro, question, correct_answer, type, trial_id, race, bias, race_consistency)

toJSON(critstims_toJSON %>%
         filter(race_consistency == "white_consistent") #%>%
         #select(-race_consistency)
       )

white_consistentJSON <- toJSON(critstims_toJSON %>%
                                 filter(race_consistency == "white_consistent") #%>%
                                 #select(-race_consistency)
                               )

black_consistentJSON <- toJSON(critstims_toJSON %>%
                                 filter(race_consistency == "black_consistent") #%>%
                                 #select(-race_consistency)
                               )

white_inconsistentJSON <- toJSON(critstims_toJSON %>%
                                 filter(race_consistency == "white_inconsistent") #%>%
                                 #select(-race_consistency)
                                 )

black_inconsistentJSON <- toJSON(critstims_toJSON %>%
                                 filter(race_consistency == "black_inconsistent") #%>%
                                 #select(-race_consistency)
                                 )

header = sprintf("var white_consistent = %s; \nvar black_consistent = %s; \nvar white_inconsistent = %s; \nvar black_inconsistent = %s; \n", 
                 white_consistentJSON, black_consistentJSON, 
                 white_inconsistentJSON, black_inconsistentJSON)

write_file(header, "RT_criticals.js")


#----------------------------
### WORKING WITH FILLER STIMS
# Read in the stims
fillerstims <- read_csv("RT_fillers.csv")

# Make list of lists of word features (for each filler sentence)

wordsList_fill = c()

for (i in 1:nrow(fillerstims)) {
  words_fill <- make_wordsSubArray(fillerstims[i, "crit_sentence"],
                              fillerstims[i, "crit_word"])
  wordsList_fill[[i]] = words_fill
}

rm(words_fill)

fillerstims$words <- wordsList_fill

fillerstims_toJSON <- fillerstims %>%
  rename(question = comp_question, correct_answer = answer,
         trial_id = id, intro = intro_sentence) %>%
  mutate(type = "filler") %>%
  select(words, intro, question, correct_answer, type, trial_id, race, bias, race_consistency)

whitefillerJSON <- toJSON(fillerstims_toJSON %>%
                                 filter(race == "white")
)

blackfillerJSON <- toJSON(fillerstims_toJSON %>%
                                 filter(race == "black")
)

header_filler = sprintf("var white_fillers = %s; \nvar black_fillers = %s; \n", 
                 whitefillerJSON, blackfillerJSON)

write_file(header_filler, "RT_fillers.js")


### WORKING WITH PRACTICE STIMS
# New helper function
make_wordsSubArrayPractice <- function(crit_sentence, crit_word) {
  
  wordsList <- str_split(crit_sentence, " ")
  outputDF <- setNames(data.frame(wordsList), c("form")) %>%
    mutate(region = case_when(form == "FIRST" ~ "name",
                              TRUE ~ "filler")) %>%
    mutate(form_order = row_number())
  
  return(outputDF)
  
}
# Read in the stims
practicestims <- read_csv("RT_practice.csv")

# Make list of lists of word features (for each filler sentence)

wordsList_prac = c()

for (i in 1:nrow(practicestims)) {
  words_prac <- make_wordsSubArrayPractice(practicestims[i, "crit_sentence"],
                                   practicestims[i, "crit_word"])
  wordsList_prac[[i]] = words_prac
}

rm(words_prac)

practicestims$words <- wordsList_prac

practicestims_toJSON <- practicestims %>%
  rename(question = comp_question, correct_answer = answer,
         trial_id = id, intro = intro_sentence) %>%
  mutate(type = "practice") %>%
  select(words, intro, question, correct_answer, trial_id)

practice_header_filler <- sprintf("var practice_stims = %s;", toJSON(practicestims_toJSON))

write_file(practice_header_filler, "RT_practice.js")
