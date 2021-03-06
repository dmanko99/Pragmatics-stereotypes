library(dplyr)
library(tidyverse)
library(jsonlite)
library(bootstrap)
library(brms)
library(ggwordcloud)
library(tm)

source("helpers.R")

theme_set(theme_bw())

options(mc.cores = parallel::detectCores())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))    

# Load the data
df = read.csv("../data/Norming/Live/tes_norming-trials.csv", header = TRUE)%>%
  filter(slide_type != "bot_check")

subinfo = read.csv("../data/Norming/Live/tes_norming-subject_information.csv", header = TRUE)

# DETERMINE MEAN COMPLETION TIME
mean((read.csv("../data/Norming/Live/tes_norming-time_in_minutes.csv", header = TRUE))$time_in_minutes)

# HELPER SCRIPTS
dodge = position_dodge(.9)
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  mean(x,na.rm=na.rm) - quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) - mean(x,na.rm=na.rm)}

# SANITY CHECK: SHOULD EQUAL 200
length(unique(df$workerid))

# MAKE EXCLUSIONS
wrong_responses <- df %>%
  filter(tag == "exclusion" & response == "fail")
blacklist <- unique(wrong_responses$workerid)

df <- df %>%
  filter(!(workerid %in% blacklist))

# CODE NAME CATEGORY
df$nameCategory <- factor(df$first %in% c("Trevon",
                                        "Tyree",
                                        "Deion",
                                        "Marquis",
                                        "Jermaine",
                                        "Lamont",
                                        "Tyrone",
                                        "Deandre",
                                        "Tremayne",
                                        "Lamar",
                                        "Kareem",
                                        "Hakeem",
                                        "Jamal",
                                        "Rasheed",
                                        "Deshawn"))

# SANITY CHECK: TRUE AND FALSE SHOULD BE EQUAL
df %>%
  filter(type != "exclusion") %>%
  group_by(nameCategory) %>%
  summarize(n = n())

levels(df$nameCategory) <- c("white", "black")

# RENAME COLUMNS
df <- df %>%
  rename(stereotype = tag,
         name = first)

# DROP IRRELEVANT COLUMNS
df <- subset(df, select = -c(audio, image, slide_number, slide_type, error, proliferate.condition))

# CONVERT RESPONSES TO USABLE TEXT
df$response <- as.character(df$response)

df$response <- df$response %>%
  stripWhitespace() %>%
  removeWords( stopwords("english")) %>%
  tolower() %>%
  removePunctuation(preserve_intra_word_contractions = FALSE,
                    preserve_intra_word_dashes = FALSE,
                    ucp = FALSE)

# SET UP BASIC DATAFRAMES
critical_df <- filter(df, type == "critical")

filler_df <- filter(df, type == "filler")

# SET UP DATAFRAMES BY STORY
storylist <- unique(critical_df$story)

byStoryResponse <- critical_df %>%
  select(name, response, story, nameCategory) %>%
  group_split(story, nameCategory)

byStoryFreq <- 

# FREQUENCY OF RESPONSE BY STORY - to be expanded on with more data
df_freq <- critical_df %>%
  #count(story, sort = TRUE) %>%
  group_by(story) %>%
  count(response, sort = TRUE)

#### looking at relative frequency
##### here nStoryRace is the number of time a story/nameCategory combination was presented
##### rFreq is the relative frequency, the percentage of times that a response occurred
#####   in a given nStory combination — simply put, n ÷ nStoryRace
attempt1 <- critical_df %>%
  group_by(story, nameCategory)

attempt2 <- function(rowNum, counter) {
  story_instance <- df_freq_race[rowNum,]
  race_instance <- df_freq_race[rowNum,]
  counter
}

df_freq_race$nStoryRace <- critical_df %>%
  group_by(story, nameCategory) %>%
  length()

df_freq$rel_freq <- 

df_freq_race <- critical_df %>%
  group_by(story, nameCategory) %>%
  count(response, sort = TRUE)
  

# OBTAINING LIST OF RESPONSES BY STORY TO LOOK AT ASYMMETRICAL RESPONSES
get_responses <- function(prompt_name) {
  response_list_white <- list()
  response_list_black <- list()
  asymmetrical_response_list_w <- list()
  subframe_white <- filter(critical_df, story == prompt_name & nameCategory == "white") 
  subframe_black <- filter(critical_df, story == prompt_name & nameCategory == "black")
  
  for (i in subframe_white$response) {
    response_list_white <- c(response_list_white, i)
  }
  for (j in subframe_black$response) {
    response_list_black <- c(response_list_black, j)
  }
  for (k in response_list_white) {
    if (k %in% response_list_black == FALSE) {
      asymmetrical_response_list_w <- c(asymmetrical_response_list_w, k)
    }
  }
  
  asymmetrical_w_story_df <- do.call(rbind.data.frame, asymmetrical_response_list_w)
  asymmetrical_w_story_df$story <- prompt_name
  colnames(asymmetrical_w_story_df) <- c("response", "story")
  return(asymmetrical_w_story_df)
}

for (storyName in storylist) {
  if (storyName == "performing") { #to create the dataframe, unsure how to add to empty df
    firstStory_asym_w_df <- get_responses(storyName)
  } else if (storyName == "lazy day") { #this is the second story
    otherStory_asym_w_df <- get_responses(storyName)
    asym_w_df <- rbind(firstStory_asym_w_df, otherStory_asym_w_df)
  } else {
    otherStory_asym_w_df <- get_responses(storyName)
    asym_w_df <- rbind(asym_w_df, otherStory_asym_w_df)
  }
}

asym_w_freq_df <- asym_w_df %>%
  group_by(story) %>%
  count(response, sort = TRUE)

### exporting these dataframes
for (item in storylist) {
  write.csv((filter(asym_w_freq_df, story == item)),
            sprintf("../tables/asymfreqW.%s.csv", item))
}


# dataframe of all response lists by story — to be updated when more data is available
# responses_white <- c()
# responses_black <- c()
# for (a in storylist) {
#   subf <- get_responses(a)
#   responses_white <- c(responses_white, subf[1:(length(subf)/2)])
#   responses_black <- c(responses_black, subf[(length(subf)/2):(length(subf))])
# }
# byStoryLists <- data.frame(x = storylist, y = responses_by_story)
# responses_by_story <- data.frame(storylist, )

# VISUALIZE RESPONSES BY STORY
wordcloud_by_story <- ggplot(critical_df, aes(label = response, color = nameCategory)) +
  geom_text_wordcloud() +
  theme_minimal() +
  facet_wrap(~story)

wordcloud_freq <- ggplot(data = df_freq, aes(label = response, size = n)) +
  geom_text_wordcloud() +
  facet_wrap(~story)

wc_freq_races <- ggplot(data = df_freq_race, aes(label = response, size = n)) +
  geom_text_wordcloud(area_corr = TRUE) +
  scale_size_area(max_size = 24) +
  facet_wrap(~story)  

wc_trial1 <- ggplot(data = filter(df_freq_race, story == "test"), aes(label = response, size = n, color = nameCategory)) +
  geom_text_wordcloud(area_corr = TRUE, rm_outside = TRUE) +
  scale_size_area(max_size = 10)


# FILLER STIMS
## frequency of responses
df_filler_fr <- filler_df %>%
  group_by(story, nameCategory) %>%
  count(response, sort = TRUE)
##visualization
wc_trial2 <- ggplot(data = filter(df_filler_fr, story == "job"), aes(label = response, size = n, color = nameCategory)) +
  geom_text_wordcloud(area_corr = TRUE, rm_outside = TRUE) +
  scale_size_area(max_size = 10)
