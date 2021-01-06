library(dplyr)
library(tidyverse)
library(jsonlite)
library(bootstrap)
library(brms)

source("helpers.R")

theme_set(theme_bw())

options(mc.cores = parallel::detectCores())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))    

# Load the data
df = read.csv("../data/Pilot/tesnorming-trials.csv", header = TRUE)%>%
  filter(slide_type != "bot_check")

subinfo = read.csv("../data/Pilot/tesnorming-subject_information.csv", header = TRUE)

# DETERMINE MEAN COMPLETION TIME
mean(df$Answer.time_in_minutes)

# HELPER SCRIPTS
dodge = position_dodge(.9)
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  mean(x,na.rm=na.rm) - quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) - mean(x,na.rm=na.rm)}

# SANITY CHECK: SHOULD EQUAL 280
length(unique(df$workerid))

# MAKE EXCLUSIONS
wrong_responses <- df %>%
  filter((tag == "exclusion_wrong" & response > 50) | (tag == "exclusion_right" && response < 50))
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
         item = story,
         name = first)

# VISUALIZE RESPONSE BY STEREOTYPE AND NAME
df %>% 
  filter(type == "critical") %>%
  filter(stereotype == "evoke") %>%
  ggplot(aes(x = response, fill = nameCategory)) +
  geom_histogram(alpha = 0.5, position = "identity") +
  facet_wrap(~item) 

byStereotypeNameCategory <- df %>%
  filter(type == "critical") %>%
  group_by(nameCategory, stereotype) %>%
  summarize(Mean = mean(response), 
            CILow =ci.low(response),
            CIHigh =ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow,
         YMax = Mean + CIHigh)

ggplot(byStereotypeNameCategory, aes(x=nameCategory, y=Mean, fill = stereotype)) +
  # facet_wrap(~kind, scales = "free") +
  theme_bw() +
  scale_fill_grey() +
  geom_bar(stat="identity",position = "dodge") +
  theme(axis.text.x=element_text(angle=20,hjust=1,vjust=1)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),size = 0.25,width= 0.025,position = dodge) +  
  labs(x = "Race", y = "Mean rating", fill = "Stereotype") 


# BY STEREOTYPE AND NAME CATEGEORY, BY WORKER ID
byStereotypeNameCategoryWorker <- df %>%
  filter(type == "critical") %>%
  group_by(nameCategory, stereotype, workerid) %>%
  summarize(Mean = mean(response), 
            CILow =ci.low(response),
            CIHigh =ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow,
         YMax = Mean + CIHigh)
ggplot(byStereotypeNameCategoryWorker, aes(x=nameCategory, y=Mean, fill = stereotype)) +
  # facet_wrap(~kind, scales = "free") +
  facet_wrap(~workerid) +
  theme_bw() +
  scale_fill_grey() +
  geom_bar(stat="identity",position = "dodge") +
  theme(axis.text.x=element_text(angle=20,hjust=1,vjust=1)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),size = 0.25,width= 0.025,position = dodge) +  
  labs(x = "Race", y = "Mean rating", fill = "Stereotype") 


# VISUALIZE RESPONSE BY NAME AND ITEM
byNameCategoryItem <- df %>%
  filter(type == "critical") %>%
  group_by(nameCategory, item, stereotype) %>%
  summarize(Mean = mean(response), 
            CILow =ci.low(response),
            CIHigh =ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow,
         YMax = Mean + CIHigh)

byNameCategoryItem %>%
  # CAN BE 'evoke' or 'repress'
  filter(stereotype == "evoke") %>%
  ggplot(aes(x=item, y=Mean, fill = nameCategory)) +
  theme_bw() +
  scale_fill_grey() +
  geom_bar(stat="identity",position = "dodge") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=1)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),size = 0.25,width= 0.025,position = dodge) +  
  labs(x = "Item", y = "Mean rating", fill = "Name") 

# MEAN CENTERING AND BRMS ANALYSIS
df$response_ctr <- scale(df$response, center = TRUE, scale = FALSE)
df$response_ctr <- df$response_ctr - mean(df$response_ctr)
df$stereotype = relevel(df$stereotype, ref = "repress")

##center affiliation measurements?
dfAffiliation$response_ctr <- scale(df$response, center = TRUE, scale = FALSE)
dfAffiliation$response_ctr <- df$response_ctr - mean(df$response_ctr)
dfAffiliation$stereotype = relevel(df$stereotype, ref = "repress")


m <- brm(
  response_ctr ~ nameCategory * stereotype + (1 + nameCategory|item) + (1 + stereotype|name) + (1 + nameCategory*stereotype|workerid), 
  data = df %>% filter(type == "critical"),
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  family = gaussian(),
  seed = 123,
  iter = 5000
)

summary(m)

m_affiliation <- brm(
  response_ctr ~ nameCategory * stereotype * affiliation + (1 + nameCategory*stereotype|workerid) +
    (1 + stereotype*affiliation|nameCategory),
  data = dfAffiliation %>% filter(type == "critical", affiliation > -1),
  control = list(adapt_delta = 0.9, max_treedepth = 15),
  family = gaussian(),
  seed = 123,
  iter = 5000
)

summary(m_affiliation)
# COEFFICIENT IS GREATER THAN 0: 

hypothesis(m, "nameCategoryTRUE:stereotypeevoke > 0")
