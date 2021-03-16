library(tidyverse)
library(lme4)
library(lmerTest)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# HELPER SCRIPTS
dodge = position_dodge(.9)
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  mean(x,na.rm=na.rm) - quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) - mean(x,na.rm=na.rm)}

# Import dataframe
#df <- read_csv("../data/RT_mainNorming/tesRT-trials.csv", header = TRUE) %>%
df = read.csv("../data/Pilot/tesRT_pilot2-trials.csv", header = TRUE) %>%
  # remove bot check slides
  filter(!(type == "bot_check")) %>%
  # remove the practice trial
  filter(!(type == "practice")) %>%
  # remove non-relevant cols
  select( -c(audio, image, response, slide_number, slide_type, error)) %>%
  drop_na()

subinfo = read.csv("../data/RT_mainNorming/tesRT-subject_information.csv", header = TRUE)

# SANITY CHECK: SHOULD EQUAL 180
length(unique(df$workerid))

# Create trial summary df
by_trial <- df %>%
  group_by(workerid, trial_id, response_correct, nameCategory, bias, type) %>%
  summarise_each(list())

# Exclude poor-performing participants (>3 wrong on comp trials)

excludeList <- (by_trial %>%
  group_by(workerid) %>%
  summarize(nWrong = sum(response_correct == 0)) %>%
  ungroup() %>%
  filter(nWrong > 3))$workerid

df <- df %>%
  filter(!(workerid %in% excludeList))

# Identify per-trial rt (and exclude data from trials > or < 2.5 sds from mean)

df <- df %>%
  group_by(workerid, trial_no) %>%
  mutate(trial_time = sum(rt)) 

trialTimeMean <- mean(df$trial_time)
trialTimeSD <- sd(df$trial_time)
slowTrial <- trialTimeMean + (2.5 * trialTimeSD)
fastTrial <- trialTimeMean - (2.5 * trialTimeSD)

df <- df %>%
  filter(!(trial_time > slowTrial || trial_time < fastTrial))

# Data transformations: log transform & word length

df$logrt = log(df$rt)
df$wordlen = nchar(as.character(df$form))
df$logtrial = log(df$trial_no + 1)

# Regress word length and overall trial number onto log RT (ie, subtract out the variance contributed by these factors.)

resid_model = lmer(logrt ~ wordlen * logtrial + (wordlen | workerid), df, REML=F)  # estimates the random effects of per participant reading time differences

res = resid(resid_model)

df$residuals <- res

# Predict residualized reading time from race, consistency, and interaction

criticaldf <- filter(df, type == "critical")
criticaldf$bias <- factor(criticaldf$bias)
criticaldf$nameCategory <- factor(criticaldf$nameCategory)
criticaldf <- criticaldf %>%
  mutate(consistency = case_when(bias == nameCategory ~ "consistent", TRUE ~ "inconsistent"))


model = lmer(residuals ~ nameCategory * consistency + 
               ((nameCategory * consistency) | trial_id) + ((nameCategory * consistency) | workerid), criticaldf,
             REML = F)

summary(model)

# Visualize raw & transformed data
rawgrouped = criticaldf %>% 
  group_by(type, region, trial_no) %>% 
  summarize(rt = mean(rt),CILow=ci.low(rt),CIHigh=ci.high(rt)) %>% 
  ungroup() %>% 
  mutate(YMin=rt-CILow,YMax=rt+CIHigh)

ggplot(rawgrouped, aes(x=region, y=rt, color=type, group=type)) + 
  geom_point()  + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_line() + 
  colscale +
  labs(x='Sentence region', y='Mean raw RT (ms)', fill='Type')

ggplot(rawgrouped, aes(x=trial_no, y=rt, col=type)) + 
  geom_point(alpha=.2)  + 
  geom_smooth() + 
  colscale +
  labs(x='Item order (#RCs seen)', y='Raw RTs (ms)', fill='Type')

grouped = criticaldf %>% 
  group_by(trial_no, type) %>% 
  summarize(rt = mean(residuals))
ggplot(grouped, aes(x=trial_no, y=rt, col=type)) + 
  geom_point() +
  geom_smooth(method="lm") + 
  colscale +
  labs(x='Item order (#RCs seen)', y='Length- and order-corrected log RTs (ms)', fill='Type')
