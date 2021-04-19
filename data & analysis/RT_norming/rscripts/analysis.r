library(tidyverse)
library(lme4)
library(lmerTest)
library(ggplot2)
library(bootstrap)
library(tm)
library(plyr)

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
df <- read_csv("../data/RT_mainNorming/tesRT-trials.csv") %>%
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

rawUpperBound = mean(df$rt) + (2*sd(df$rt))
rawLowerBound = mean(df$rt) - (2*sd(df$rt))

df <- df %>%
  filter(!(workerid %in% excludeList)) %>%
  filter(!(rt >= rawUpperBound)) %>%
  filter(!(rt <= rawLowerBound))

# Remove punctuation from items as to not mess up word lengths
df$form = gsub("&rsquo;", "", df$form) #removing HTML code for apostrophes
df$form = gsub("&quot;", "", df$form) #removing HTML code for double quotes
df$form = removePunctuation(df$form) #removing all remaining punctuation

# Set up different regions
df <- df %>%
  mutate(region_old = region) %>%
  mutate(region = case_when(((lead(region_old) == "crit") & (lead(trial_id) == trial_id) &
                               (lead(workerid) == workerid))
                            ~ "precritical_1",
                            ((lag(region_old) == "crit") & (lag(trial_id) == trial_id) &
                               (lag(workerid) == workerid))
                            ~ "spillover_1",
                            region_old == "crit" ~ "crit",
                            region_old == "name" ~ "name",
                            TRUE ~ "other")) %>%
  mutate(region_old = region) %>%
  mutate(region = case_when(region_old == "precritical_1" ~ "precritical_1",
                            region_old == "spillover_1" ~ "spillover_1",
                            region_old == "crit" ~ "crit",
                            region_old == "name" ~ "name",
                            ((lead(region_old) == "precritical_1") & (lead(trial_id) == trial_id) &
                               (lead(workerid) == workerid))
                            ~ "precritical_2",
                            ((lag(region_old) == "spillover_1") & (lag(trial_id) == trial_id) &
                               (lag(workerid) == workerid))
                            ~ "spillover_2",
                            TRUE ~ "other")) %>%
  select(-region_old)

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

resid_model = lmer(logrt ~ wordlen + (0+wordlen|workerid), data = df)
res = resid(resid_model)

df$residuals <- res

# Predict residualized reading time from race, consistency, and interaction

## For critical stories, items in the "critical" region
criticaldf <- filter(df, region == "crit", type == "critical")
criticaldf$bias <- factor(criticaldf$bias)
criticaldf$nameCategory <- factor(criticaldf$nameCategory)
criticaldf <- criticaldf %>%
  mutate(consistency = case_when(bias == nameCategory ~ "consistent", TRUE ~ "inconsistent"))

criticaldf$ctr_nameCategory = as.numeric(criticaldf$nameCategory) - mean(as.numeric(criticaldf$nameCategory))
criticaldf$ctr_consistency = as.numeric(factor(criticaldf$consistency)) - mean(as.numeric(factor(criticaldf$consistency)))


model = lmer(residuals ~ logtrial * ctr_nameCategory * ctr_consistency + 
               (1 + (logtrial * ctr_nameCategory * ctr_consistency) | trial_id) + 
               (1 + (logtrial * ctr_nameCategory * ctr_consistency) | workerid), 
             criticaldf, REML = F)

model2 = lmer(residuals ~ logtrial * ctr_nameCategory * ctr_consistency + 
               (1 | trial_id) + 
               (1 | workerid), 
             criticaldf, REML = F)

summary(model)
summary(model2)

## Looking at all regions of interest (namely pre-crit, crit, and spillover)
subregion_df <- filter(df, region %in% c("precritical_1", "precritical_2",
                                         "crit", "spillover_1", "spillover_2"),
                       type == "critical")

subregion_df$bias <- factor(subregion_df$bias)
subregion_df$nameCategory <- factor(subregion_df$nameCategory)
subregion_df <- subregion_df %>%
  mutate(consistency = case_when(bias == nameCategory ~ "consistent", TRUE ~ "inconsistent"))

subregion_df$ctr_nameCategory = as.numeric(subregion_df$nameCategory) - 
  mean(as.numeric(subregion_df$nameCategory))
subregion_df$ctr_consistency = as.numeric(factor(subregion_df$consistency)) - 
  mean(as.numeric(factor(subregion_df$consistency)))

## getting residuals & length-correcting
subregion_df$logrt = log(subregion_df$rt)
subregion_df$wordlen = nchar(as.character(subregion_df$form))
subregion_df$logtrial = log(subregion_df$trial_no + 1)
subregion_resid_model <- lmer(logrt ~ wordlen + (0+wordlen|workerid), data = subregion_df)
subregion_res <- resid(subregion_resid_model)
subregion_df$residuals <- subregion_res

##centering RTs
subregion_df$ctr_RT = as.numeric(subregion_df$residuals) - mean(subregion_df$residuals)


# Visualize raw & transformed data
#rawgrouped = filter(subregion_df, type == "critical") %>% 
 # mutate(consistency = case_when(bias == 
  #                                 nameCategory ~ "consistent", 
   #                              TRUE ~ "inconsistent")) %>%
rawgrouped = subregion_df %>%
  group_by(region, consistency, nameCategory, trial_id) %>% 
  summarize(mean_rt = mean(rt),CILow=ci.low(rt),CIHigh=ci.high(rt)) %>% 
  ungroup() %>% 
  mutate(YMin=mean_rt-CILow,YMax=mean_rt+CIHigh)

rawgrouped$region <- factor(rawgrouped$region, levels=c("precritical_2",
                                                        "precritical_1",
                                                        "crit",
                                                        "spillover_1",
                                                        "spillover_2"))

residuals_grouped = subregion_df %>%
  group_by(region, consistency, nameCategory, trial_id) %>% 
  summarize(res_RT = resid(lmer(log(rt) ~ nchar(as.character(form)) + (0+(nchar(as.character(form)))|workerid), data = subregion_df)),
            CILow=ci.low(res_RT), CIHigh=ci.high(res_RT)) %>% 
  ungroup() %>% 
  mutate(YMin=res_RT-CILow,YMax=res_RT+CIHigh)

residuals_grouped2 = subregion_df %>%
  group_by(region, consistency, nameCategory, trial_id) %>% 
  summarize(mean_resid = mean(residuals), CILow=ci.low(residuals), CIHigh=ci.high(residuals)) %>% 
  ungroup() %>% 
  mutate(YMin=mean_resid-CILow,YMax=mean_resid+CIHigh)


residuals_grouped2$region <- factor(residuals_grouped2$region, levels=c("precritical_2",
                                                        "precritical_1",
                                                        "crit",
                                                        "spillover_1",
                                                        "spillover_2"))

residuals_wbias = subregion_df %>%
  group_by(region, bias, nameCategory, trial_id) %>% 
  summarize(mean_resid = mean(residuals), CILow=ci.low(residuals), CIHigh=ci.high(residuals)) %>% 
  ungroup() %>% 
  mutate(YMin=mean_resid-CILow,YMax=mean_resid+CIHigh)

residuals_wbias$region <- factor(residuals_wbias$region)
residuals_wbias$region <- factor(residuals_wbias$region, levels=c("precritical_2",
                                                                        "precritical_1",
                                                                        "crit",
                                                                        "spillover_1",
                                                                        "spillover_2"))
residuals_wbias$bias <- revalue(residuals_wbias$bias, c("black" = "black-typical",
                                                                "white" = "white-typical"))


#raw reading times by item and consistency
ggplot(rawgrouped, aes(x=region, y=mean_rt, color=consistency, group=consistency)) + 
  facet_grid(nameCategory~trial_id) +
  geom_point()  + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_line() +
  labs(x='Sentence region', y='Mean raw RT (ms)', fill='Type') +
  theme(axis.text.x = element_text(angle = 30))

#residualized/centered reading times by consistency
ggplot(residuals_grouped2, aes(x=region, y=mean_resid, color=consistency, group=consistency)) + 
  facet_wrap(~nameCategory) +
  geom_point()  + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_line() +
  labs(x='Sentence region', y='Residualized/Centered RT (ms)', fill='Type') +
  theme(axis.text.x = element_text(angle = 30))

#and also by item
ggplot(residuals_grouped2, aes(x=region, y=mean_resid, color=consistency, group=consistency)) + 
  facet_grid(nameCategory~trial_id) +
  geom_point()  + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_line() +
  labs(x='Sentence region', y='Residualized/Centered RT (ms)', fill='Type') +
  theme(axis.text.x = element_text(angle = 30))


#looking at same-item data -- by nameCategory
ggplot(residuals_wbias, aes(x=region, y=mean_resid, color=nameCategory, group=nameCategory)) + 
  facet_wrap(~bias) +
  geom_point()  + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_line() +
  labs(x='Sentence region', y='Residualized/Centered RT (ms)', fill='Type') +
  theme(axis.text.x = element_text(angle = 30))
## and now by trial id
ggplot(residuals_wbias, aes(x=region, y=mean_resid, color=nameCategory, group=nameCategory)) + 
  facet_wrap(trial_id~bias) +
  geom_point()  + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_line() +
  labs(x='Sentence region', y='Residualized/Centered RT (ms)', fill='Type') +
  theme(axis.text.x = element_text(angle = 30))


ggplot(filter(df, type == "critical"), aes(x=trial_no, y=rt, col=type)) + 
  geom_point(alpha=.2)  + 
  #geom_smooth() + 
  #colscale +
  labs(x='Item order (#RCs seen)', y='Raw RTs (ms)', fill='Type')

grouped = subregion_df %>% 
  group_by(trial_no, type) %>% 
  summarize(rt = mean(residuals))
ggplot(grouped, aes(x=trial_no, y=rt, col=type)) + 
  geom_point() +
  geom_smooth(method="lm") + 
  #colscale +
  labs(x='Item order (#sentences seen)', y='Length- and order-corrected log RTs (ms)', fill='Type')

### FILLER STIM ANALYSES
#set up dataframe
filler_df <- filter(df, region %in% c("precritical_1", "precritical_2",
                                         "crit", "spillover_1", "spillover_2"),
                       type == "filler")

filler_df$bias <- factor(filler_df$bias)
filler_df$nameCategory <- factor(filler_df$nameCategory)

filler_df$ctr_nameCategory = as.numeric(filler_df$nameCategory) - 
  mean(as.numeric(filler_df$nameCategory))
#filler_df$ctr_consistency = as.numeric(factor(filler_df$consistency)) - 
#  mean(as.numeric(factor(filler_df$consistency)))

## getting residuals & length-correcting
filler_df$logrt = log(filler_df$rt)
filler_df$wordlen = nchar(as.character(filler_df$form))
filler_df$logtrial = log(filler_df$trial_no + 1)
filler_resid_model <- lmer(logrt ~ wordlen + (0+wordlen|workerid), data = filler_df)
filler_res <- resid(filler_resid_model)
filler_df$residuals <- filler_res

##centering RTs
filler_df$ctr_RT = as.numeric(filler_df$residuals) - mean(filler_df$residuals)

fillers_byTrial <- filler_df%>%
  group_by(region, nameCategory, trial_id) %>% 
  summarize(mean_resid = mean(residuals), CILow=ci.low(residuals), CIHigh=ci.high(residuals)) %>% 
  ungroup() %>% 
  mutate(YMin=mean_resid-CILow,YMax=mean_resid+CIHigh)


fillers_byTrial$region <- factor(fillers_byTrial$region, levels=c("precritical_2",
                                                                        "precritical_1",
                                                                        "crit",
                                                                        "spillover_1",
                                                                        "spillover_2"))


fillers_byRace <- filler_df%>%
  group_by(region, nameCategory) %>% 
  summarize(mean_resid = mean(residuals), CILow=ci.low(residuals), CIHigh=ci.high(residuals)) %>% 
  ungroup() %>% 
  mutate(YMin=mean_resid-CILow,YMax=mean_resid+CIHigh)


fillers_byRace$region <- factor(fillers_byRace$region, levels=c("precritical_2",
                                                                     "precritical_1",
                                                                     "crit",
                                                                     "spillover_1",
                                                                     "spillover_2"))

#sample graphs
#residualized/centered reading times by nameCategory
ggplot(fillers_byRace, aes(x=region, y=mean_resid, color=nameCategory, group = nameCategory)) + 
  geom_point()  + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_line() +
  labs(x='Sentence region', y='Residualized/Centered RT (ms)', fill='Type') +
  theme(axis.text.x = element_text(angle = 30))

#and also by item
ggplot(fillers_byTrial, aes(x=region, y=mean_resid, color = nameCategory, group = nameCategory)) + 
  facet_grid(~trial_id) +
  geom_point()  + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_line() +
  labs(x='Sentence region', y='Residualized/Centered RT (ms)', fill='Type') +
  theme(axis.text.x = element_text(angle = 30))

