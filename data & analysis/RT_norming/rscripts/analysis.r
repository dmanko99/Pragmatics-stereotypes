library(tidyverse)
library(lme4)
library(lmerTest)
library(ggplot2)
library(bootstrap)
library(tm)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# HELPER SCRIPTS
dodge = position_dodge(.9)
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  mean(x,na.rm=na.rm) - quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) - mean(x,na.rm=na.rm)}

# Import dataframe
#df <- read_csv("../data/RT_mainNorming/tesRT-trials.csv") %>%
df <- read_csv("../data/crimeRTnorming/tes_crimeRT-trials.csv") %>%
  # remove bot check slides
  filter(!(type == "bot_check")) %>%
  # remove the practice trial
  filter(!(type == "practice")) %>%
  # remove non-relevant cols
  select( -c(audio, image, response, slide_number, slide_type, error)) %>%
  drop_na()

subinfo = read.csv("../data/crimeRTnorming/tes_crimeRT-subject_information.csv", header = TRUE)

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
  filter(nWrong > 5))$workerid

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
## region names
regionLabels <- c("pre_crit_2",
                  "pre_crit_1",
                  "crit",
                  "spillover_1",
                  "spillover_2",
                  "spillover_3",
                  "spillover_4")

## For criticals
df <- df %>%
  mutate(region_old = region) %>%
  mutate(region = case_when((((lag(region_old) == "filler") & (lead(region_old == "filler"))) &
                               (region_old != "name") & (region_old != "crit"))
                            ~ "filler",
                            region_old == "pre_crit_2" ~ "pre_crit_2",
                            region_old == "pre_crit_1" ~ "pre_crit_1",
                            region_old == "crit" ~ "crit",
                            region_old == "spillover_1" ~ "spillover_1",
                            region_old == "spillover_2" ~ "spillover_2",
                            region_old == "spillover_3" ~ "spillover_3",
                            region_old == "spillover_4" ~ "spillover_4",
                            region_old == "name" ~ "name",
                            region_old == "filler" ~ "filler",
                            TRUE ~ "other")) %>%
  select(-region_old)

#for non-criminality ones
df <- df %>%
  mutate(region_old = region) %>%
  mutate(region = case_when(lead(region_old) == "crit" ~ "precritical_1",
                            lag(region_old) == "crit" ~ "spillover_1",
                            region_old == "crit" ~ "crit",
                            TRUE ~ "other")) %>%
  mutate(region_old = region) %>%
  mutate(region = case_when(region_old == "precritical_1" ~ "precritical_1",
                            region_old == "spillover_1" ~ "spillover_1",
                            region_old == "crit" ~ "crit",
                            lead(region_old) == "precritical_1" ~ "precritical_2",
                            lag(region_old) == "spillover_1" ~ "spillover_2",
                            TRUE ~ "other")) %>%
  select(-region_old)

## For fillers
fillerdf <- filter(df, type == "filler") %>%
  mutate(region_old = region) %>%
  mutate(region = case_when(((lead(region_old) == "crit") & (lead(trial_id) == trial_id) &
                               (lead(workerid) == workerid) & (type == "filler"))
                            ~ "pre_crit_1",
                            ((lag(region_old) == "crit") & (lag(trial_id) == trial_id) &
                               (lag(workerid) == workerid) & (type == "filler"))
                            ~ "spillover_1",
                            region_old == "crit" ~ "crit",
                            region_old == "name" ~ "name",
                            TRUE ~ "other")) %>%
  mutate(region_old = region) %>%
  mutate(region = case_when(region_old == "pre_crit_1" ~ "pre_crit_1",
                            region_old == "spillover_1" ~ "spillover_1",
                            region_old == "crit" ~ "crit",
                            region_old == "name" ~ "name",
                            ((lead(region_old) == "pre_crit_1") & (lead(trial_id) == trial_id) &
                               (lead(workerid) == workerid) & (type == "filler"))
                            ~ "pre_crit_2",
                            ((lag(region_old) == "spillover_1") & (lag(trial_id) == trial_id) &
                               (lag(workerid) == workerid) & (type == "filler"))
                            ~ "spillover_2",
                            region_old == "pre_crit_2" ~ "pre_crit_2",
                            region_old == "pre_crit_1" ~ "pre_crit_1",
                            region_old == "crit" ~ "crit",
                            region_old == "spillover_1" ~ "spillover_1",
                            region_old == "spillover_2" ~ "spillover_2",
                            region_old == "name" ~ "name",
                            region_old == "filler" ~ "filler",
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
criticaldf$ctr_crim = as.numeric(factor(criticaldf$bias)) - mean(as.numeric(factor(criticaldf$bias)))



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


spilloverdf <- filter(df, region %in% c("spillover_1", "spillover_2"), type == "critical")
spilloverdf$bias <- factor(spilloverdf$bias)
spilloverdf$nameCategory <- factor(spilloverdf$nameCategory)
spilloverdf <- spilloverdf %>%
  mutate(consistency = case_when(bias == nameCategory ~ "consistent", TRUE ~ "inconsistent"))

spilloverdf$ctr_nameCategory = as.numeric(spilloverdf$nameCategory) - mean(as.numeric(spilloverdf$nameCategory))
spilloverdf$ctr_consistency = as.numeric(factor(spilloverdf$consistency)) - mean(as.numeric(factor(spilloverdf$consistency)))
spilloverdf$ctr_crim = as.numeric(factor(spilloverdf$bias)) - mean(as.numeric(factor(spilloverdf$bias)))

model_spil = lmer(residuals ~ logtrial * ctr_nameCategory * ctr_consistency + 
                     (1 | trial_id) + 
                     (1 | workerid), 
                   spilloverdf, REML = F)
summary(model_spil)

## models looking at criminality instead of consistency
criminalityModel_crit <- lmer(residuals ~ logtrial * ctr_nameCategory * ctr_crim + 
                                (1 | trial_id) + 
                                (1 | workerid), 
                              criticaldf, REML = F)
summary(criminalityModel_crit)

criminalityModel_crit2 <- lmer(residuals ~ logtrial * ctr_nameCategory * ctr_crim + 
                                (1 + ctr_nameCategory | trial_id) + 
                                (1 + (ctr_nameCategory * ctr_crim) | workerid), 
                              criticaldf, REML = F)
summary(criminalityModel_crit2)


criminalityModel_spill <- lmer(residuals ~ logtrial * ctr_nameCategory * ctr_crim + 
                                (1 | trial_id) + 
                                (1 | workerid), 
                              spilloverdf, REML = F)
summary(criminalityModel_spill)

criminalityModel_spill2 <- lmer(residuals ~ logtrial * ctr_nameCategory * ctr_crim + 
                                  (1 + ctr_nameCategory | trial_id) + 
                                  (1 + (ctr_nameCategory * ctr_crim) | workerid), 
                               spilloverdf, REML = F)
summary(criminalityModel_spill2)
## Looking at all regions of interest (namely pre-crit, crit, and spillover)
subregion_df <- filter(df, region %in% c("pre_crit_2", "pre_crit_1",
                                         "crit", "spillover_1", "spillover_2",
                                         "spillover_3", "spillover_4"),
                       type == "critical")
#ugh just for non-crim
subregion_df <- filter(df, region %in% c("precritical_2", "precritical_1",
                                         "crit", "spillover_1", "spillover_2"),
                       #"spillover_3", "spillover_4"),
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

rawgrouped$region <- factor(rawgrouped$region, levels=c("pre_crit_2",
                                                        "pre_crit_1",
                                                        "crit",
                                                        "spillover_1",
                                                        "spillover_2",
                                                        "spillover_3",
                                                        "spillover_4"))

residuals_grouped = subregion_df %>%
  group_by(region, consistency, nameCategory, trial_id) %>% 
  summarize(res_RT = resid(lmer(log(rt) ~ nchar(as.character(form)) + (0+(nchar(as.character(form)))|workerid), data = subregion_df)),
            CILow=ci.low(res_RT), CIHigh=ci.high(res_RT)) %>% 
  ungroup() %>% 
  mutate(YMin=res_RT-CILow,YMax=res_RT+CIHigh)

#by trial
residuals_grouped2_bytrial = subregion_df %>%
  group_by(region, consistency, nameCategory, trial_id) %>% 
  summarize(mean_resid = mean(residuals), CILow=ci.low(residuals), CIHigh=ci.high(residuals)) %>% 
  ungroup() %>% 
  mutate(YMin=mean_resid-CILow,YMax=mean_resid+CIHigh)


residuals_grouped2_bytrial$region <- factor(residuals_grouped2_bytrial$region,
                                            levels=c("pre_crit_2",
                                                     "pre_crit_1",
                                                     "crit",
                                                     "spillover_1",
                                                     "spillover_2",
                                                     "spillover_3",
                                                     "spillover_4"))

residuals_grouped2_byname = subregion_df %>%
  group_by(region, consistency, nameCategory) %>% 
  summarize(mean_resid = mean(residuals), CILow=ci.low(residuals), CIHigh=ci.high(residuals)) %>% 
  ungroup() %>% 
  mutate(YMin=mean_resid-CILow,YMax=mean_resid+CIHigh)


residuals_grouped2_byname$region <- factor(residuals_grouped2_byname$region,
                                           levels=c("pre_crit_2",
                                                    "pre_crit_1",
                                                    "crit",
                                                    "spillover_1",
                                                    "spillover_2",
                                                    "spillover_3",
                                                    "spillover_4"))
residuals_grouped2_byname$nameCategory <- recode(residuals_grouped2_byname$nameCategory, 
                                                 "black" = "Black-normed Names",
                                                 "white" = "White-normed Names")


residuals_wbias_byname = subregion_df %>%
  group_by(region, bias, nameCategory) %>% 
  summarize(mean_resid = mean(residuals), CILow=ci.low(residuals), CIHigh=ci.high(residuals)) %>% 
  ungroup() %>% 
  mutate(YMin=mean_resid-CILow,YMax=mean_resid+CIHigh)

residuals_wbias_byname$region <- factor(residuals_wbias_byname$region)
#for non-crim
residuals_wbias_byname$region <- factor(residuals_wbias_byname$region, 
                                        levels=c("precritical_2",
                                                 "precritical_1",
                                                 "crit",
                                                 "spillover_1",
                                                 "spillover_2"))
#for crim
residuals_wbias_byname$region <- factor(residuals_wbias_byname$region, 
                                 levels=c("pre_crit_2",
                                          "pre_crit_1",
                                          "crit",
                                          "spillover_1",
                                          "spillover_2",
                                          "spillover_3",
                                          "spillover_4"))
residuals_wbias_byname$bias <- recode(residuals_wbias_byname$bias, "black" = "Evoking Criminality",
                                                                "white" = "Not Evoking Criminality")

residuals_wbias_bytrial = subregion_df %>%
  group_by(region, bias, nameCategory, trial_id) %>% 
  summarize(mean_resid = mean(residuals), CILow=ci.low(residuals), CIHigh=ci.high(residuals)) %>% 
  ungroup() %>% 
  mutate(YMin=mean_resid-CILow,YMax=mean_resid+CIHigh)

residuals_wbias_bytrial$region <- factor(residuals_wbias_bytrial$region)
residuals_wbias_bytrial$region <- factor(residuals_wbias_bytrial$region, 
                                 levels=c("pre_crit_2",
                                          "pre_crit_1",
                                          "crit",
                                          "spillover_1",
                                          "spillover_2"))
                                          #"spillover_3",
                                          #"spillover_4"))
residuals_wbias_bytrial$bias <- recode(residuals_wbias_bytrial$bias, "black" = "black-typical",
                                                        "white" = "white-typical")


#raw reading times by item and consistency
ggplot(rawgrouped, aes(x=region, y=mean_rt, color=consistency, group=consistency)) + 
  facet_grid(nameCategory~trial_id) +
  geom_point()  + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_line() +
  labs(x='Sentence region', y='Mean raw RT (ms)', fill='Type') +
  theme(axis.text.x = element_text(angle = 30))

#residualized/centered reading times by consistency
ggplot(residuals_grouped2_byname, aes(x=region, y=mean_resid, color=consistency, group=consistency)) + 
  facet_wrap(~nameCategory) +
  geom_point()  + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_line() +
  scale_colour_manual("Stereotype\nConsistency", values = c("#00397A","#CCCCCC")) +
  labs(x = "Sentence Region", y = "Residualized/Centered RT (ms)", fill = "Type")  +
  ggtitle("Mean Reading Times for Black- and White-typical Completions") +
  theme(title = element_text(size=18), legend.text = element_text(size=15),
        axis.text = element_text(angle = 30, size = 12),
        strip.text = element_text(size=12))

#and also by item
ggplot(residuals_grouped2_bytrial, aes(x=region, y=mean_resid, color=consistency, group=consistency)) + 
  facet_grid(nameCategory~trial_id) +
  geom_point()  + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_line() +
  labs(x='Sentence region', y='Residualized/Centered RT (ms)', fill='Type') +
  theme(axis.text.x = element_text(angle = 30))


#looking at same-item data -- by nameCategory
ggplot(residuals_wbias_byname, aes(x=region, y=mean_resid, color=nameCategory, group=nameCategory)) + 
  facet_wrap(~bias) +
  geom_point()  + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_line() +
  scale_colour_manual("Name Bias", values = c("#00397A","#CCCCCC")) +
  labs(x = "Sentence Region", y = "Residualized/Centered RT (ms)", fill = "Type")  +
  #scale_x_discrete(labels = c("White", "Black")) +
  ggtitle("Mean Reading Times for Black- and White-typical Completions") +
  theme(title = element_text(size=18), legend.text = element_text(size=15),
        axis.text = element_text(angle = 30, size = 12),
        strip.text = element_text(size=12))
## and now by trial id
ggplot(filter(residuals_wbias_bytrial, bias == "black-typical"), 
       aes(x=region, y=mean_resid, color=nameCategory, group=nameCategory)) + 
  facet_wrap(trial_id~bias) +
  geom_point()  + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_line() +
  labs(x='Sentence region', y='Residualized/Centered RT (ms)', fill='Type') +
  theme(axis.text.x = element_text(angle = 30))
ggplot(filter(residuals_wbias_bytrial, bias == "white-typical"), 
       aes(x=region, y=mean_resid, color=nameCategory, group=nameCategory)) + 
  facet_wrap(trial_id~bias) +
  geom_point()  + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_line() +
  labs(x='Sentence region', y='Residualized/Centered RT (ms)', fill='Type') +
  theme(axis.text.x = element_text(angle = 30))


ggplot(filter(df, type == "critical"), aes(x=trial_no, y=rt, col=type)) + 
  geom_point(alpha=.2)  + 
  geom_smooth() + 
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
fillerdf <- filter(fillerdf, region %in% regionLabels)

fillerdf$bias <- factor(fillerdf$bias)
fillerdf$nameCategory <- factor(fillerdf$nameCategory)

fillerdf$ctr_nameCategory = as.numeric(fillerdf$nameCategory) - 
  mean(as.numeric(fillerdf$nameCategory))
#fillerdf$ctr_consistency = as.numeric(factor(fillerdf$consistency)) - 
#  mean(as.numeric(factor(fillerdf$consistency)))

## getting residuals & length-correcting
fillerdf$logrt = log(fillerdf$rt)
fillerdf$wordlen = nchar(as.character(fillerdf$form))
fillerdf$logtrial = log(fillerdf$trial_no + 1)
filler_resid_model <- lmer(logrt ~ wordlen + (0+wordlen|workerid), data = fillerdf)
filler_res <- resid(filler_resid_model)
fillerdf$residuals <- filler_res

##centering RTs
fillerdf$ctr_RT = as.numeric(fillerdf$residuals) - mean(fillerdf$residuals)

fillers_byTrial <- fillerdf%>%
  group_by(region, nameCategory, trial_id) %>% 
  summarize(mean_resid = mean(residuals), CILow=ci.low(residuals), CIHigh=ci.high(residuals)) %>% 
  ungroup() %>% 
  mutate(YMin=mean_resid-CILow,YMax=mean_resid+CIHigh)


fillers_byTrial$region <- factor(fillers_byTrial$region, levels = regionLabels)


fillers_byRace <- fillerdf%>%
  group_by(region, nameCategory) %>% 
  summarize(mean_resid = mean(residuals), CILow=ci.low(residuals), CIHigh=ci.high(residuals)) %>% 
  ungroup() %>% 
  mutate(YMin=mean_resid-CILow,YMax=mean_resid+CIHigh)


fillers_byRace$region <- factor(fillers_byRace$region, levels= regionLabels)

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


# Looking at ordering effects
subregion_df <- subregion_df %>%
  mutate(quarter = case_when((trial_no <= 4) ~ "firstQuarter",
                             (trial_no >= 13) ~ "lastQuarter",
                             TRUE ~ "middleHalf")) %>%
  mutate(half = case_when((trial_no < 9) ~ "firstHalf",
                          (trial_no > 8) ~ "secondHalf",
                          TRUE ~ "other"))

#by quarter
residuals_orderEffects_Q = subregion_df %>%
  group_by(region, bias, nameCategory, quarter) %>% 
  summarize(mean_resid = mean(residuals), CILow=ci.low(residuals), CIHigh=ci.high(residuals)) %>% 
  ungroup() %>% 
  mutate(YMin=mean_resid-CILow,YMax=mean_resid+CIHigh)
residuals_orderEffects_Q$region <- factor(residuals_orderEffects_Q$region)
residuals_orderEffects_Q$region <- factor(residuals_orderEffects_Q$region, 
                                         levels=regionLabels)
residuals_orderEffects_Q$bias <- recode(residuals_orderEffects_Q$bias, "black" = "black-typical",
                                       "white" = "white-typical")

ggplot(residuals_orderEffects_Q, aes(x=region, y=mean_resid, color=nameCategory, 
                                     group=nameCategory)) + 
  facet_grid(quarter~bias) +
  geom_point()  + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_line() +
  labs(x='Sentence region', y='Residualized/Centered RT (ms)', fill='Type') +
  theme(axis.text.x = element_text(angle = 30))

#by half
residuals_orderEffects_H = subregion_df %>%
  group_by(region, bias, nameCategory, half) %>% 
  summarize(mean_resid = mean(residuals), CILow=ci.low(residuals), CIHigh=ci.high(residuals)) %>% 
  ungroup() %>% 
  mutate(YMin=mean_resid-CILow,YMax=mean_resid+CIHigh)
residuals_orderEffects_H$region <- factor(residuals_orderEffects_H$region)
residuals_orderEffects_H$region <- factor(residuals_orderEffects_H$region, 
                                          levels=regionLabels)
residuals_orderEffects_H$bias <- recode(residuals_orderEffects_H$bias, "black" = "black-typical",
                                        "white" = "white-typical")

ggplot(residuals_orderEffects_H, aes(x=region, y=mean_resid, color=nameCategory, 
                                     group=nameCategory)) + 
  facet_grid(half~bias) +
  geom_point()  + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_line() +
  labs(x='Sentence region', y='Residualized/Centered RT (ms)', fill='Type') +
  theme(axis.text.x = element_text(angle = 30))


#looking at sub-populations of participants
subinfo <- subinfo %>%
  filter(!(workerid %in% excludeList)) %>%
  mutate(age = as.numeric(subinfo$age)) %>%
  mutate(affiliation = as.numeric(subinfo$affiliation))

### political affiliation
subinfo <- subinfo %>%
  mutate(affiliationGroup = case_when((affiliation > -1) & (affiliation < 3) ~ "liberal",
                                      (affiliation == 3) ~ "independent",
                                      (affiliation > 3) ~ "conservative",
                                      TRUE ~ "no response"))
liberalIDs <- filter(subinfo, affiliationGroup == "liberal")$workerid
independentIDs <- filter(subinfo, affiliationGroup == "independent")$workerid
conservativeIDs <- filter(subinfo, affiliationGroup == "conservative")$workerid
noAffIDs <- filter(subinfo, affiliationGroup == "no response")$workerid

demographics_df <- subregion_df %>%
  mutate(affiliationGroup = case_when((workerid %in% liberalIDs) ~ "liberal",
                                      (workerid %in% independentIDs) ~ "independent",
                                      (workerid %in% conservativeIDs) ~ "conservative",
                                      (workerid %in% noAffIDs) ~ "no response",
                                      TRUE ~ "other"))
residuals_affiliation = demographics_df %>%
  group_by(region, bias, nameCategory, affiliationGroup) %>% 
  summarize(mean_resid = mean(residuals), CILow=ci.low(residuals), CIHigh=ci.high(residuals)) %>% 
  ungroup() %>% 
  mutate(YMin=mean_resid-CILow,YMax=mean_resid+CIHigh)
residuals_affiliation$region <- factor(residuals_affiliation$region)
residuals_affiliation$region <- factor(residuals_affiliation$region, 
                                          levels=regionLabels)
residuals_affiliation$bias <- recode(residuals_affiliation$bias, "black" = "black-typical",
                                        "white" = "white-typical")

ggplot(residuals_affiliation, aes(x=region, y=mean_resid, color=nameCategory, 
                                     group=nameCategory)) + 
  facet_grid(affiliationGroup~bias) +
  geom_point()  + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_line() +
  labs(x='Sentence region', y='Residualized/Centered RT (ms)', fill='Type') +
  theme(axis.text.x = element_text(angle = 30))

### race
subinfo <- subinfo %>%
  mutate(raceGroup = case_when(((race == "AA") | (race == "AA, White") | (race == "AA, Latino")
                                | (race == "Asian, AA, White") | (race == "AA, Latino, White"))
                                ~ "AA",
                               ((race == "Latino") | (race == "Latino, White")) ~ "Latinx",
                               ((race == "Asian") | (race == "Asian, White")) ~ "Asian",
                               (race == "White") ~ "White",
                               TRUE ~ "not reported"))

AA_ids <- filter(subinfo, raceGroup == "AA")$workerid
Latinx_ids <- filter(subinfo, raceGroup == "Latinx")$workerid
Asian_ids <- filter(subinfo, raceGroup == "Asian")$workerid
White_ids <- filter(subinfo, raceGroup == "White")$workerid

demographics_df <- demographics_df %>%
  mutate(raceGroup = case_when((workerid %in% AA_ids) ~ "AA",
                                      (workerid %in% Latinx_ids) ~ "Latinx",
                                      (workerid %in% Asian_ids) ~ "Asian",
                                      (workerid %in% White_ids) ~ "White",
                                      TRUE ~ "not reported"))

residuals_race = demographics_df %>%
  group_by(region, bias, nameCategory, raceGroup) %>% 
  summarize(mean_resid = mean(residuals), CILow=ci.low(residuals), CIHigh=ci.high(residuals)) %>% 
  ungroup() %>% 
  mutate(YMin=mean_resid-CILow,YMax=mean_resid+CIHigh)
residuals_race$region <- factor(residuals_race$region)
residuals_race$region <- factor(residuals_race$region, 
                                       levels=regionLabels)
residuals_race$bias <- recode(residuals_race$bias, "black" = "black-typical",
                                     "white" = "white-typical")

ggplot(residuals_race, aes(x=region, y=mean_resid, color=nameCategory, 
                                  group=nameCategory)) + 
  facet_grid(raceGroup~bias) +
  geom_point()  + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_line() +
  labs(x='Sentence region', y='Residualized/Centered RT (ms)', fill='Type') +
  theme(axis.text.x = element_text(angle = 30))

ggplot(filter(residuals_race, raceGroup == "White"), aes(x=region, y=mean_resid, color=nameCategory, 
                           group=nameCategory)) + 
  facet_grid(~bias) +
  geom_point()  + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_line() +
  labs(x='Sentence region', y='Residualized/Centered RT (ms)', fill='Type') +
  theme(axis.text.x = element_text(angle = 30))

### age
averageAge <- mean(subinfo$age, na.rm = TRUE)
subinfo$ageGroup2 <- as.numeric(cut2(as.numeric(subinfo$age), g=3))
subinfo <- subinfo %>%
  mutate(ageGroup = case_when((ageGroup2 == 1) ~ "younger third",
                              (ageGroup2 == 2) ~ "middle third",
                              (ageGroup2 == 3) ~ "older third",
                              TRUE ~ "other"))
olderAgeIDs <- filter(subinfo, ageGroup == "older third")$workerid
middleAgeIDs <- filter(subinfo, ageGroup == "middle third")$workerid
youngerAgeIDs <- filter(subinfo, ageGroup == "younger third")$workerid

demographics_df <- demographics_df %>%
  mutate(ageGroup = case_when((workerid %in% olderAgeIDs) ~ "older third",
                              (workerid %in% middleAgeIDs) ~ "middle third",
                              (workerid %in% youngerAgeIDs) ~ "younger third",
                              TRUE ~ "other"))

residuals_age = demographics_df %>%
  group_by(region, bias, nameCategory, ageGroup) %>% 
  summarise(mean_resid = mean(residuals), CILow=ci.low(residuals), CIHigh=ci.high(residuals)) %>% 
  ungroup() %>% 
  mutate(YMin=mean_resid-CILow,YMax=mean_resid+CIHigh)
residuals_age$region <- factor(residuals_age$region)
residuals_age$region <- factor(residuals_age$region, 
                                       levels=regionLabels)
residuals_age$bias <- recode(residuals_age$bias, "black" = "black-typical",
                                     "white" = "white-typical")

ggplot(residuals_age, aes(x=region, y=mean_resid, color=nameCategory, 
                                  group=nameCategory)) + 
  facet_grid(ageGroup~bias) +
  geom_point()  + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_line() +
  labs(x='Sentence region', y='Residualized/Centered RT (ms)', fill='Type') +
  theme(axis.text.x = element_text(angle = 30))

###raw RTs
residuals_age_raw = demographics_df %>%
  group_by(region, bias, nameCategory, ageGroup) %>% 
  summarise(meanRT = mean(rt), CILow=ci.low(rt), CIHigh=ci.high(rt)) %>% 
  ungroup() %>% 
  mutate(YMin=meanRT-CILow,YMax=meanRT+CIHigh)
residuals_age_raw$region <- factor(residuals_age_raw$region)
residuals_age_raw$region <- factor(residuals_age_raw$region, 
                               levels=regionLabels)
residuals_age_raw$bias <- recode(residuals_age_raw$bias, "black" = "black-typical",
                             "white" = "white-typical")

ggplot(residuals_age_raw, aes(x=region, y=meanRT, color=nameCategory, 
                          group=nameCategory)) + 
  facet_grid(ageGroup~bias) +
  geom_point()  + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_line() +
  labs(x='Sentence region', y='Residualized/Centered RT (ms)', fill='Type') +
  theme(axis.text.x = element_text(angle = 30))

## looking at the name region instead of other regions
name_region_df <- filter(df, region == "name", type == "critical")

name_region_df$bias <- factor(name_region_df$bias)
name_region_df$nameCategory <- factor(name_region_df$nameCategory)
name_region_df <- name_region_df %>%
  mutate(consistency = case_when(bias == nameCategory ~ "consistent", TRUE ~ "inconsistent"))

name_region_df$ctr_nameCategory = as.numeric(name_region_df$nameCategory) - 
  mean(as.numeric(name_region_df$nameCategory))
name_region_df$ctr_consistency = as.numeric(factor(name_region_df$consistency)) - 
  mean(as.numeric(factor(name_region_df$consistency)))

name_RT_byBias = name_region_df %>%
  group_by(region, bias, nameCategory) %>% 
  summarize(mean_resid = mean(residuals), CILow=ci.low(residuals), CIHigh=ci.high(residuals)) %>% 
  ungroup() %>% 
  mutate(YMin=mean_resid-CILow,YMax=mean_resid+CIHigh)

name_RT_byBias$bias <- recode(name_RT_byBias$bias, "black" = "black-typical",
                                       "white" = "white-typical")

name_RT_byBias_raw = name_region_df %>%
  group_by(region, bias, nameCategory) %>% 
  summarize(meanRT = mean(rt), CILow=ci.low(rt), CIHigh=ci.high(rt)) %>% 
  ungroup() %>% 
  mutate(YMin=meanRT-CILow,YMax=meanRT+CIHigh)

name_RT_byBias_raw$bias <- recode(name_RT_byBias_raw$bias, "black" = "black-typical",
                              "white" = "white-typical")

ggplot(name_RT_byBias, aes(x=region, y=mean_resid, fill=nameCategory)) + 
  facet_wrap(~bias) +
  geom_bar(stat = "identity", position = "dodge")  + 
  labs(x='Sentence region', y='Residualized/Centered RT (ms)', fill='Name Category') +
  theme(axis.text.x = element_text(angle = 30))

ggplot(name_RT_byBias_raw, aes(x=region, y=meanRT, fill=nameCategory)) + 
  facet_wrap(~bias) +
  geom_bar(stat = "identity", position = "dodge")  + 
  labs(x='Sentence region', y='Raw RT (ms)', fill='Name Category') +
  theme(axis.text.x = element_text(angle = 30))
