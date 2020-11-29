##### LIBRARIES & CREATE MASTER #####
# pull libraries and create master dataframe
library(tidyverse)
library(psych)
library(car)
library(nlme)
library(ez)
library(emmeans)
library(lme4)
library(lmerTest)
source('http://psych.colorado.edu/~jclab/R/mcSummaryLm.R')
library(lmSupport)

JNmaster <- read.csv("data/master_info_excludes/master.csv")
surveyAll <- read.csv("data/JNSurvey.csv")

#pull relevant survey information for analysis, rename for consistency
survey <- surveyAll %>% dplyr::select(prolificID, musicianYN, EmotionalContent, Contour, Dissonance, 
                                      IntervalVariety, IntervalSize, ImpliedHarmony, HoursWeekListen, 
                                      HoursWeekListenJazz, InstrumentYN, YearsFormalTraining, 
                                      PrimaryProficiency, YearsFormalImp, ImpProficiency, 
                                      YearsTheory, PlayHoursNow, ImpHoursNow, PercentImp) %>%
  rename(emotionalContent = EmotionalContent,
         contour = Contour,
         dissonance = Dissonance,
         intervalVariety = IntervalVariety,
         intervalSize = IntervalSize,
         impliedHarmony = ImpliedHarmony,
         hoursWeekListen = HoursWeekListen,
         hoursWeekListenJazz = HoursWeekListenJazz,
         instrumentYN = InstrumentYN,
         yearsFormalTraining = YearsFormalTraining,
         primaryProficiency = PrimaryProficiency,
         yearsFormalImp = YearsFormalImp,
         impProficiency = ImpProficiency,
         yearsTheory = YearsTheory,
         playHoursNow = PlayHoursNow,
         impHoursNow = ImpHoursNow,
         percentImp = PercentImp)

#MERGE
master <- merge(JNmaster, survey)

# music predictors:
# overall listening hours - hoursWeekListen W
# jazz listening hours - hoursWeekListenJazz W
# primary instrument proficiency - primaryProficiency normal W*
# improvisation proficiency - impProficiency W
# playing hours - playHoursNow W
# improv hours - impHoursNow W*
# percent improv played - percentImp W



##### RESPONSE #####

### RESPONSE WITH 20 ###
#logistic regression with chisq WITH 20
log.modelC <- glm(response ~ 1, data = master, family = "binomial")
summary(log.modelC)
log.modelA <- glm(response ~ distance*musicianYN + hoursWeekListen + hoursWeekListenJazz, data = master, family = "binomial")
summary(log.modelA)
anova(log.modelC, log.modelA, test = "Chisq")

#converting log odds into odds ratio
exp(cbind(OR = coef(log.modelA), confint(log.modelA)))

#confusion table
logtable <-  data.frame(observed = master$response,
                        predicted = ifelse(fitted(log.modelA) > .5, 1, 0))
xtabs(~ observed + predicted, data = logtable)



### RESPONSE WITHOUT 20 ###
#logistic regression with chisq WITHOUT 20
masterNo20 <- master %>% dplyr::filter(distance != 20)
log.modelC <- glm(response ~ 1, data = masterNo20, family = "binomial")
summary(log.modelC)
log.modelA <- glm(response ~ distance*musicianYN + hoursWeekListen + hoursWeekListenJazz, data = masterNo20, family = "binomial")
summary(log.modelA)
anova(log.modelC, log.modelA, test = "Chisq")

#converting log odds into odds ratio
exp(cbind(OR = coef(log.modelA), confint(log.modelA)))

#confusion table
logtable <-  data.frame(observed = master$response,
                        predicted = ifelse(fitted(log.modelA) > .5, 1, 0))
xtabs(~ observed + predicted, data = logtable)



### RESPONSE JUST 10 & 20 ###
master1020 <- master %>% dplyr::filter(distance %in% c(10, 20))

#logistic regression with chisq JUST 10 & 20
log.modelC <- glm(response ~ 1, data = master1020, family = "binomial")
summary(log.modelC)
log.modelA <- glm(response ~ distance*musicianYN + hoursWeekListen + hoursWeekListenJazz, data = master1020, family = "binomial")
summary(log.modelA)
anova(log.modelC, log.modelA, test = "Chisq")

#converting log odds into odds ratio
exp(cbind(OR = coef(log.modelA), confint(log.modelA)))

#confusion table
logtable <-  data.frame(observed = master$response,
                        predicted = ifelse(fitted(log.modelA) > .5, 1, 0))
xtabs(~ observed + predicted, data = logtable)



### RESPONSE WITHOUT 20 MUSICIANS/NON ###
#musician interaction (which is NS) + musician variables
musicians <- masterNo20 %>% dplyr::filter(musicianYN == 1)
musician.int <- glm(response ~ distance + hoursWeekListen + hoursWeekListenJazz + 
                      primaryProficiency + impProficiency +
                      playHoursNow + impHoursNow + percentImp, data = musicians, family = "binomial")
summary(musician.int)
exp(cbind(OR = coef(musician.int), confint(musician.int)))

#nonmusician interaction (NS)
nonmusicians <- masterNo20 %>% dplyr::filter(musicianYN == 0)
non.int <- glm(response ~ distance, data = nonmusicians, family = "binomial")
summary(non.int)
exp(cbind(OR = coef(non.int), confint(non.int)))



### RESPONSE 10-20 MUSICIANS ###
musicians <- master1020 %>% dplyr::filter(musicianYN == 1)
musician.int <- glm(response ~ distance + hoursWeekListen + hoursWeekListenJazz + 
                      primaryProficiency + impProficiency +
                      playHoursNow + impHoursNow + percentImp, data = musicians, family = "binomial")
summary(musician.int)
exp(cbind(OR = coef(musician.int), confint(musician.int)))

#nonmusician interaction (NS)
nonmusicians <- master1020 %>% dplyr::filter(musicianYN == 0)
non.int <- glm(response ~ distance, data = nonmusicians, family = "binomial")
summary(non.int)
exp(cbind(OR = coef(non.int), confint(non.int)))

##### RESPONSE PLOTS #####

#ggplot for response (+ error)
respPlot <- ggplot(master,
                   aes(x = as.factor(distance),
                       y = response,
                       group = musicianYN,
                       color = as.factor(musicianYN))) +
  stat_summary(fun.y = "mean", geom = "point") +
  stat_summary(fun.y = "mean", geom = "line") +
  stat_summary(fun.data = "mean_se", geom = "errorbar") +
  labs(x = "Distance",
       y = "Mean Response",
       title = "Mean Response for Distance (With 20), Musicians vs. Nons") +
  scale_color_manual(labels = c("Nonmusicians", "Musicians"), values = c("blue", "red")) +
  guides(color = guide_legend("Group"))+
  theme_bw()
respPlot

#response but without 20
master10 <- master %>% dplyr::filter(distance != 20)

log.modelC10 <- glm(response ~ 1, data = master10, family = "binomial")
summary(log.modelC10)
log.modelA10 <- glm(response ~ distance*musicianYN + hoursWeekListen + hoursWeekListenJazz, data = master10, family = "binomial")
summary(log.modelA10)
anova(log.modelC10, log.modelA10, test = "Chisq")

respPlot10 <- ggplot(master10,
                     aes(x = as.factor(distance),
                         y = response,
                         group = musicianYN,
                         color = as.factor(musicianYN))) +
  stat_summary(fun.y = "mean", geom = "point") +
  stat_summary(fun.y = "mean", geom = "line") +
  stat_summary(fun.data = "mean_se", geom = "errorbar") +
  labs(x = "Distance",
       y = "Mean Response",
       title = "Mean Response for Distance (No 20), Musicians vs. Nons") +
  scale_color_manual(labels = c("Nonmusicians", "Musicians"), values = c("blue", "red")) +
  guides(color = guide_legend("Group"))+
  theme_bw()
respPlot10

##### RT #####

#regression for RT with only correct trials
masterCorr <- read.csv("data/master_info_excludes/correctTrials.csv")
masterCorr <- merge(masterCorr, survey)

#omnibus
modelc.RT <- lm(RT ~ 1, data = masterCorr)
mcSummary(modelc.RT)
model1.RT <- lm(RT ~ distance*musicianYN + hoursWeekListen + hoursWeekListenJazz, data = masterCorr)
mcSummary(model1.RT)
anova(modelc.RT, model1.RT)
modelCompare(modelc.RT, model1.RT)

#distance*musician intx
musicianRT <- masterCorr %>% dplyr::filter(musicianYN == 1)
musicianRT.lm <- lm(RT ~ distance, data = musicianRT)
mcSummary(musicianRT.lm)

nonRT <- masterCorr %>% dplyr::filter(musicianYN == 0)
nonRT.lm <- lm(RT ~ distance, data = nonRT)
mcSummary(nonRT.lm)

##### RT PLOTS #####
#ggplot for RT (just correct trials)
library(effects)
predicted.values <- effect('distance*musicianYN', model1.RT,
                           xlevels = list(distance = c(1, 2, 3, 4, 6, 10, 20),
                                          musicianYN = c(0, 1)),
                           se = TRUE, confidence.level = .95, typical = mean)
predicted.values = as.data.frame(predicted.values)
predicted.values$distance.factor <- factor(predicted.values$distance,
                                           levels = c(1, 2, 3, 4, 6, 10, 20))
predicted.values$musicianYN.factor <- factor(predicted.values$musicianYN,
                                             levels = c(0, 1),
                                             labels = c("Non-musicians", "Musicians"))
RTplot <- ggplot(predicted.values, aes(x = distance, y = fit)) +
  geom_jitter(data = masterCorr, aes(x = distance, y = RT), alpha = .3, color = "gray50", pch = 21, size = 1) +
  ylab("Reaction Time (seconds)") + scale_x_continuous("Distance", c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = musicianYN.factor), alpha = 0.2) +
  geom_line(aes(x = distance, y = fit, color = musicianYN.factor), size = 1.25, show.legend = FALSE) +
  ylim(0, 3) + ggtitle("Reaction Time by Distance and Group (Correct Trials)") +
  theme_bw() + scale_fill_discrete(name = "Group")
RTplot