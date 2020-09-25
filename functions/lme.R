library(tidyverse)
library(psych)
library(car)
library(nlme)
library(ez)
library(emmeans)
library(lme4)
library(lmerTest)

JNmaster <- read.csv("pilot data/master_info_excludes/master.csv")
surveyAll <- read.csv("pilot data/JNPilotSurvey.csv")

#pull relevant survey information for analysis, rename for consistency
survey <- surveyAll %>% dplyr::select(participant, musicianYN, EmotionalContent, Contour, Dissonance, 
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

# predictors for response:
# overall listening hours - hoursWeekListen W
# jazz listening hours - hoursWeekListenJazz W
# primary instrument proficiency - primaryProficiency normal W*
# improvisation proficiency - impProficiency W
# playing hours - playHoursNow W
# improv hours - impHoursNow W*
# percent improv played - percentImp W
#lmm.basic <- lmer(response ~ distance*musicianYN + impHoursNow + (1 | participant), data = master)
#summary(lmm.basic)

#logistic regression with chisq
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

#musician interaction + musician variables
musicians <- master %>% dplyr::filter(musicianYN == 1)
musician.int <- glm(response ~ distance, data = musicians, family = "binomial")
summary(musician.int)
exp(cbind(OR = coef(musician.int), confint(musician.int)))

#nonmusician interaction
nonmusicians <- master %>% dplyr::filter(musicianYN == 0)
non.int <- glm(response ~ distance, data = nonmusicians, family = "binomial")
summary(non.int)
exp(cbind(OR = coef(non.int), confint(non.int)))

#then do it again for RT

#up through distance 4 the stimuli overlap
#between 5 and 6 no overlap - there is a difference!
#for 1, 2, 3, 4 - remove trials with response = 0
#for 6, 10 - remove trials with response = 1
#THEN look at RT

#ggplot for response (+ error)
#ggplot for RT (just correct trials)