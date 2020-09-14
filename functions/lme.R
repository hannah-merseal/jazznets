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

#lme for response:
# overall listening hours - hoursWeekListen W
# jazz listening hours - hoursWeekListenJazz W
# primary instrument proficiency - primaryProficiency normal W*
# improvisation proficiency - impProficiency W
# playing hours - playHoursNow W
# improv hours - impHoursNow W*
# percent improv played - percentImp W
lmm.basic <- lmer(response ~ distance*musicianYN + (1 | participant), data = master)
summary(lmm.basic)

#
#do I want musician to be another random effect instead of this? 
#below: the above music effects are only collected for the musician set - just run these on the musician model?

#create musician and non sets
musicians <- master %>% dplyr::filter(musicianYN == 1)
nonmusicians <- master %>% dplyr::filter(musicianYN == 0)

#musician
lmm.mus <- lmer(response ~ distance + (1|participant), data = musicians)
summary(lmm.mus)

#non
lmm.non <- lmer(response ~ distance + (1|participant), data = nonmusicians)
summary(lmm.non)

#then do it again for RT

#up through distance 4 the stimuli overlap
#between 5 and 6 no overlap - there is a difference!
#for 1, 2, 3, 4 - remove trials with response = 0
#for 6, 10 - remove trials with response = 1
#THEN look at RT

#ggplot for response 
#ggplot for RT (just correct trials)