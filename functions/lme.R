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

#lme covariates for response:
# overall listening hours
# jazz listening hours
# instrument
# instrument proficiency
# improvisation proficiency
# playing hours
# imp hours
# percent imp
lmm.kitchensink <- lmer(response ~ distance*musicianYN + (1 | participant), data = master)
summary(lmm.kitchensink)

#create musician and non sets
musicians <- master %>% dplyr::filter(musicianYN == 1)
nonmusicians <- master %>% dplyr::filter(musicianYN == 0)





#then do it again for RT

#up through distance 4 the stimuli overlap
#between 5 and 6 no overlap - there is a difference!
#for 1, 2, 3, 4 - remove trials with response = 0
#for 6, 10 - remove trials with response = 1
#THEN look at RT

#ggplot for response (don't forget error bars)
#ggplot for RT (just correct trials)