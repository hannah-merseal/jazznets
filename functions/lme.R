library(tidyverse)
library(psych)
library(car)
library(nlme)
library(ez)
library(emmeans)
library(lme4)

JNmaster <- read.csv("pilot data/master_info_excludes/master.csv")
surveyAll <- read.csv("pilot data/JNPilotSurvey.csv")

survey <- surveyAll %>% dplyr::select(participant, musicianYN, EmotionalContent, Contour, Dissonance, 
                                      IntervalVariety, IntervalSize, ImpliedHarmony, HoursWeekListen, 
                                      HoursWeekListenJazz, InstrumentYN, YearsFormalTraining, 
                                      PrimaryProficiency, YearsFormalImp, ImpProficiency, 
                                      YearsTheory, PlayHoursNow, ImpHoursNow, PercentImp)


#up through distance 4 these overlap
#between 5 and 6 no overlap - there is a difference!
#add standard error to ggplots
#just trials that are related

#just trials that are unrelated - 5 and up (coincides with no overlap)

#classical v jazz v non

#listening hours as covariate

#music expertise as covariate

#lme4 package!



#what distances are definitely related?
#what happens after distance = 10?