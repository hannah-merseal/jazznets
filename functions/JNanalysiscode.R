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
library(mgcv)

JNmaster <- read.csv("data/master_info_excludes/master.csv")
survey <- read.csv("data/JNSurvey.csv")

#pull relevant survey information for analysis, rename for consistency
survey <- survey %>% dplyr::select(prolificID, musicianYN, EmotionalContent, Contour, Dissonance, 
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

master <- master %>%
  filter(between(RT, mean(RT, na.rm=TRUE) - (2.5 * sd(RT,na.rm=TRUE)),
                 mean(RT, na.rm=TRUE) + (2.5* sd(RT, na.rm = TRUE))))

##### PREDICTORS #####
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
logtable <-  data.frame(observed = masterNo20$response,
                        predicted = ifelse(fitted(log.modelA) > .5, 1, 0))
xtabs(~ observed + predicted, data = logtable)

### RESPONSE WITHOUT 20 MUSICIANS ###
#musician interaction (which is NS) + musician variables
musiciansNo20 <- masterNo20 %>% dplyr::filter(musicianYN == 1)
musician.null <- glm(response ~ distance, data = musiciansNo20, family = "binomial")
musician.int <- glm(response ~ distance +
                      primaryProficiency + impProficiency + playHoursNow + impHoursNow + 
                      percentImp, data = musiciansNo20, family = "binomial")
summary(musician.int)
anova(musician.null, musician.int, test = "Chisq")
exp(cbind(OR = coef(musician.int), confint(musician.int)))

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
  scale_color_manual(labels = c("Nonmusicians", "Musicians"), values = c("#00A08A", "#FD6467")) + 
  guides(color = guide_legend("Group"))+
  theme_minimal()
respPlot10

#cool logistic plot
glm_predicted <- predictvals()
logPlot <- ggplot(master10, aes(x = distance, y = as.factor(response))) 


  stat_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Distance",
       y = "Response") +
  scale_color_manual(name = "Legend",
                       breaks = c("#FD6467", "#00A08A"),
                       labels = c("Musicians", "Nonmusicians"),
                       guide = "legend") +
  scale_x_continuous(limits = c(0,10), breaks = c(1:10)) +
  scale_y_continuous(limits = c(0,1))
logPlot

##### RT #####

### masterCorrect with 20 == 0
masterCorrectLow <- master %>% dplyr::filter(distance < 5, response == 1)
masterCorrectHigh <- master %>% dplyr::filter(distance >= 5, response == 0)
masterCorrect <- merge(masterCorrectLow, masterCorrectHigh, all = TRUE)
masterCorrectNo20 <- masterCorrect %>% dplyr::filter(distance < 20)

#omnibus
modelc.RT <- lm(RT ~ 1, data = masterCorrectNo20)
mcSummary(modelc.RT)
model1.RT20No <- lm(RT ~ distance + musicianYN + hoursWeekListen + hoursWeekListenJazz, data = masterCorrectNo20)
mcSummary(model1.RT20No)
anova(modelc.RT, model1.RT20No)
modelCompare(modelc.RT, model1.RT20No)

#RT not including 20, quadratic model

masterCorrectNo20$distancesq <- masterCorrectNo20$distance*masterCorrectNo20$distance
quadModel.RT <- lm(RT ~ distance + distancesq + musicianYN + hoursWeekListen + hoursWeekListenJazz, data = masterCorrectNo20)
mcSummary(quadModel.RT)
anova(modelc.RT, quadModel.RT)
modelCompare(modelc.RT, quadModel.RT)
#compare to linear - quadratic model is definitely a better fit :)
anova(model1.RT20No, quadModel.RT)
modelCompare(model1.RT20No, quadModel.RT)
#quadratic plot - try long format table with gather https://stackoverflow.com/questions/42764028/fitting-a-quadratic-curve-in-ggplot

masterCorrectNo20$musicianYN[masterCorrectNo20$musicianYN==0] <- "Non-musicians"
masterCorrectNo20$musicianYN[masterCorrectNo20$musicianYN==1] <- "Musicians"

quadPlot.RT <- ggplot(masterCorrectNo20, aes(x = distance, y = RT)) +
  theme_minimal() +   ggtitle("Reaction Time by Distance & Group") +
  ylab("Reaction Time (seconds)") + scale_x_continuous("Distance", c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  geom_jitter(alpha = .3, color = "gray50", pch = 21, size = 1) +
  stat_smooth(method = 'lm', formula = y ~ poly(x, 2), aes(color = musicianYN, fill = musicianYN)) +
  scale_color_manual(name = "Group", values = c("#FD6467", "#00A08A")) +
  scale_fill_manual(name = "Group", values = c("#FD6467", "#00A08A")) +
  guides(fill = guide_legend(override.aes = list(color = c("#FD6467", "#00A08A"), size = 1)))
quadPlot.RT
#add key & recolor geom ribbon


#GAM (bad)
jazz_gam <- gam(RT ~ s(distance, k = 2) + s(musicianYN) + s(hoursWeekListen) + s(hoursWeekListenJazz), data = masterCorrectNo20, method = 'REML')
summary(jazz_gam)
gam.check(jazz_gam)


#distance*musician intx (NS)
# musicianRT <- masterCorr %>% dplyr::filter(musicianYN == 1)
# musicianRT.lm <- lm(RT ~ distance, data = musicianRT)
# mcSummary(musicianRT.lm)
# 
# nonRT <- masterCorr %>% dplyr::filter(musicianYN == 0)
# nonRT.lm <- lm(RT ~ distance, data = nonRT)
# mcSummary(nonRT.lm)

### masterCorrect with 20 == 1
masterYes <- master %>% dplyr::filter(distance %in% c(1, 2, 3, 4, 20), response == 1)
masterNo <- master %>% dplyr::filter(distance %in% c(6, 10), response == 0)
masterCorrect20 <- merge(masterYes, masterNo, all = TRUE)
masterCorrect20 <- merge(masterCorrect20, survey)

modelc.RT <- lm(RT ~ 1, data = masterCorrect20)
mcSummary(modelc.RT)
model1.RT20Yes <- lm(RT ~ distance*musicianYN + hoursWeekListen + hoursWeekListenJazz, data = masterCorrect20)
mcSummary(model1.RT20Yes)
anova(modelc.RT, model1.RT20Yes)
modelCompare(modelc.RT, model1.RT20Yes)

### RT 20 == 1 split 1-4
masterCorrect20.firsthalf <- masterCorrect20 %>% dplyr::filter(distance %in% c(1, 2, 3, 4))
modelc.firsthalf <- lm(RT ~ 1, data = masterCorrect20.firsthalf)
modela.firsthalf <- lm(RT ~ distance*musicianYN + hoursWeekListen + hoursWeekListenJazz, data = masterCorrect20.firsthalf)
anova(modelc.firsthalf, modela.firsthalf)
modelCompare(modelc.firsthalf, modela.firsthalf)
mcSummary(modela.firsthalf)

### RT 20 == 1 split 4-20
masterCorrect20.secondhalf <- masterCorrect20 %>% dplyr::filter(distance %in% c(4, 6, 10, 20))
modelc.secondhalf <- lm(RT ~ 1, data = masterCorrect20.secondhalf)
modela.secondhalf <- lm(RT ~ distance*musicianYN + hoursWeekListen + hoursWeekListenJazz, data = masterCorrect20.secondhalf)
anova(modelc.secondhalf, modela.secondhalf)
modelCompare(modelc.secondhalf, modela.secondhalf)
mcSummary(modela.secondhalf)

##### RT PLOTS #####
#ggplot for RT (No 20, quad)
library(effects)
predicted.values <- effect('distance*musicianYN', quadModel.RT,
                           xlevels = list(distance = c(1, 2, 3, 4, 6, 10),
                                          musicianYN = c(0, 1)),
                           se = TRUE, confidence.level = .95, typical = mean)
predicted.values = as.data.frame(predicted.values)
predicted.values$distance.factor <- factor(predicted.values$distance,
                                           levels = c(1, 2, 3, 4, 6, 10))
predicted.values$musicianYN.factor <- factor(predicted.values$musicianYN,
                                             levels = c(0, 1),
                                             labels = c("Non-musicians", "Musicians"))
RTplot.quad <- ggplot(predicted.values, aes(x = distance, y = fit)) +
  geom_jitter(data = masterCorrectNo20, aes(x = distance, y = RT), alpha = .3, color = "gray50", pch = 21, size = 1) +
  ylab("Reaction Time (seconds)") + scale_x_continuous("Distance", c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = musicianYN.factor), alpha = 0.2) +
  geom_smooth(aes(x = distance, y = fit, color = musicianYN.factor, method = "lm"), size = 1.25, show.legend = FALSE) +
  ylim(0, 3) + ggtitle("Reaction Time by Distance and Group") +
  theme_bw() + scale_fill_discrete(name = "Group")
RTplot.quad

#ggplot for RT (20 Yes)
predicted.values <- effect('distance*musicianYN', model1.RT20Yes,
                           xlevels = list(distance = c(1, 2, 3, 4, 6, 10, 20),
                                          musicianYN = c(0, 1)),
                           se = TRUE, confidence.level = .95, typical = mean)
predicted.values = as.data.frame(predicted.values)
predicted.values$distance.factor <- factor(predicted.values$distance,
                                           levels = c(1, 2, 3, 4, 6, 10, 20))
predicted.values$musicianYN.factor <- factor(predicted.values$musicianYN,
                                             levels = c(0, 1),
                                             labels = c("Non-musicians", "Musicians"))
RTplot20Yes <- ggplot(predicted.values, aes(x = distance, y = fit)) +
  geom_jitter(data = masterCorr, aes(x = distance, y = RT), alpha = .3, color = "gray50", pch = 21, size = 1) +
  ylab("Reaction Time (seconds)") + scale_x_continuous("Distance", c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = musicianYN.factor), alpha = 0.2) +
  geom_line(aes(x = distance, y = fit, color = musicianYN.factor), size = 1.25, show.legend = FALSE) +
  ylim(0, 3) + ggtitle("Reaction Time by Distance and Group (20 = Related)") +
  theme_bw() + scale_fill_discrete(name = "Group")
RTplot20Yes

# 20 related 1-4
predicted.values <- effect('distance*musicianYN', modela.firsthalf,
                           xlevels = list(distance = c(1, 2, 3, 4),
                                          musicianYN = c(0, 1)),
                           se = TRUE, confidence.level = .95, typical = mean)
predicted.values = as.data.frame(predicted.values)
predicted.values$distance.factor <- factor(predicted.values$distance,
                                           levels = c(1, 2, 3, 4))
predicted.values$musicianYN.factor <- factor(predicted.values$musicianYN,
                                             levels = c(0, 1),
                                             labels = c("Non-musicians", "Musicians"))
RTplot20FirstHalf <- ggplot(predicted.values, aes(x = distance, y = fit)) +
  geom_jitter(data = masterCorrect20.firsthalf, aes(x = distance, y = RT), alpha = .3, color = "gray50", pch = 21, size = 1) +
  ylab("Reaction Time (seconds)") + scale_x_continuous("Distance", c(1, 2, 3, 4)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = musicianYN.factor), alpha = 0.2) +
  geom_line(aes(x = distance, y = fit, color = musicianYN.factor), size = 1.25, show.legend = FALSE) +
  ylim(0, 3) + ggtitle("Reaction Time by Distance and Group (Distances 1-4)") +
  theme_bw() + scale_fill_discrete(name = "Group") + xlim(1, 4)
RTplot20FirstHalf

#20 related more than 4
predicted.values <- effect('distance*musicianYN', modela.secondhalf,
                           xlevels = list(distance = c(4, 6, 10, 20),
                                          musicianYN = c(0, 1)),
                           se = TRUE, confidence.level = .95, typical = mean)
predicted.values = as.data.frame(predicted.values)
predicted.values$distance.factor <- factor(predicted.values$distance,
                                           levels = c(4, 6, 10, 20))
predicted.values$musicianYN.factor <- factor(predicted.values$musicianYN,
                                             levels = c(0, 1),
                                             labels = c("Non-musicians", "Musicians"))
RTplot20SecondHalf <- ggplot(predicted.values, aes(x = distance, y = fit)) +
  geom_jitter(data = masterCorrect20.secondhalf, aes(x = distance, y = RT), alpha = .3, color = "gray50", pch = 21, size = 1) +
  ylab("Reaction Time (seconds)") + scale_x_continuous("Distance", c(4, 6, 10, 20)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = musicianYN.factor), alpha = 0.2) +
  geom_line(aes(x = distance, y = fit, color = musicianYN.factor), size = 1.25, show.legend = FALSE) +
  ylim(0, 3) + ggtitle("Reaction Time by Distance and Group (Distances 4, 6, 10, 20)") +
  theme_bw() + scale_fill_discrete(name = "Group") + xlim(4, 20)
RTplot20SecondHalf