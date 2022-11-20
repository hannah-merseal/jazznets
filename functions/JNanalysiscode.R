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
#library(lmSupport)
library(mgcv)

JNmaster <- read.csv("data/master_info_excludes/master.csv")
survey <- read.csv("data/JNSurvey.csv")

All20No <- read.csv("data/means/means_all_20No.csv")
Musicians20No <- read.csv("data/means/means_musicians_20No.csv")
Nonmusicians20No <- read.csv("data/means/means_nonmusicians_20No.csv")
All20Yes <- read.csv("data/means/means_all_20Yes.csv")
Musicians20Yes <- read.csv("data/means/means_musicians_20Yes.csv")
Nonmusicians20Yes <- read.csv("data/means/means_nonmusicians_20Yes.csv")

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

##### FIGURE 2 #####
pitch <- read.csv("figs/WJDpitch.csv")
Fig2Hist <- pitch %>%
  ggplot(aes(x = degree)) +
  geom_histogram(binwidth = 1, fill = "#90D4CC", color="#e9ecef") +
  scale_y_log10() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 20)) +
  annotation_logticks(sides = "l") +
  xlab("Degree") +
  ylab("Frequency")

##### RESPONSE #####
### RESPONSE WITH 20 ###
#logistic regression with chisq WITH 20
log.modelC <- glm(response ~ 1, data = master, family = "binomial")
summary(log.modelC)
log.modelA <- glm(response ~ distance*musicianYN, data = master, family = "binomial")
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
log.modelA <- glm(response ~ distance*musicianYN, data = masterNo20, family = "binomial")
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

##### FIGURE 6 #####
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

#cool logistic plot - not using
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
quadModel.RT <- lm(RT ~ distance + distancesq + musicianYN, data = masterCorrectNo20)
mcSummary(quadModel.RT)
anova(modelc.RT, quadModel.RT)
modelCompare(modelc.RT, quadModel.RT)
#compare to linear - quadratic model is definitely a better fit :)
anova(model1.RT20No, quadModel.RT)
modelCompare(model1.RT20No, quadModel.RT)

##### FIGURE 6 #####

pd <- position_dodge(0.1)
pdd <- position_dodge(0.2)

Figure6A <- ggplot() + theme_minimal() +
  geom_line(data = Musicians20No, aes(x = distance, y = meanRT, color = "#FD6467")) +
  geom_errorbar(data = Musicians20No, aes(x = distance, ymin = meanRT - SERT, ymax = meanRT + SERT), inherit.aes = FALSE, color = "#FD6467") +
  geom_line(data = Nonmusicians20No, aes(x = distance, y = meanRT, color = "#00A08A")) +
  geom_errorbar(data = Nonmusicians20No, aes(x = distance, ymin = meanRT - SERT, ymax = meanRT + SERT), inherit.aes = FALSE, color = "#00A08A", position = pdd) +
  geom_ribbon() +
  labs(x = "Distance",
       y = "Mean Reaction Time (seconds)",
       colour = "Legend") +
  scale_color_identity(name = "Legend",
                       breaks = c("#FD6467", "#00A08A"),
                       labels = c("Musicians", "Nonmusicians"),
                       guide = "legend") +
  scale_x_continuous(limits = c(0,10.5), breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 20))

masterCorrectNo20$musicianYN[masterCorrectNo20$musicianYN==0] <- "Nonmusicians"
masterCorrectNo20$musicianYN[masterCorrectNo20$musicianYN==1] <- "Musicians"

Figure6B <- ggplot(masterCorrectNo20, aes(x = distance, y = RT)) +
  ylab("Reaction Time (seconds)") + scale_x_continuous("Distance", c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  stat_smooth(method = 'lm', formula = y ~ poly(x, 2), aes(color = musicianYN, fill = musicianYN)) +
  scale_color_manual(values = c("#FD6467", "#00A08A")) +
  scale_fill_manual(values = c("#FD6467", "#00A08A")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 20), legend.position = "none")

library(patchwork)

Figure6 <- (Figure6A/Figure6B) + plot_annotation(tag_levels = 'A') &
  theme(plot.tag.position = 'topright')

##### OLD FIGURE 7 #####

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
quadPlot.RT <- ggplot(predicted.values, aes(x = distance, y = fit)) +
  geom_jitter(data = masterCorrectNo20, aes(x = distance, y = RT), alpha = .3, color = "gray50", pch = 21, size = 1) +
  ylab("Reaction Time (seconds)") + scale_x_continuous("Distance", c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = musicianYN.factor), alpha = 0.2) +
  geom_smooth(aes(x = distance, y = fit, color = musicianYN.factor, method = "lm"), size = 1.25, show.legend = FALSE) +
  ylim(0, 3) + ggtitle("Reaction Time by Distance and Group") +
  theme_bw() + scale_fill_discrete(name = "Group")
