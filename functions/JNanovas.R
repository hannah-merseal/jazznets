library(tidyverse)
library(psych)
library(car)
library(nlme)
library(ez)
library(emmeans)

meansAll <- read.csv("pilot data/means/means_all.csv")
meansMusicians <- read.csv("pilot data/means/means_musicians.csv")
meansNon <- read.csv("pilot data/means/means_nonmusicians.csv")
meansNon <- meansNon %>% dplyr::rename(meanResponse = meanDistance)
JNmaster <- read.csv("pilot data/master_info_excludes/master.csv")
info <- read.csv("pilot data/master_info_excludes/info.csv")

JNmaster <- merge(JNmaster, info, by = "participant") 
JNmaster.meanRes <- aggregate(JNmaster$response, 
                              by = list(JNmaster$participant,
                                        JNmaster$distance,
                                        JNmaster$musician),
                              FUN = 'mean')
colnames(JNmaster.meanRes) <- c("participant", "distance", "musician", "response")
JNmaster.meanRes <- JNmaster.meanRes[order(JNmaster.meanRes$participant), ]
JNmaster.meanRes$distance <- as.factor(JNmaster.meanRes$distance)
JNmaster.meanRes$musician <- as.factor(JNmaster.meanRes$musician)
JNmaster.aov <- with(JNmaster.meanRes,
                   aov(response ~ distance + musician + distance*musician +
                         Error(participant/(distance*musician))))
summary(JNmaster.aov)

pttRes <- with(JNmaster.meanRes,
               pairwise.t.test(response, distance, p.adjust.method = "bonferroni", paired = TRUE))
pttRes

JNmaster.meanRT <- aggregate(JNmaster$RT, 
                              by = list(JNmaster$participant,
                                        JNmaster$distance,
                                        JNmaster$musician),
                              FUN = 'mean')
colnames(JNmaster.meanRT) <- c("participant", "distance", "musician", "RT")
JNmaster.meanRT <- JNmaster.meanRT[order(JNmaster.meanRT$participant), ]
JNmaster.meanRT$distance <- as.factor(JNmaster.meanRT$distance)
JNmaster.meanRT$musician <- as.factor(JNmaster.meanRT$musician)
JNmasterRT.aov <- with(JNmaster.meanRT,
                     aov(RT ~ distance + musician + distance*musician +
                           Error(participant/(distance*musician))))
summary(JNmasterRT.aov)

#initial pass - see lme for next steps