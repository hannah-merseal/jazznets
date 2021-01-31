#libraries
library(tidyverse)
library(plyr)
library(dplyr)

###DON'T USE THIS IF THE FILE NAMES ARE RIGHT OR YOU'LL HAVE TO REDO EVERYTHING###
#file rename to new participant IDs
#JNfilerename()

#cleans up from psychopy raw format into tidy raw format
JNtidy()

#filters bad-RT trials, saves agreement for repeat trials, means for each distance
JNstats()

#load info
info <- read.csv("data/master_info_excludes/info.csv")

#merge all participant files to one big dataframe
list_of_files <- list.files(path = "~/hannah-merseal/jazznets/data/filtered",
                            full.names = TRUE)
master <- list_of_files %>%
  setNames(nm = .) %>%
  map_df(~read_csv(.x, col_types = cols(), col_names = TRUE), .id = "file_name")

master <- master %>% dplyr::select(participant, trial, distance, stimNumber, response, RT)

#pilot - for participants with <.50 on repeat agreement from info, delete from means and master
master <- merge(master, info, by = "participant")
#excludedAgreement <- master %>% dplyr::filter(repeatAgree < .5)
#write.csv(excludedAgreement, "excludedAgreement.csv")

#also manually exclude participants who were bots
# master <- master %>% dplyr::filter(repeatAgree > .5) %>%
#   filter(prolificID != "5ee4e82b8e523a1fea7856c8") %>%
#   filter(prolificID != "5ecee8199d86e91500183985") %>%
#   select(participant, trial, distance, stimNumber, response, RT, NRemoved, repeatAgree)

#exclusions
master <- master %>% dplyr::filter(participant != "JN_24") %>%
  filter(participant != "JN_43") %>%
  filter(participant != "JN_47") %>%
  select(prolificID, participant, trial, distance, stimNumber, response, RT)

write.csv(master, "master.csv")

survey <- read.csv("data/JNSurvey.csv")
participantInfo <- merge(survey, info, py = "prolificID")
participantInfo <- participantInfo %>% dplyr::filter(participant != "JN_24") %>%
  filter(participant != "JN_43") %>%
  filter(participant != "JN_47")
write.csv(participantInfo, "participants.csv")

#find means & SD for each distance
distance1 <- master %>% dplyr::filter(distance == 1)
distance1.corr <- distance1 %>% dplyr::filter(response == 1)
meanRT.distance1 <- mean(distance1.corr$RT)
SDRT.distance1 <- sd(distance1.corr$RT)
meanResponse.distance1 <- mean(distance1$response)
SDResponse.distance1 <- sd(distance1$response)

distance2 <- master %>% dplyr::filter(distance == 2)
distance2.corr <- distance2 %>% dplyr::filter(response == 1)
meanRT.distance2 <- mean(distance2.corr$RT)
SDRT.distance2 <- sd(distance2.corr$RT)
meanResponse.distance2 <- mean(distance2$response)
SDResponse.distance2 <- sd(distance2$response)

distance3 <- master %>% dplyr::filter(distance == 3)
distance3.corr <- distance3 %>% dplyr::filter(response == 1)
meanRT.distance3 <- mean(distance3.corr$RT)
SDRT.distance3 <- sd(distance3.corr$RT)
meanResponse.distance3 <- mean(distance3$response)
SDResponse.distance3 <- sd(distance3$response)

distance4 <- master %>% dplyr::filter(distance == 4)
distance4.corr <- distance4 %>% dplyr::filter(response == 1)
meanRT.distance4 <- mean(distance4.corr$RT)
SDRT.distance4 <- sd(distance4.corr$RT)
meanResponse.distance4 <- mean(distance4$response)
SDResponse.distance4 <- sd(distance4$response)

distance6 <- master %>% dplyr::filter(distance == 6)
distance6.corr <- distance6 %>% dplyr::filter(response == 0)
meanRT.distance6 <- mean(distance6.corr$RT)
SDRT.distance6 <- sd(distance6.corr$RT)
meanResponse.distance6 <- mean(distance6$response)
SDResponse.distance6 <- sd(distance6$response)

distance10 <- master %>% dplyr::filter(distance == 10)
distance10.corr <- distance10 %>% dplyr::filter(response == 0)
meanRT.distance10 <- mean(distance10.corr$RT)
SDRT.distance10 <- sd(distance10.corr$RT)
meanResponse.distance10 <- mean(distance10$response)
SDResponse.distance10 <- sd(distance10$response)

distance20 <- master %>% dplyr::filter(distance == 20)
distance20.corr <- distance20 %>% dplyr::filter(response == 0)
meanRT.distance20 <- mean(distance20.corr$RT)
SDRT.distance20 <- sd(distance20.corr$RT)
meanResponse.distance20 <- mean(distance20$response)
SDResponse.distance20 <- sd(distance20$response)

means <- data.frame(meanRT.distance1, meanRT.distance2, meanRT.distance3, meanRT.distance4, meanRT.distance6, meanRT.distance10, meanRT.distance20, 
                    meanResponse.distance1, meanResponse.distance2, meanResponse.distance3, meanResponse.distance4, meanResponse.distance6, meanResponse.distance10, meanResponse.distance20)
write.csv(means, "means_all_20Yes.csv")
SD <- data.frame(SDRT.distance1,SDRT.distance2,SDRT.distance3,SDRT.distance4,SDRT.distance6,SDRT.distance10,SDRT.distance20,
                 SDResponse.distance1,SDResponse.distance2,SDResponse.distance3,SDResponse.distance4,SDResponse.distance6,SDResponse.distance10,SDResponse.distance20)
write.csv(SD, "SD_all_20Yes.csv")

#calculating % related
pctunrelated1 <- (nrow(distance1) - nrow(distance1.corr))/nrow(distance1)
pctunrelated2 <- (nrow(distance2) - nrow(distance2.corr))/nrow(distance2)
pctunrelated3 <- (nrow(distance3) - nrow(distance3.corr))/nrow(distance3)
pctunrelated4 <- (nrow(distance4) - nrow(distance4.corr))/nrow(distance4)
pctrelated6 <- (nrow(distance6) - nrow(distance6.corr))/nrow(distance6)
pctrelated10 <- (nrow(distance10) - nrow(distance10.corr))/nrow(distance10)
pctrelated20 <- (nrow(distance20) - nrow(distance20.corr))/nrow(distance20)

#remove response = 0 for distance = 1, save to .csv
distance1 <- distance1 %>% dplyr::filter(response == 1)
master_filtered1 <- master %>% dplyr::filter(distance > 1)
master_filtered1 <- rbind(distance1, master_filtered1)
write.csv(master_filtered1, "master_filtered1.csv")

#add musician data & repeat
#musicians = play an instrument NOW
JNSurvey <- read.csv("data/JNSurvey.csv")
musicMaster <- merge(master, JNSurvey, by = "prolificID")
musicians <- musicMaster %>% dplyr::filter(musicianYN == 1) %>%
  select(participant, trial, distance, stimNumber, response, RT, musicianYN)

distance1 <- musicians %>% dplyr::filter(distance == 1)
distance1.corr <- distance1 %>% dplyr::filter(response == 1)
meanRT.distance1 <- mean(distance1.corr$RT)
SDRT.distance1 <- sd(distance1.corr$RT)
meanResponse.distance1 <- mean(distance1$response)
SDResponse.distance1 <- sd(distance1$response)

distance2 <- musicians %>% dplyr::filter(distance == 2)
distance2.corr <- distance2 %>% dplyr::filter(response == 1)
meanRT.distance2 <- mean(distance2.corr$RT)
SDRT.distance2 <- sd(distance2.corr$RT)
meanResponse.distance2 <- mean(distance2$response)
SDResponse.distance2 <- sd(distance2$response)

distance3 <- musicians %>% dplyr::filter(distance == 3)
distance3.corr <- distance3 %>% dplyr::filter(response == 1)
meanRT.distance3 <- mean(distance3.corr$RT)
SDRT.distance3 <- sd(distance3.corr$RT)
meanResponse.distance3 <- mean(distance3$response)
SDResponse.distance3 <- sd(distance3$response)

distance4 <- musicians %>% dplyr::filter(distance == 4)
distance4.corr <- distance4 %>% dplyr::filter(response == 1)
meanRT.distance4 <- mean(distance4.corr$RT)
SDRT.distance4 <- sd(distance4.corr$RT)
meanResponse.distance4 <- mean(distance4$response)
SDResponse.distance4 <- sd(distance4$response)

distance6 <- musicians %>% dplyr::filter(distance == 6)
distance6.corr <- distance6 %>% dplyr::filter(response == 0)
meanRT.distance6 <- mean(distance6.corr$RT)
SDRT.distance6 <- sd(distance6.corr$RT)
meanResponse.distance6 <- mean(distance6$response)
SDResponse.distance6 <- sd(distance6$response)

distance10 <- musicians %>% dplyr::filter(distance == 10)
distance10.corr <- distance10 %>% dplyr::filter(response == 0)
meanRT.distance10 <- mean(distance10.corr$RT)
SDRT.distance10 <- sd(distance10.corr$RT)
meanResponse.distance10 <- mean(distance10$response)
SDResponse.distance10 <- sd(distance10$response)

distance20 <- musicians %>% dplyr::filter(distance == 20)
distance20.corr <- distance20 %>% dplyr::filter(response == 0)
meanRT.distance20 <- mean(distance20.corr$RT)
SDRT.distance20 <- sd(distance20.corr$RT)
meanResponse.distance20 <- mean(distance20$response)
SDResponse.distance20 <- sd(distance20$response)

meansMus <- data.frame(meanRT.distance1, meanRT.distance2, meanRT.distance3, meanRT.distance4, meanRT.distance6, meanRT.distance10, meanRT.distance20, 
                    meanResponse.distance1, meanResponse.distance2, meanResponse.distance3, meanResponse.distance4, meanResponse.distance6, meanResponse.distance10, meanResponse.distance20)
write.csv(meansMus, "means_musicians_20Yes.csv")
SDMus <- data.frame(SDRT.distance1,SDRT.distance2,SDRT.distance3,SDRT.distance4,SDRT.distance6,SDRT.distance10,SDRT.distance20,
                 SDResponse.distance1,SDResponse.distance2,SDResponse.distance3,SDResponse.distance4,SDResponse.distance6,SDResponse.distance10,SDResponse.distance20)
write.csv(SDMus, "SD_musicians_20Yes.csv")

#nonmusicians
nonmusicians <- musicMaster %>% dplyr::filter(musicianYN == 0) %>%
  select(participant, trial, distance, stimNumber, response, RT, musicianYN)

distance1 <- nonmusicians %>% dplyr::filter(distance == 1)
distance1.corr <- distance1 %>% dplyr::filter(response == 1)
meanRT.distance1 <- mean(distance1.corr$RT)
SDRT.distance1 <- sd(distance1.corr$RT)
meanResponse.distance1 <- mean(distance1$response)
SDResponse.distance1 <- sd(distance1$response)

distance2 <- nonmusicians %>% dplyr::filter(distance == 2)
distance2.corr <- distance2 %>% dplyr::filter(response == 1)
meanRT.distance2 <- mean(distance2.corr$RT)
SDRT.distance2 <- sd(distance2.corr$RT)
meanResponse.distance2 <- mean(distance2$response)
SDResponse.distance2 <- sd(distance2$response)

distance3 <- nonmusicians %>% dplyr::filter(distance == 3)
distance3.corr <- distance3 %>% dplyr::filter(response == 1)
meanRT.distance3 <- mean(distance3.corr$RT)
SDRT.distance3 <- sd(distance3.corr$RT)
meanResponse.distance3 <- mean(distance3$response)
SDResponse.distance3 <- sd(distance3$response)

distance4 <- nonmusicians %>% dplyr::filter(distance == 4)
distance4.corr <- distance4 %>% dplyr::filter(response == 1)
meanRT.distance4 <- mean(distance4.corr$RT)
SDRT.distance4 <- sd(distance4.corr$RT)
meanResponse.distance4 <- mean(distance4$response)
SDResponse.distance4 <- sd(distance4$response)

distance6 <- nonmusicians %>% dplyr::filter(distance == 6)
distance6.corr <- distance6 %>% dplyr::filter(response == 0)
meanRT.distance6 <- mean(distance6.corr$RT)
SDRT.distance6 <- sd(distance6.corr$RT)
meanResponse.distance6 <- mean(distance6$response)
SDResponse.distance6 <- sd(distance6$response)

distance10 <- nonmusicians %>% dplyr::filter(distance == 10)
distance10.corr <- distance10 %>% dplyr::filter(response == 0)
meanRT.distance10 <- mean(distance10.corr$RT)
SDRT.distance10 <- sd(distance10.corr$RT)
meanResponse.distance10 <- mean(distance10$response)
SDResponse.distance10 <- sd(distance10$response)

distance20 <- nonmusicians %>% dplyr::filter(distance == 20)
distance20.corr <- distance20 %>% dplyr::filter(response == 1)
meanRT.distance20 <- mean(distance20.corr$RT)
SDRT.distance20 <- sd(distance20.corr$RT)
meanResponse.distance20 <- mean(distance20$response)
SDResponse.distance20 <- sd(distance20$response)

meansNon <- data.frame(meanRT.distance1, meanRT.distance2, meanRT.distance3, meanRT.distance4, meanRT.distance6, meanRT.distance10, meanRT.distance20, 
                       meanResponse.distance1, meanResponse.distance2, meanResponse.distance3, meanResponse.distance4, meanResponse.distance6, meanResponse.distance10, meanResponse.distance20)
write.csv(meansNon, "means_nonmusicians_20Yes.csv")
SDNon <- data.frame(SDRT.distance1,SDRT.distance2,SDRT.distance3,SDRT.distance4,SDRT.distance6,SDRT.distance10,SDRT.distance20,
                    SDResponse.distance1,SDResponse.distance2,SDResponse.distance3,SDResponse.distance4,SDResponse.distance6,SDResponse.distance10,SDResponse.distance20)
write.csv(SDNon, "SD_nonmusicians_20Yes.csv")

#to do: clean up the above