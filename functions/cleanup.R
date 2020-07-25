#libraries
library(tidyverse)
library(plyr)
source("~/hannah-merseal/jazznets/functions/JNfilerename.R")
source("~/hannah-merseal/jazznets/functions/JNtidy.R")
source("~/hannah-merseal/jazznets/functions/JNstats.R")
source("~/hannah-merseal/jazznets/functions/JNmaster.R")

###DON'T USE THIS IF THE FILE NAMES ARE RIGHT OR YOU'LL HAVE TO REDO EVERYTHING###
#file rename to new participant IDs
#JNfilerename()

#cleans up from psychopy raw format into tidy raw format
JNtidy()

#filters bad-RT trials, saves agreement for repeat trials, means for each distance
JNstats()

#load info, means
info <- read.csv("pilot data/master_info_excludes/info.csv")
means <- read.csv("means.csv")
#merge all participant files to one big dataframe
list_of_files <- list.files(path = "~/hannah-merseal/jazznets/pilot data/filtered",
                            full.names = TRUE)
master <- list_of_files %>%
  setNames(nm = .) %>%
  map_df(~read_csv(.x, col_types = cols(), col_names = TRUE), .id = "file_name")

master <- master %>% dplyr::select(participant, trial, distance, stimNumber, response, RT)

#for participants with <.50 on repeat agreement from info, delete from means and master
master <- merge(master, info, by = "participant")
excludedAgreement <- master %>% dplyr::filter(repeatAgree < .5)
write.csv(excludedAgreement, "excludedAgreement.csv")

#also manually exclude participants who were bots
master <- master %>% dplyr::filter(repeatAgree > .5) %>%
  filter(prolificID != "5ee4e82b8e523a1fea7856c8") %>%
  filter(prolificID != "5ecee8199d86e91500183985") %>%
  select(participant, trial, distance, stimNumber, response, RT, NRemoved, repeatAgree)
write.csv(master, "master.csv")

#find means for each distance
distance1 <- master %>% dplyr::filter(distance == 1)
meanRT.distance1 <- mean(distance1$RT)
meanResponse.distance1 <- mean(distance1$response)
distance2 <- master %>% dplyr::filter(distance == 2)
meanRT.distance2 <- mean(distance2$RT)
meanResponse.distance2 <- mean(distance2$response)
distance3 <- master %>% dplyr::filter(distance == 3)
meanRT.distance3 <- mean(distance3$RT)
meanResponse.distance3 <- mean(distance3$response)
distance4 <- master %>% dplyr::filter(distance == 4)
meanRT.distance4 <- mean(distance4$RT)
meanResponse.distance4 <- mean(distance4$response)
distance6 <- master %>% dplyr::filter(distance == 6)
meanRT.distance6 <- mean(distance6$RT)
meanResponse.distance6 <- mean(distance6$response)
distance10 <- master %>% dplyr::filter(distance == 10)
meanRT.distance10 <- mean(distance10$RT)
meanResponse.distance10 <- mean(distance10$response)
means <- data.frame(meanRT.distance1, meanRT.distance2, meanRT.distance3, meanRT.distance4, meanRT.distance6, meanRT.distance10, meanResponse.distance1, meanResponse.distance2, meanResponse.distance3, meanResponse.distance4, meanResponse.distance6, meanResponse.distance10)
write.csv(means, "means_all.csv")

#remove response = 0 for distance = 1, save to .csv
distance1 <- distance1 %>% dplyr::filter(response == 1)
master_filtered1 <- master %>% dplyr::filter(distance > 1)
master_filtered1 <- rbind(distance1, master_filtered1)
write.csv(master_filtered1, "master_filtered1.csv")

#add musician data & repeat
info <- read.csv("info.csv")
musicMaster <- merge(master, info, by = "participant")
musicians <- musicMaster %>% dplyr::filter(musicianYN == 1) %>%
  select(participant, trial, distance, stimNumber, response, RT, musicianYN)
distance1 <- musicians %>% dplyr::filter(distance == 1)
meanRT.distance1 <- mean(distance1$RT)
meanResponse.distance1 <- mean(distance1$response)
distance2 <- musicians %>% dplyr::filter(distance == 2)
meanRT.distance2 <- mean(distance2$RT)
meanResponse.distance2 <- mean(distance2$response)
distance3 <- musicians %>% dplyr::filter(distance == 3)
meanRT.distance3 <- mean(distance3$RT)
meanResponse.distance3 <- mean(distance3$response)
distance4 <- musicians %>% dplyr::filter(distance == 4)
meanRT.distance4 <- mean(distance4$RT)
meanResponse.distance4 <- mean(distance4$response)
distance6 <- musicians %>% dplyr::filter(distance == 6)
meanRT.distance6 <- mean(distance6$RT)
meanResponse.distance6 <- mean(distance6$response)
distance10 <- musicians %>% dplyr::filter(distance == 10)
meanRT.distance10 <- mean(distance10$RT)
meanResponse.distance10 <- mean(distance10$response)
means <- data.frame(meanRT.distance1, meanRT.distance2, meanRT.distance3, meanRT.distance4, meanRT.distance6, meanRT.distance10, meanResponse.distance1, meanResponse.distance2, meanResponse.distance3, meanResponse.distance4, meanResponse.distance6, meanResponse.distance10)
write.csv(means, "means_musicians.csv")

nonmusicians <- musicMaster %>% dplyr::filter(musicianYN == 0) %>%
  select(participant, trial, distance, stimNumber, response, RT, musicianYN)
distance1 <- nonmusicians %>% dplyr::filter(distance == 1)
meanRT.distance1 <- mean(distance1$RT)
meanResponse.distance1 <- mean(distance1$response)
distance2 <- nonmusicians %>% dplyr::filter(distance == 2)
meanRT.distance2 <- mean(distance2$RT)
meanResponse.distance2 <- mean(distance2$response)
distance3 <- nonmusicians %>% dplyr::filter(distance == 3)
meanRT.distance3 <- mean(distance3$RT)
meanResponse.distance3 <- mean(distance3$response)
distance4 <- nonmusicians %>% dplyr::filter(distance == 4)
meanRT.distance4 <- mean(distance4$RT)
meanResponse.distance4 <- mean(distance4$response)
distance6 <- nonmusicians %>% dplyr::filter(distance == 6)
meanRT.distance6 <- mean(distance6$RT)
meanResponse.distance6 <- mean(distance6$response)
distance10 <- nonmusicians %>% dplyr::filter(distance == 10)
meanRT.distance10 <- mean(distance10$RT)
meanResponse.distance10 <- mean(distance10$response)
means <- data.frame(meanRT.distance1, meanRT.distance2, meanRT.distance3, meanRT.distance4, meanRT.distance6, meanRT.distance10, meanResponse.distance1, meanResponse.distance2, meanResponse.distance3, meanResponse.distance4, meanResponse.distance6, meanResponse.distance10)
write.csv(means, "means_nonmusicians.csv")

#to do: clean up the above so no manual rearranging of excel file