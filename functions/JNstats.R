###PARTICIPANT FILTERING###

JNstats <- function(){
  info <- c()
  means <- c()
  for(i in 1:73){
    #load tidy participant files
    loadFilePath <- paste("pilot data/clean/JN_", i, "_clean.csv", sep = "")
    subject <- read.csv(loadFilePath)
    
    #calculate agreement on catch trials (*later: add agreement on reversed trials)
    subjectRepeat <- subject[duplicated(subject$audioStim)|duplicated(subject$audioStim, fromLast = TRUE),]
    subjectRepeat <- subjectRepeat %>% separate(audioStim, into = c(NA, "audioStim"), sep = "/") %>%
      separate(audioStim, into = c("audioStim", NA), sep = ".wav") %>%
      dplyr::arrange(audioStim) %>%
      dplyr::group_by(audioStim)
    agreement <- c()
    for(j in 1:nrow(subjectRepeat)/2){
      trial1 <- subjectRepeat[2*j-1,]$response
      trial2 <- subjectRepeat[2*j,]$response
      thisComparison <- (trial1 == trial2)
      agreement <- c(agreement, thisComparison)
    }
    agree <- sum(agreement)
    agreelength <- length(agreement)
    repeatAgree <- agree/agreelength
    
    #sort columns and split audioStim into distance and number
    subject <- subject %>% dplyr::select(participant, prolificID, trial, audioStim, response, RT) %>%
      separate(audioStim, into = c(NA, "audioStim"), sep = "/") %>%
      separate(audioStim, into = c("audioStim", NA), sep = ".wav") %>%
      separate(audioStim, into = c("distance", "stimNumber"), sep = "_")
    
    #remove trials with RT less than 250ms or +/- 2.5 SD away from mean for each distance, save # removed
    subject <- subject %>% dplyr::filter(RT > .250) %>% 
      group_by(distance) %>%
      mutate(zscore = (RT - mean(RT))/sd(RT)) %>%
      filter(zscore < 2.5) %>% filter(zscore > -2.5)
    NRemoved <- 300 - nrow(subject)
    
    #save IDs, # removed trials, catch trial agreement to info dataframe
    prolificID <- subject$prolificID[1]
    participant <- subject$participant[1]
    info <- rbind(info, data.frame(participant, prolificID, NRemoved, repeatAgree))
    saveFilePath <- paste("pilot data/filtered/JN_", i, ".csv", sep = "")
    write.csv(subject, saveFilePath)
    message <- paste("Participant", i, "is filtered!", sep = " ")
    print(message)
  }
  write.csv(info, "~/hannah-merseal/jazznets/pilot data/master_info_excludes/info.csv")
  message2 <- paste("Info written to file")
  print(message2)
  write.csv(means, "~/hannah-merseal/jazznets/pilot data/master_info_excludes/means.csv")
  message3 <- paste("Means written to file")
  print(message3)
}

