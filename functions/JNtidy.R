###PARTICIPANT CLEANING###
JNtidy <- function(){
  for(i in 1:73){
    #load participant files
    loadFilePath <- paste("pilot data/raw/JN_", i, ".csv", sep = "")
    subject <- read.csv(loadFilePath, na.strings = c("","NA"))
    #select relevant columns
    subject <- subject %>% dplyr::select(participant, key_resp_2.keys, key_resp_2.rt, audioStim) %>%
      dplyr::rename(prolificID = participant,
                    RT = key_resp_2.rt,
                    response = key_resp_2.keys)
    #recode y/n responses to 1 and 0, delete NA rows
    subject <- subject %>% 
      dplyr::mutate(response = recode(response, s = 1, k = 0)) %>%
      na.omit()
    #add trial column
    subject$trial <- c(1:300)
    #add participant column
    subject$participant <- c(paste("JN_", i, sep = ""))
    #write each participant to new csv
    saveFilePath <- paste("pilot data/clean/JN_", i, "_clean.csv", sep = "")
    write.csv(subject, saveFilePath)
    message <- paste("Participant JN_", i, "is complete!", sep = " ")
    print(message)
  }
}