library(xlsx)
library(tidyverse)

JNSurvey <- readxl::read_excel("pilot data/JNSurvey.xlsx")
info <- read.csv("pilot data/master_info_excludes/info.csv")

JNSurvey <- JNSurvey %>% dplyr::rename(prolificID = ProlificID)
cleanSurvey <- merge(info, JNSurvey, by = "prolificID")

write.csv(cleanSurvey, "JNPilotSurvey.csv")
