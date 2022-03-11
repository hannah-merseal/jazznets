#source JNanalysiscode and JNmelodicsimilarity
detach_package <- function(pkg, character.only = FALSE)
{
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}

#reverse stim stuff
master_reverse <- merge(master_rev, survey)
master_reverse <- master_reverse %>% 
  dplyr::select(participant, trial, distance, stimNumber, rev, response, 
                RT, musicianYN, EmotionalContent, Contour, Dissonance, 
                IntervalVariety, IntervalSize, ImpliedHarmony, HoursWeekListen, 
                HoursWeekListenJazz, InstrumentYN, YearsFormalTraining, 
                PrimaryProficiency, YearsFormalImp, ImpProficiency, 
                YearsTheory, PlayHoursNow, ImpHoursNow, PercentImp) %>%
  filter(stimNumber <= 5) %>%
  mutate(rev = replace_na(rev, 0)) %>%
  mutate(rev = dplyr::recode(rev, "R" = '1'))

reverseMeans <- master_reverse %>%
  group_by(distance, rev) %>%
  summarize(meanResponse = mean(response),
            seResponse = sqrt(var(response)/length(response)),
            meanRT = mean(RT),
            seRT = sqrt(var(RT)/length(RT))) 
  
reversePlot <- ggplot(data = reverseMeans, aes(x = as.factor(distance), y = meanResponse, fill = rev)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = meanResponse - seResponse, ymax = meanResponse + seResponse), width = .2, position = position_dodge(0.9)) +
  xlab("Distance") +
  scale_y_continuous("Proportion of Yes Responses", limits = c(0, 1)) +
  scale_fill_discrete(name = "Stimuli", labels = c("Original \nPairs", "Reversed \nPairs")) +
  theme_bw()

master_reverse$distance = factor(master_reverse$distance)
master_reverse$rev = factor(master_reverse$rev)
reverse.glm <- glm(response ~ distance*rev, family = binomial, data = master_reverse)
summary(reverse.glm)

#correlation matrix
library(Hmisc)
library(corrplot)

CorrMatrix <- survey %>%
  dplyr::filter(musicianYN == 1) %>%
  select(hoursWeekListen, hoursWeekListenJazz, yearsFormalTraining,
                primaryProficiency, yearsFormalImp, impProficiency, yearsTheory,
                playHoursNow, impHoursNow, percentImp)
CorrMatrix.cor <- cor(CorrMatrix)
CorrMatrix.rcorr <- rcorr(as.matrix(CorrMatrix))
CorrMatrix.coeff <- CorrMatrix.rcorr$r %>%
  round(2)
CorrMatrix.p <- CorrMatrix.rcorr$P %>%
  round(3)

#Figure 2 - degree distribution
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
