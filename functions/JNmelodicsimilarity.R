#make sure to source JNanalysiscode

stimuli <- read.csv("stimuli/MelodicDistance_With20.csv") %>%
  dplyr::rename(pairID = ï..pairID)
frequencycomb <- read.csv("stimuli/frequencycomb.csv") %>% 
  dplyr::rename(distance = ï..distance)

#get summary statistics
stimuliStats <- frequencycomb %>%
  group_by(distance) %>%
  summarise(
    count = n(),
    mean = mean(frequency),
    sd = sd(frequency)
  )

#frequency boxplot
freqboxplot <- ggplot(frequencycomb, aes(x = factor(distance), y = frequency, fill = factor(distance))) +
  geom_boxplot(show.legend = FALSE) +
  theme_bw() +
  xlab("Distance") +
  ylab("Frequency") 

freq.aov <- aov(frequency ~ factor(distance), data = frequencycomb)
summary(freq.aov)
TukeyHSD(freq.aov)

#pcdist1 anova
pcdist1.aov <- aov(pcdist1 ~ factor(distance), data = stimuli)
summary(pcdist1.aov)
TukeyHSD(pcdist1.aov)

#pcdist1 plot
pcdist1.stats <- stimuli %>%
  group_by(distance) %>%
  summarise(
    count = n(),
    mean = mean(pcdist1),
    sd = sd(pcdist1)
  ) %>%
  mutate(se = (sd/sqrt(count)))
  
pcdist1.gg <- ggplot(pcdist1.stats, aes(x = factor(distance), y = mean, fill = factor(distance))) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .2, position = position_dodge(.9)) +
  theme_bw() +
  guides(fill = FALSE) +
  xlab("Distance") +
  ylab("Mean Pitch Class Distribution")

pcdist1.vp <- ggplot(stimuli, aes(x = factor(distance), y = pcdist1, fill = factor(distance))) +
  geom_violin(scale = "width", show.legend = FALSE) +
  geom_boxplot(width = .1, fill = "black", outlier.color = NA) +
  stat_summary(fun.y = median, geom = "point", fill = "white", shape = 21, size = 2) +
  theme_bw() +
  xlab("Distance") +
  ylab("Pitch-Class Distribution")

#ivdist1 anova
ivdist1.aov <- aov(ivdist1 ~ factor(distance), data = stimuli)
summary(ivdist1.aov)
TukeyHSD(ivdist1.aov)

#ivdist1 plot
ivdist1.stats <- stimuli %>%
  group_by(distance) %>%
  summarise(
    count = n(),
    mean = mean(ivdist1),
    sd = sd(ivdist1)
  ) %>%
  mutate(se = (sd/sqrt(count)))

ivdist1.gg <- ggplot(ivdist1.stats, aes(x = factor(distance), y = mean, fill = factor(distance))) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .2, position = position_dodge(.9)) +
  theme_bw() +
  guides(fill = FALSE) +
  xlab("Distance") +
  ylab("Mean Interval Class Distribution")

ivdist1.vp <- ggplot(stimuli, aes(x = factor(distance), y = ivdist1, fill = factor(distance))) +
  geom_violin(scale = "width", show.legend = FALSE) +
  geom_boxplot(width = .1, fill = "black", outlier.color = NA) +
  stat_summary(fun.y = median, geom = "point", fill = "white", shape = 21, size = 2) +
  theme_bw() +
  xlab("Distance") +
  ylab("Interval-Class Distribution")

#contour anova
contour.aov <- aov(contour ~ factor(distance), data = stimuli)
summary(contour.aov)
TukeyHSD(contour.aov)

#contour plot
contour.stats <- stimuli %>%
  group_by(distance) %>%
  summarise(
    count = n(),
    mean = mean(contour),
    sd = sd(contour)
  ) %>%
  mutate(se = (sd/sqrt(count)))

contour.gg <- ggplot(contour.stats, aes(x = factor(distance), y = mean, fill = factor(distance))) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .2, position = position_dodge(.9)) +
  theme_bw() +
  guides(fill = FALSE) +
  xlab("Distance") +
  ylab("Mean Melodic Contour")

contour.vp <- ggplot(stimuli, aes(x = factor(distance), y = contour, fill = factor(distance))) +
  geom_violin(scale = "width", show.legend = FALSE) +
  geom_boxplot(width = .1, fill = "black", outlier.color = NA) +
  stat_summary(fun.y = median, geom = "point", fill = "white", shape = 21, size = 2) +
  theme_bw() +
  xlab("Distance") +
  ylab("Melodic Contour")
