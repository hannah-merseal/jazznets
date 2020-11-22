library(tidyverse)

All <- read.csv("data/means/means_all.csv")
Musicians <- read.csv("data/means/means_musicians.csv")
Nonmusicians <- read.csv("data/means/means_nonmusicians.csv")

meansPlot <- ggplot() + theme_bw() +
  geom_line(data = All, aes(x = distance, y = meanResponse, color = "red"))
  geom_line(data = Musicians, aes(x = distance, y = meanResponse, color = "blue")) +
  geom_line(data = Nonmusicians, aes(x = distance, y = meanResponse, color = "green")) +
  geom_ribbon() +
  labs(x = "Distance",
       y = "Mean Response",
       colour = "Legend") +
  scale_color_identity(name = "Legend",
                       breaks = c("red", "blue", "green"),
                       labels = c("All", "Musicians", "Nonmusicians"),
                       guide = "legend") +
  scale_x_continuous(limits = c(1,20), breaks = c(1,2,3,4,5,6,7,8,9,10,20)) +
  scale_y_continuous(limits = c(0.3,0.7)) +
  ggtitle("Average Response by Distance and Group")

meansPlot  

RTPlot <- ggplot() + theme_bw() +
  geom_line(data = All, aes(x = distance, y = meanRT, color = "red")) +
  geom_line(data = Musicians, aes(x = distance, y = meanRT, color = "blue")) +
  geom_line(data = Nonmusicians, aes(x = distance, y = meanRT, color = "green")) +
  geom_ribbon() +
  labs(x = "Distance",
       y = "Mean Reaction Time",
       colour = "Legend") +
  scale_color_identity(name = "Legend",
                       breaks = c("red", "blue", "green"),
                       labels = c("All", "Musicians", "Nonmusicians"),
                       guide = "legend") +
  ggtitle("Average Reaction Time by Distance and Group (Correct Trials") +
  scale_x_continuous(limits = c(1,20), breaks = c(1,2,3,4,5,6,7,8,9,10,20))

RTPlot
