library(tidyverse)

All20No <- read.csv("data/means/means_all_20No.csv")
Musicians20No <- read.csv("data/means/means_musicians_20No.csv")
Nonmusicians20No <- read.csv("data/means/means_nonmusicians_20No.csv")
All20Yes <- read.csv("data/means/means_all_20Yes.csv")
Musicians20Yes <- read.csv("data/means/means_musicians_20Yes.csv")
Nonmusicians20Yes <- read.csv("data/means/means_nonmusicians_20Yes.csv")

meansPlot <- ggplot() + theme_minimal() +
  geom_line(data = Musicians20No, aes(x = distance, y = meanResponse, color = "#FD6467")) +
  geom_errorbar(data = Musicians20No, aes(x = distance, ymin = meanResponse - SEResponse, ymax = meanResponse + SEResponse), inherit.aes = FALSE, color = "#FD6467", position = pd) +
  geom_line(data = Nonmusicians20No, aes(x = distance, y = meanResponse, color = "#00A08A")) +
  geom_errorbar(data = Nonmusicians20No, aes(x = distance, ymin = meanResponse - SEResponse, ymax = meanResponse + SEResponse), inherit.aes = FALSE, color = "#00A08A", position = pdd) +
  geom_ribbon() +
  labs(x = "Distance",
       y = "Mean Response",
       colour = "Legend") +
  scale_color_identity(name = "Legend",
                       breaks = c("#FD6467", "#00A08A"),
                       labels = c("Musicians", "Nonmusicians"),
                       guide = "legend") +
  scale_x_continuous(limits = c(0,21), breaks = c(0:20)) +
  scale_y_continuous(limits = c(.25,1)) +
  ggtitle("Average Response by Distance and Group")

meansPlot  

pd <- position_dodge(0.1)
pdd <- position_dodge(0.2)

RTPlot20No <- ggplot() + theme_minimal() +
  geom_line(data = Musicians20No, aes(x = distance, y = meanRT, color = "#FD6467")) +
  geom_errorbar(data = Musicians20No, aes(x = distance, ymin = meanRT - SERT, ymax = meanRT + SERT), inherit.aes = FALSE, color = "#FD6467") +
  geom_line(data = Nonmusicians20No, aes(x = distance, y = meanRT, color = "#00A08A")) +
  geom_errorbar(data = Nonmusicians20No, aes(x = distance, ymin = meanRT - SERT, ymax = meanRT + SERT), inherit.aes = FALSE, color = "#00A08A", position = pdd) +
  geom_ribbon() +
  labs(x = "Distance",
       y = "Mean Reaction Time",
       colour = "Legend") +
  scale_color_identity(name = "Legend",
                       breaks = c("#FD6467", "#00A08A"),
                       labels = c("Musicians", "Nonmusicians"),
                       guide = "legend") +
  scale_x_continuous(limits = c(0,10.5), breaks = c(1,2,3,4,5,6,7,8,9,10)) 

RTPlot20No

RTPlot20Yes <- ggplot() + theme_bw() +
  geom_line(data = All20Yes, aes(x = distance, y = meanRT, color = "red")) +
  geom_errorbar(data = All20Yes, aes(x = distance, ymin = meanRT - SERT, ymax = meanRT + SERT), inherit.aes = FALSE, color = "red", position = pd) +
  geom_line(data = Musicians20Yes, aes(x = distance, y = meanRT, color = "blue")) +
  geom_errorbar(data = Musicians20Yes, aes(x = distance, ymin = meanRT - SERT, ymax = meanRT + SERT), inherit.aes = FALSE, color = "blue") +
  geom_line(data = Nonmusicians20Yes, aes(x = distance, y = meanRT, color = "green")) +
  geom_errorbar(data = Nonmusicians20Yes, aes(x = distance, ymin = meanRT - SERT, ymax = meanRT + SERT), inherit.aes = FALSE, color = "green", position = pdd) +
  geom_ribbon() +
  labs(x = "Distance",
       y = "Mean Reaction Time",
       colour = "Legend") +
  scale_color_identity(name = "Legend",
                       breaks = c("red", "blue", "green"),
                       labels = c("All", "Musicians", "Nonmusicians"),
                       guide = "legend") +
  ggtitle("Average Reaction Time by Distance and Group (Correct Trials, 20 = Related)") +
  scale_x_continuous(limits = c(1,20), breaks = c(1,2,3,4,5,6,7,8,9,10,20)) 

RTPlot20Yes
