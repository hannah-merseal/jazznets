library(tidyverse)


meansPlot <- ggplot() + theme_minimal() +
  geom_line(data = Musicians20No, aes(x = distance, y = meanResponse, color = "#FD6467")) +
  geom_errorbar(data = Musicians20No, aes(x = distance, ymin = meanResponse - SEResponse, ymax = meanResponse + SEResponse), inherit.aes = FALSE, color = "#FD6467", position = pd) +
  geom_line(data = Nonmusicians20No, aes(x = distance, y = meanResponse, color = "#00A08A")) +
  geom_errorbar(data = Nonmusicians20No, aes(x = distance, ymin = meanResponse - SEResponse, ymax = meanResponse + SEResponse), inherit.aes = FALSE, color = "#00A08A", position = pdd) +
  geom_ribbon() +
  labs(x = "Distance",
       y = "Mean Response",
       colour = "Groups") +
  scale_color_identity(name = "Groups",
                       breaks = c("#FD6467", "#00A08A"),
                       labels = c("Musicians", "Nonmusicians"),
                       guide = "legend") +
  scale_x_continuous(limits = c(0,21), breaks = c(0:20)) +
  scale_y_continuous(limits = c(.25,1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 20))
meansPlot  


