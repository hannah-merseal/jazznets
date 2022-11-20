library(poweRlaw)
library(tidyverse)

nodes <- read_csv("node-list.csv") %>%
  select(Id, Label, frequency, indegree, outdegree)

#setup
JN_in <- nodes %>% select(indegree) %>% filter(indegree > 0)
JN_out <- nodes %>% select(outdegree) %>% filter(outdegree > 0)

m_in <- displ$new(JN_in$indegree)
m_out <- displ$new(JN_out$outdegree)

est_in <- estimate_pars(m_in)
est_out <- estimate_pars(m_out)

est_in_x <- estimate_xmin(m_in)
est_out_x <- estimate_xmin(m_out)

m_in$setXmin(est_in_x)
m_out$setXmin(est_out_x)

#bootstrap
bs_in <- bootstrap(m_in, no_of_sims = 1000, threads = 1)
bs_out <- bootstrap(m_out, no_of_sims = 1000, threads = 1)

#bootstrapped p values
bs_in_p <- bootstrap_p(m_in, no_of_sims = 1000, threads = 2)
bs_out_p <- bootstrap_p(m_out, no_of_sims = 1000, threads = 2)

#plots
plot(m_in, xlab = " ", ylab = "CDF")

in_plot <- plot(m_in)
out_plot <- plot(m_out)
in_fit <- lines(m_in, col = 2)
out_fit <- lines(m_out, col = 2)

library(ggplot2)
plot <- ggplot(NULL, aes(x, y)) +
  geom_point(data = in_plot, color = 'blue', shape = 15) +
  geom_line(data = in_fit, color = 'blue') +
  geom_point(data = out_plot, color = 'red', shape = 17) +
  geom_line(data = out_fit, color = 'red') +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  xlab("In-degree and out-degree of melodic network") +
  ylab("CDF")
