# libraries ----
library(ggplot2)
library(dplyr)

# import, clean csv ----
f_polls <- as.data.frame(
  read.csv("https://projects.fivethirtyeight.com/2020-general-data/presidential_polls_2020.csv")
)

f_polls <- f_polls %>%
  select(state,
         candidate_name,
         enddate,
         samplesize,
         pct) %>%
  filter(state == "National") %>%
  mutate(enddate = as.Date(enddate, "%m/%d/%y"),
         pct = pct/100) %>%
  filter(enddate >= "2020-10-20",
         enddate <= "2020-11-03") %>%
  mutate(pct = if_else(candidate_name == "Donald Trump",
                       1 - pct,
                       pct)) %>%
  arrange(enddate) %>%
  select(-state, -candidate_name)

# for some reason, can't attatch this to above pipe...
f_polls <- f_polls %>%
  mutate(index = seq.int(nrow(f_polls))) %>%
  relocate(index)

# set bayes cols a/b/mlo ----

# set sum of params a & b for beta shape
sum_params <- 50

f_polls <- f_polls %>%
  mutate(biden_success = round(samplesize * pct),
         a_sum = cumsum(biden_success),
         b_sum = cumsum(samplesize) - a_sum,
         a_param = sum_params * a_sum/(a_sum + b_sum),
         b_param = sum_params * b_sum/(a_sum + b_sum))

# starting up summary frame
f_pollmean <- f_polls %>%
  group_by(enddate) %>%
  summarise_each(funs("max")) %>%
  select(enddate, a_param, b_param) %>%
  mutate(var = a_param / (a_param + b_param),
         desc = "b_pct")

a_param <- f_pollmean[[nrow(f_pollmean), 2]]
b_param <- f_pollmean[[nrow(f_pollmean), 3]]

# frame for CDF
f_cdf <- f_pollmean %>%
  select(-var, -desc) %>%
  mutate(cdf = 1 - pbeta(0.5, a_param, b_param)) %>%
  select(-a_param, -b_param) %>%
  mutate(t_cdf = 1 - cdf)

colnames(f_cdf) <- c("date", "b_cdf", "t_cdf")

# finishing out summary frame
f_pollmean <- f_pollmean %>%
  select(-a_param, -b_param)

f_polls <- f_polls %>%
  select(enddate, pct) %>%
  mutate(desc = "polls")

v_colname <- c("date", "var", "desc")

colnames(f_polls) <- v_colname
colnames(f_pollmean) <- v_colname

f_summary <- rbind(f_pollmean, f_polls)

# creating beta dist frame
x <- seq(0, 1, length = 201)
f_beta <- as.data.frame(cbind(x, dbeta(x, a_param, b_param)))

colnames(f_beta) <- c("x", "beta")

f_beta <- f_beta %>%
  mutate(win = if_else(x < 0.5, "trump", "biden"))

# ggplotting ----

# dd colors !
dd_orange <- "#F48847"
dd_black <- "#282D30"
dd_cream <- "#F9F3EB"
dd_blue <- "#5565D7"
dd_red <- "#D75565"

# voteshare plot
p_voteshare <- ggplot(data = f_summary,
                      aes(x = date,
                          y = var,
                          color = desc)) + 
  geom_point(data = filter(f_summary,
                           desc == "polls"),
             alpha = 0.5,
             size = 5,
             color = dd_orange,
             position = position_jitter(width = 0.3,
                                        height = 0)) +
  geom_path(data = filter(f_summary,
                          desc == "b_pct"),
            size = 5,
            alpha = 0.75,
            color = "white") +
  geom_path(data = filter(f_summary,
                          desc == "b_pct"),
            size = 1.5,
            color = dd_black)

# adding formatting
p_voteshare <- p_voteshare +
  theme(text = element_text(color = dd_black),
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 12),
        panel.background = element_rect(fill = dd_cream),
        axis.line.x.bottom = element_line(color = dd_black),
        axis.line.y.left = element_line(color = dd_black),
        axis.text = element_text(color = dd_black),
        axis.ticks = element_line(color = dd_black)) + 
  labs(title = "Joe Biden's Projected Popular Vote",
       subtitle = "National polls from Oct-20 to Nov-3",
       caption = "National polls from @FiveThirtyEight's polling repository:
                  https://projects.fivethirtyeight.com/",
       x = NULL,
       y = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

# annotations and captions
p_voteshare <- p_voteshare +
  annotate(geom = "curve",
           x = as.Date("2020-11-1"),
           y = 0.63,
           xend = as.Date("2020-11-2"),
           yend = 0.54,
           curvature = -0.3,
           arrow = arrow(length = unit(3, "mm")),
           size = 1.1,
           color = dd_black) +
  annotate(geom = "label",
           label = "Model prediction: 53.6%\nActual outcome: 52.2%",
           label.size = 0,
           family = "mono",
           fontface = "bold",
           fill = dd_orange,
           alpha = 0.25,
           x = as.Date("2020-10-30"),
           y = 0.63) +
  annotate(geom = "curve",
           x = as.Date("2020-10-25"),
           y = 0.63,
           xend = as.Date("2020-10-26"),
           yend = 0.615,
           curvature = -0.3,
           arrow = arrow(length = unit(3, "mm")),
           size = 1.1,
           color = dd_black) +
  annotate(geom = "label",
           label = "Each point is a poll",
           label.size = 0,
           family = "mono",
           fontface = "bold",
           fill = dd_orange,
           alpha = 0.25,
           x = as.Date("2020-10-25"),
           y = 0.63,
           hjust = "right")

# save voteshare plot
ggsave("p_voteshare.png",
       p_voteshare,
       width = 9,
       height = 6,
       units = "in",
       dpi = 500)

# plot beta dist
p_beta <- ggplot(f_beta,
                 aes(x = x,
                     y = beta)) +
  geom_area(data = filter(f_beta,
                          win == "trump"),
            aes(x = x,
                y = beta),
            fill = dd_red) +
  geom_area(data = filter(f_beta,
                          win == "biden"),
            aes(x = x,
                y = beta),
            fill = dd_blue) + 
  geom_path(color = "white",
            size = 3,
            alpha = 0.75) +
  geom_path(color = dd_black,
            size = 1.5)

# add theme elements
p_beta <- p_beta +
  theme(text = element_text(color = dd_black),
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 12),
        panel.background = element_rect(fill = dd_cream),
        axis.line.x.bottom = element_line(color = dd_black),
        axis.line.y.left = element_line(color = dd_black),
        axis.text = element_text(color = dd_black),
        axis.ticks = element_line(color = dd_black),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  labs(title = "Popular Vote Probability Distribution",
       subtitle = "Two-party Popular Vote, Nov-2",
       caption = "National polls from @FiveThirtyEight's polling repository:
                  https://projects.fivethirtyeight.com/",
       x = "Biden's Popular Vote Share",
       y = NULL) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0, 6.5))

# add annotations

p_beta <- p_beta + 
  annotate(geom = "curve",
           x = 0.75,
           y = 6,
           xend = 0.55,
           yend = 5.8,
           curvature = 0.4,
           size = 1.1,
           color = dd_black,
           arrow = arrow(length = unit(3, "mm"))) +
  annotate(geom = "label",
           label = "Biden's projected\npopular vote: 53.6%",
           label.size = 0,
           family = "mono",
           fontface = "bold",
           x = 0.75,
           y = 6,
           fill = dd_orange,
           alpha = 0.25,
           vjust = "top") +
  annotate(geom = "label",
           label = "The total area under\nthe curve represents\neach candidate's\nprobability of winning\n the popular vote.",
           label.size = 0,
           family = "mono",
           fontface = "bold",
           x = 0.2,
           y = 5,
           fill = dd_orange,
           alpha = 0.25) + 
  annotate(geom = "text",
           label = "69.6%",
           x = 0.6,
           y = 0.5,
           color = "white",
           family = "mono",
           fontface = "bold",
           size = 6) +
  annotate(geom = "text",
           label = "30.4%",
           x = 0.443,
           y = 0.5,
           color = "white",
           family = "mono",
           fontface = "bold",
           size = 6)

# save beta plot
ggsave("p_beta.png",
       p_beta,
       width = 9,
       height = 6,
       units = "in",
       dpi = 500)

# startup winprob plot
p_winprob <- ggplot() +
  geom_path(data = f_cdf,
            aes(x = date,
                y = b_cdf),
            color = "white",
            size = 3,
            alpha = 0.75) +
  geom_path(data = f_cdf,
            aes(x = date,
                y = t_cdf),
            color = "white",
            size = 3,
            alpha = 0.75) +
  geom_path(data = f_cdf,
            aes(x = date,
                y = b_cdf),
            color = dd_blue,
            size = 1.5) +
  geom_path(data = f_cdf,
            aes(x = date,
                y = t_cdf),
            color = dd_red,
            size = 1.5,
            alpha = 0.75)

# add theme elements
p_winprob <- p_winprob +
  theme(text = element_text(color = dd_black),
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 12),
        panel.background = element_rect(fill = dd_cream),
        axis.line.x.bottom = element_line(color = dd_black),
        axis.line.y.left = element_line(color = dd_black),
        axis.text = element_text(color = dd_black),
        axis.ticks = element_line(color = dd_black)) + 
  labs(title = "Probability of Winning the National Popular Vote",
       subtitle = "From Oct-26 to Nov-2",
       caption = "National polls from @FiveThirtyEight's polling repository:
                  https://projects.fivethirtyeight.com/",
       x = NULL,
       y = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.2, 0.8))

# add annotations
p_winprob <- p_winprob +
  annotate(geom = "curve",
           x = as.Date("2020-10-22"),
           y = 0.5,
           xend = as.Date("2020-10-21"),
           yend = 0.68,
           curvature = -0.3,
           size = 1.1,
           color = dd_black,
           arrow = arrow(length = unit(3, "mm"))) + 
  annotate(geom = "curve",
           x = as.Date("2020-10-22"),
           y = 0.5,
           xend = as.Date("2020-10-21"),
           yend = 0.32,
           curvature = 0.3,
           size = 1.1,
           color = dd_black,
           arrow = arrow(length = unit(3, "mm"))) +
  annotate(geom = "label",
           x = as.Date("2020-10-22"),
           y = 0.5,
           hjust = "left",
           label = "Initial prior is \nupdated quickly with\na 2nd day of polling\ndata, then stabalizes",
           family = "mono",
           fontface = "bold",
           label.size = 0,
           fill = dd_orange,
           alpha = 0.25) +
  annotate(geom = "curve",
           x = as.Date("2020-11-1"),
           y = 0.775,
           xend = as.Date("2020-11-2"),
           yend = 0.71,
           curvature = -0.4,
           size = 1.1,
           color = dd_black,
           arrow = arrow(length = unit(3, "mm"))) +
  annotate(geom = "curve",
           x = as.Date("2020-11-1"),
           y = 0.225,
           xend = as.Date("2020-11-2"),
           yend = 0.29,
           curvature = 0.4,
           size = 1.1,
           color = dd_black,
           arrow = arrow(length = unit(3, "mm"))) +
  annotate(geom = "label",
           x = as.Date("2020-11-1"),
           y = 0.775,
           hjust = "right",
           label = "Probability of Biden\nwinning: 69.6%",
           family = "mono",
           fontface = "bold",
           label.size = 0,
           fill = dd_orange,
           alpha = 0.25) +
  annotate(geom = "label",
           x = as.Date("2020-11-1"),
           y = 0.225,
           hjust = "right",
           label = "Probability of Trump\nwinning: 30.4%",
           family = "mono",
           fontface = "bold",
           label.size = 0,
           fill = dd_orange,
           alpha = 0.25)

# save plot
ggsave("p_winprob.png",
       p_winprob,
       width = 9,
       height = 6,
       units = "in",
       dpi = 500)

# test area ----

# hi!