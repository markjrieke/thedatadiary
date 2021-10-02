# script to create a plot of time-decay weighted polls of
# VA Governor race with Bayesian updating 

# libraries ----
library(tidyverse)
library(lubridate)
library(patchwork)

# themes
source("https://raw.githubusercontent.com/markjrieke/thedatadiary/main/dd_theme_elements/dd_theme_elements.R")

# read in polls ----
polls <- read_csv("https://projects.fivethirtyeight.com/polls-page/data/governor_polls.csv")

# wrangle polls ----
polls <- 
  polls %>% 
  filter(state == "Virginia") %>%
  
  # showing which metrics I'm not using!!!
  select(poll_id, question_id, answer, pct, sample_size, end_date, election_date, methodology, internal, partisan, answer, pct) %>%
  
  # get to 2pv responses
  pivot_wider(names_from = answer,
              values_from = pct) %>%
  mutate(d_votes = round(McAuliffe/100 * sample_size),
         r_votes = round(Youngkin/100 * sample_size),
         t_votes = d_votes + r_votes) %>%
  select(-McAuliffe, -Youngkin, -Blanding) %>%
  
  # reformat dates
  mutate(end_date = mdy(end_date)) %>%
  
  # final cols
  select(end_date, d_votes, r_votes, t_votes)

# common beta function ----


# function for getting decayed weight ----
poll_map <- function(inp_date, final_date) {
  
  polls %>%
    filter(end_date <= inp_date) %>%
    mutate(days_diff = as.numeric(final_date - end_date),
           d_pct = d_votes/t_votes,
           
           # 95% ci
           b_lower = qbeta(0.025, d_votes, r_votes),
           b_upper = qbeta(0.975, d_votes, r_votes),
           plus_minus = b_upper - b_lower,
           
           # polls are weighted by standard error against a sample
           # size of 1000 (~+/- 3.1 se)
           pm_wt = .062/plus_minus,
           
           # poll weight decays as days pass by
           day_wt = 0.9 ^ days_diff,
           
           # each poll's contribution to the beta distribution
           alpha = d_votes * pm_wt * day_wt,
           beta = r_votes * pm_wt * day_wt) %>%
    
    # add in uniform prior (a = b = 1)
    summarize(alpha = sum(alpha) + 1,
              beta = sum(beta) + 1) %>%
    
    # get day's CI, voteshare, & p_win for both candidates
    mutate(date = inp_date,
           d_voteshare = alpha/(alpha + beta),
           d_lower = qbeta(0.025, alpha, beta),
           d_upper = qbeta(0.975, alpha, beta),
           d_win = 1 - pbeta(0.5, alpha, beta),
           r_voteshare = beta/(alpha + beta),
           r_lower = qbeta(0.025, beta, alpha),
           r_upper = qbeta(0.975, beta, alpha),
           r_win = 1 - pbeta(0.5, beta, alpha)) %>%
    
    # add in polls to plot against
    left_join(polls, by = c("date" = "end_date")) %>%
    
    # get 2pv result from polls
    mutate(poll_res = d_votes/t_votes) %>%
    select(date, poll_res, alpha, beta,
           d_voteshare, d_lower, d_upper, d_win, 
           r_voteshare, r_lower, r_upper, r_win)  
  
}

# plot polling results & confidence bands ----
days_sequence <- seq(ymd("2021-06-06"), ymd("2021-11-02"), by = "days")

expected_vote <- 
  map2_dfr(days_sequence,
           days_sequence,
           ~poll_map(.x, .y)) %>%
  mutate(remove = if_else(date < Sys.Date() & is.na(poll_res), "remove", "x")) %>%
  filter(remove != "remove") %>%
  select(-remove) %>%
  ggplot(aes(x = date)) +
  theme_minimal(base_family = "Roboto Slab") +
  geom_ribbon(aes(ymin = r_lower,
                  ymax = r_upper),
              fill = dd_red,
              alpha = 0.2) +
  geom_ribbon(aes(ymin = d_lower,
                  ymax = d_upper),
              fill = dd_blue,
              alpha = 0.2) +
  geom_hline(yintercept = 0.5,
             linetype = "dashed",
             size = 1,
             color = dd_gray) +
  geom_point(aes(y = poll_res),
             color = dd_blue,
             size = 3,
             alpha = 0.3) +
  geom_point(aes(y = 1 - poll_res),
             color = dd_red,
             size = 3,
             alpha = 0.3) +
  geom_line(aes(y = r_voteshare),
            color = "white",
            size = 3) +
  geom_line(aes(y = r_voteshare),
            color = dd_red,
            size = 1.25) +
  geom_line(aes(y = d_voteshare),
            color = "white",
            size = 3) +
  geom_line(aes(y = d_voteshare),
            color = dd_blue,
            size = 1.25) +
  theme(legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_markdown(size = 16),
        plot.subtitle = element_markdown(size = 14)) +
  labs(title = "**Expected Voteshare in the Virginia Governor Race**",
       subtitle = paste("<span style=color:'#5565D7'>**McAuliffe's**</span> and <span style=color:'#D75565'>**Youngkin's**</span> two-party polling average as of",
                        format(Sys.Date(), "%B %d"),
                        sep = " "),
       caption = "Polls weighted by recency & sample size.\nData from @FiveThirtyEight",
       x = NULL,
       y = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

# plot win probability ----

probability <- 
  map2_dfr(days_sequence,
           rep(ymd("2021-11-02"), 150),
           ~poll_map(.x, .y)) %>% 
  ggplot(aes(x = date)) +
  geom_hline(yintercept = 0.5,
             linetype = "dashed",
             size = 1,
             color = dd_gray) +
  geom_line(aes(y = r_win),
            color = "white",
            size = 3) +
  geom_line(aes(y = r_win),
            color = dd_red,
            size = 1.25) +
  geom_line(aes(y = d_win),
            color = "white",
            size = 3) +
  geom_line(aes(y = d_win),
            color = dd_blue,
            size = 1.25) +
  theme_minimal(base_family = "Roboto Slab") +
  theme(plot.title.position = "plot",
        plot.title = element_markdown(size = 16),
        plot.subtitle = element_markdown(size = 14)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "**Probability of Winning Virginia's Governor Race**",
       subtitle = paste("<span style=color:'#5565D7'>**McAuliffe's**</span> and <span style=color:'#D75565'>**Youngkin's**</span> probability of winning as of",
                        format(Sys.Date(), "%B %d"),
                        sep = " "),
       caption = "Polls weighted by recency & sample size.\nData from @FiveThirtyEight",
       x = NULL,
       y = NULL)

# merge & save plots ----

# patchwork plot
expected_vote / probability

# add path
path <- "2021.10.01-virginia_governors_race/"

# save with date
ggsave(paste0(path, "plots/race_results_", Sys.Date(), ".png"),
       width = 9,
       height = 12,
       units = "in",
       dpi = 500)

# save without date
ggsave(paste0(path, "plots/race_results_current.png"),
       width = 9,
       height = 12,
       units = "in",
       dpi = 500)
  

  
  
  



