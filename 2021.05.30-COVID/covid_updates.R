# libraries ----
library(tidyverse)
library(zoo)
devtools::source_url("https://raw.githubusercontent.com/markjrieke/thedatadiary/main/dd_theme_elements/dd_theme_elements.R")

# wrangle ----

# create frame of state populations
# based on Jul 1 2020 population estimates
f_pop <- read_csv("2021.05.30-COVID/state_pops_jul_1_2020.csv")

# wrangle nyt COVID data for cases
read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv") %>%
  filter(!state %in% c("Puerto Rico", "Virgin Islands", "Northern Mariana Islands", "Guam")) %>%
  select(-fips, -deaths) %>%
  pivot_wider(names_from = state,
              values_from = cases) %>%
  replace(is.na(.), 0) %>%
  mutate(USA = rowSums(across(where(is.numeric)))) %>%
  pivot_longer(cols = -date,
               names_to = "state",
               values_to = "cases") %>% 
  group_by(state) %>%
  mutate(cases_new = cases - lag(cases, default = 0),
         cases_ma = rollmean(cases_new, 7, fill = 0, align = "right")) %>% 
  ungroup() %>%
  left_join(f_pop, by = "state") %>%
  mutate(cases_ma_norm = cases_ma/pop * 1000000, # cases per million
         us_avg = if_else(state == "USA", "us","rest")) %>%
  ggplot(aes(x = date,
             y = cases_ma_norm,
             group = state,
             color = us_avg,
             size = us_avg,
             alpha = us_avg)) +
  geom_line() +
  scale_alpha_manual(values = c(0.1, 1)) +
  scale_size_manual(values = c(0.75, 1.2)) +
  scale_color_manual(values = c(dd_blue, dd_blue_dark)) +
  dd_theme +
  labs(title = "Vaccines help reduce new case load",
       subtitle = "7-day avg. of new cases per million in <span style= color:'#5565D7'>**each state**</span> and <span style= color:'#353F86'>**the US as a whole.**</span>",
       caption = "Data from the New York Times, based on reports from state and local health agencies.",
       x = NULL,
       y = NULL) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::comma_format(),
                     limits = c(0, 2000)) +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "3 months") +
  geom_vline(xintercept = as.Date("2020-12-14"),
             linetype = "dashed",
             color = dd_gray,
             alpha = 0.5,
             size = 1) +
  annotate(geom = "text",
           x = as.Date("2020-12-25"),
           y = 1900,
           label = "Vaccines available\nin the US",
           family = "Siemens Slab",
           fontface = "bold",
           hjust = "left") +
  annotate(geom = "curve",
           x = as.Date("2020-12-25"),
           y = 1750,
           xend = as.Date("2021-3-15"),
           yend = 1750,
           size = 1.1,
           color = dd_gray,
           curvature = 0,
           arrow = arrow(length = unit(3, "mm")))

# save
ggsave("2021.05.30-COVID/p1.png",
       width = 9,
       height = 6,
       units = "in",
       dpi = 500)

# wrangle nyt COVID data for deaths
read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv") %>%
  filter(!state %in% c("Puerto Rico", "Virgin Islands", "Northern Mariana Islands", "Guam")) %>%
  select(-fips, -cases) %>%
  pivot_wider(names_from = state,
              values_from = deaths) %>%
  replace(is.na(.), 0) %>%
  mutate(USA = rowSums(across(where(is.numeric)))) %>%
  pivot_longer(cols = -date,
               names_to = "state",
               values_to = "deaths") %>% 
  group_by(state) %>%
  mutate(deaths_new = deaths - lag(deaths, default = 0),
         deaths_ma = rollmean(deaths_new, 7, fill = 0, align = "right")) %>% 
  ungroup() %>%
  left_join(f_pop, by = "state") %>%
  mutate(deaths_ma_norm = deaths_ma/pop * 1000000, # cases per million
         us_avg = if_else(state == "USA", "us","rest")) %>%
  ggplot(aes(x = date,
             y = deaths_ma_norm,
             group = state,
             color = us_avg,
             size = us_avg,
             alpha = us_avg)) +
  geom_line() +
  scale_alpha_manual(values = c(0.1, 1)) +
  scale_size_manual(values = c(0.75, 1.2)) +
  scale_color_manual(values = c(dd_red, dd_red_dark)) +
  dd_theme +
  labs(title = "Vaccines help reduce new case load",
       subtitle = "7-day avg. of deaths per million in <span style= color:'#D75565'>**each state**</span> and <span style= color:'#BC4A58'>**the US as a whole.**</span>",
       caption = "Data from the New York Times, based on reports from state and local health agencies.",
       x = NULL,
       y = NULL) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::comma_format(),
                     limits = c(0, 65)) +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "3 months") +
  geom_vline(xintercept = as.Date("2020-12-14"),
             linetype = "dashed",
             color = dd_gray,
             alpha = 0.5,
             size = 1) +
  annotate(geom = "text",
           x = as.Date("2020-12-25"),
           y = 60,
           label = "Vaccines available\nin the US",
           family = "Siemens Slab",
           fontface = "bold",
           hjust = "left") +
  annotate(geom = "curve",
           x = as.Date("2020-12-25"),
           y = 55,
           xend = as.Date("2021-3-15"),
           yend = 55,
           size = 1.1,
           color = dd_gray,
           curvature = 0,
           arrow = arrow(length = unit(3, "mm")))

# save
ggsave("2021.05.30-COVID/p2.png",
       width = 9,
       height = 6,
       units = "in",
       dpi = 500)


# zoomed in version of deaths chart
read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv") %>%
  filter(!state %in% c("Puerto Rico", "Virgin Islands", "Northern Mariana Islands", "Guam")) %>%
  select(-fips, -cases) %>%
  pivot_wider(names_from = state,
              values_from = deaths) %>%
  replace(is.na(.), 0) %>%
  mutate(USA = rowSums(across(where(is.numeric)))) %>%
  pivot_longer(cols = -date,
               names_to = "state",
               values_to = "deaths") %>% 
  group_by(state) %>%
  mutate(deaths_new = deaths - lag(deaths, default = 0),
         deaths_ma = rollmean(deaths_new, 7, fill = 0, align = "right")) %>% 
  ungroup() %>%
  left_join(f_pop, by = "state") %>%
  mutate(deaths_ma_norm = deaths_ma/pop * 1000000, # cases per million
         us_avg = if_else(state == "USA", "us","rest")) %>%
  ggplot(aes(x = date,
             y = deaths_ma_norm,
             group = state,
             color = us_avg,
             size = us_avg,
             alpha = us_avg)) +
  geom_line() +
  scale_alpha_manual(values = c(0.1, 1)) +
  scale_size_manual(values = c(0.75, 1.2)) +
  scale_color_manual(values = c(dd_red, dd_red_dark)) +
  dd_theme +
  labs(title = "Vaccines help reduce new case load",
       subtitle = "7-day avg. of deaths per million in <span style= color:'#D75565'>**each state**</span> and <span style= color:'#BC4A58'>**the US as a whole.**</span>",
       caption = "Data from the New York Times, based on reports from state and local health agencies.",
       x = NULL,
       y = NULL) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::comma_format(),
                     limits = c(0, 15)) +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "3 months") +
  geom_vline(xintercept = as.Date("2020-12-14"),
             linetype = "dashed",
             color = dd_gray,
             alpha = 0.5,
             size = 1) +
  annotate(geom = "text",
           x = as.Date("2020-12-25"),
           y = 60,
           label = "Vaccines available\nin the US",
           family = "Siemens Slab",
           fontface = "bold",
           hjust = "left") +
  annotate(geom = "curve",
           x = as.Date("2020-12-25"),
           y = 55,
           xend = as.Date("2021-3-15"),
           yend = 55,
           size = 1.1,
           color = dd_gray,
           curvature = 0,
           arrow = arrow(length = unit(3, "mm")))

# save
ggsave("2021.05.30-COVID/p3.png",
       width = 9,
       height = 6,
       units = "in",
       dpi = 500)
