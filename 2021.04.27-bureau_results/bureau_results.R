# libraries/setup ----
library(tidyverse)
devtools::source_url("https://raw.githubusercontent.com/markjrieke/thedatadiary/main/dd_theme_elements/dd_theme_elements.R")

# create frame ----

# import
f_reps <- read_csv("2021.04.27-bureau_results/apportionment-2020-table01.csv")
f_pops <- read_csv("2021.04.27-bureau_results/apportionment-2020-table02.csv")

f_results <- read_csv("2021.04.18-misc/state_results.csv")

# rename cols
colnames(f_reps) <- c("state", "pop", "reps", "change")
colnames(f_pops) <- c("state", "pop")

# merge
f_reps <-
  f_reps %>%
  select(state, reps)

f_results <- 
  f_results %>%
  mutate(b_pct = biden/(biden + trump)) %>%
  select(state, b_pct)

f_pops <- 
  f_pops %>%
  select(state, pop) %>%
  drop_na() %>%
  left_join(f_reps, by = "state") %>%
  replace_na(list(reps = 0)) %>%
  mutate(congress = if_else(state == "District of Columbia",
                            0,
                            reps + 2),
         pop = pop / 1000000,
         rep_per_mil = congress/pop) %>%
  left_join(f_results, by = "state") %>%
  mutate(state = fct_reorder(state, rep_per_mil))

f_pops %>%
  ggplot(aes(x = state,
             y = rep_per_mil)) +
  geom_col(fill = dd_blue_dark) +
  coord_flip() +
  dd_theme +
  theme(plot.title = element_markdown(family = "Siemens Slab", face = "bold", size = "24"),
        plot.subtitle = element_markdown(family = "Siemens Slab", size = 16),
        axis.text = element_markdown(family = "Siemens Slab", color = dd_black, size = 12)) +
  labs(title = "A Mis-balance of Power",
       subtitle = "Number of Representatives in Congress per Million residents",
       caption = "Data from Census Bureau 2020 Apportionment:
       <br>census.gov/data/tables/2020/dec/2020-apportionment-data.html",
       x = NULL,
       y = NULL) +
  annotate(geom = "label",
           label = "Wyoming is apportioned over five\nrepresentatives per million residents",
           family = "Siemens Slab",
           fontface = "bold",
           size = 5,
           fill = dd_orange,
           alpha = 0.25,
           label.size = 0,
           x = 37,
           y = 4.2) +
  annotate(geom = "label",
           label = "D.C. residents, however, receive \nno representation in Congress",
           family = "Siemens Slab",
           fontface = "bold",
           size = 5,
           fill = dd_orange,
           alpha = 0.25,
           label.size = 0,
           x = 12,
           y = 3.5) +
  annotate(geom = "curve",
           x = 37,
           y = 5.07,
           xend = 50,
           yend = 5.2,
           size = 1.4,
           curvature = 0.3,
           arrow = arrow(length = unit(3, "mm"))) +
  annotate(geom = "curve",
           x = 10.3,
           y = 3.5,
           xend = 1,
           yend = 1.4,
           size = 1.4,
           curvature = -0.22,
           arrow = arrow(length = unit(3, "mm")))

ggsave("2021.04.27-bureau_results/p1.png",
       width = 13.5,
       height = 9,
       units = "in",
       dpi = 600)
  
  
  
