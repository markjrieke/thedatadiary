# setup ----
library(tidyverse)
devtools::source_url("https://raw.githubusercontent.com/markjrieke/thedatadiary/main/dd_theme_elements/dd_theme_elements.R")

# create yougov plot ----
f_yougov <- read_csv("2021.04.18-misc/yougov_states.csv")

# create tibble of results
f_results2020 <- read_csv("2021.04.18-misc/state_results.csv")

# create labels
v_labs <- c("Democrat", "Republican")
names(v_labs) <- c("d_pct", "r_pct")

f_results2020 %>%
  mutate(b_pct = biden/(biden + trump)) %>%
  left_join(f_yougov, by = c("state" = "State")) %>%
  pivot_longer(cols = c("d_pct", "r_pct"),
               names_to = "party",
               values_to = "score") %>%
  ggplot(aes(x = b_pct,
             y = score,
             color = party)) +
  geom_smooth(method = lm, se = FALSE) +
  geom_point(size = 3,
             alpha = 0.7) +
  scale_color_manual(values = c(dd_blue, dd_red)) +
  dd_theme +
  labs(title = "More polarized opinions",
       subtitle = "<span style = 'color:#5565D7;'>**Democrat**</span> and <span style = 'color:#D75565;'>**Republican**</span> favorability of states is 
       <br>closely aligned with Biden's 2020 voteshare",
       x = "Biden's Voteshare",
       y = "State Favorability") +
  theme(legend.position = "none",
        strip.text = element_markdown(family = "Siemens Slab")) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) + 
  facet_wrap(~party,
             labeller = labeller(party = v_labs))

ggsave("2021.04.18-misc/yougov.png",
       width = 9,
       height = 6,
       units = "in",
       dpi = 500)
