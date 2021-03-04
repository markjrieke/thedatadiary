# libraries ----
library(dplyr)
library(ggplot2)

# import & arrange ----
f_art <- as_tibble(read.csv("2021.03.08-aRt/artwerk.csv"))

p_art <- f_art %>%
  mutate(y = -y,
         color_code = rgb(r, g, b, maxColorValue = 255)) %>%
  ggplot() +
  geom_polygon(aes(x = x,
                   y = y,
                   group = group,
                   fill = color_code)) +
  scale_fill_identity() +
  labs(x = NULL,
       y = NULL) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank())

ggsave(filename = "p_art.png",
       path = "2021.03.08-aRt",
       plot = p_art,
       width = 24,
       height = 24,
       units = "in",
       dpi = 500)

# test area ----

# hi!
