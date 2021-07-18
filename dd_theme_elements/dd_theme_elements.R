# load fonts ----
library(ggtext)
extrafont::loadfonts(device = "win")

# dd colors ! ----
dd_orange <- "#F48847"
dd_black <- "#282D30"
dd_cream <- "#F9F3EB"
dd_blue <- "#5565D7"
dd_red <- "#D75565"
dd_blue_dark <- "#353F86"
dd_red_dark <- "#BC4A58"
dd_purple <- "#D755A6"
dd_gray <- "#5E6264"
dd_green <- "#65D755"

# dd theme ! ----

# must have below font installed & added with 
# extrafont::font_import(paths = C:/path/to/font/location)
dd_font <- "Roboto Slab"
dd_font_light <- "Roboto Slab"
  
dd_theme <- theme(
  text = element_text(color = dd_black),
  plot.title = element_markdown(family = dd_font, face = "bold", size = 18),
  plot.subtitle = element_markdown(family = dd_font, size = 14),
  plot.caption = element_markdown(family = dd_font),
  legend.text = element_markdown(family = dd_font),
  legend.title = element_markdown(family = dd_font),
  axis.title = element_markdown(family = dd_font, color = dd_black),
  axis.text = element_markdown(family = dd_font, color = dd_black),
  plot.title.position = "plot",
  panel.background = element_rect(fill = dd_cream),
  axis.line.x.bottom = element_line(color = dd_black),
  axis.line.y.left = element_line(color = dd_black),
  axis.ticks = element_line(color = dd_black)
)
