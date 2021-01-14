#install packages ----
install.packages("dplyr")
install.packages("plotly")
install.packages("devtools")
install.packages("Rtools")
library(dplyr)
library(plotly)

# setting up data ----
ev_import <- read.csv('electoral_vote_training.csv')
plot(ev_import)
plot(ev_import$popular_vote_pct, ev_import$electoral_vote_pct)

summary(ev_import)

#manipulate ev_import dataframe to exclude pre-1952 data ----
ev_summary <- ev_import %>% 
  filter(year > 1950)

ev_summary
plot(ev_summary)

ev_summary <- ev_summary %>%
  filter(electoral_vote != 0)
ev_summary

plot(ev_summary$popular_vote_pct, ev_summary$electoral_vote_pct)

ev_summary <- ev_summary %>%
  filter(party != "Other")
ev_summary

ev_summary <- ev_summary %>%
  select(-popular_vote, -electoral_vote)

# ggplot ----
ev_plot <- ggplot(ev_summary, 
                  aes(x = popular_vote_pct, 
                      y = electoral_vote_pct, 
                      fill = party,
                      color = party,
                      text = sprintf("%s, %s<br>Pop Vote: %s<br>Elec Vote: %s",
                                     candidate,
                                     year,
                                     scales::percent(popular_vote_pct, accuracy = 0.1),
                                     scales::percent(electoral_vote_pct, accuracy = 0.1)))) + 
  geom_point(size = 5, shape = 21, stroke = 0.5) + 
  scale_fill_manual(values = c(hcl(240, 89.7, 54.5, 0.5), hcl(0, 89.7, 54.5, 0.5))) +
  scale_color_manual(values = c(hcl(240, 89.7, 54.5, 1), hcl(0, 89.7, 54.5, 1))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggtitle("US Presidential Elections", subtitle = "Results from 1952 to 2020") + 
  xlab("Popular Vote Won") + 
  ylab("Electoral Vote Won")


ev_plot

# interactive plot ----

ev_plot_int <- ggplotly(ev_plot, tooltip = "text")

ev_plot_int

ev_plot_int <- ev_plot_int %>%
  layout(title = list(xanchor = "left",
                      x = 0.0,
                      y = 0.97,
                      text = paste0("US Presidential Election Results",
                                    "<br>",
                                    "<sup>",
                                    "Results from 1952 to 2020",
                                    "</sup>")))

         
ev_plot_int

?layout

?theme

ev_summary
