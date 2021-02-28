# libraries ----
library(dplyr)
library(ggplot2)
library(maps)
library(gganimate)
library(gifski)
library(forcats)

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

# dd theme ! ----
dd_theme <- theme(text = element_text(color = dd_black),
                  plot.title = element_text(face = "bold", size = 14),
                  plot.subtitle = element_text(size = 12),
                  panel.background = element_rect(fill = dd_cream),
                  axis.line.x.bottom = element_line(color = dd_black),
                  axis.line.y.left = element_line(color = dd_black),
                  axis.text = element_text(color = dd_black),
                  axis.ticks = element_line(color = dd_black))

# dataframe setup ----

# master frame
f_senate <- as.data.frame(read.csv("data/senate_county_candidate_ga.csv"))

f_senate <- f_senate %>%
  filter(state == "Georgia") %>%
  mutate(county = gsub(x = county,
                       pattern = " County",
                       replacement = ""),
         county = tolower(county),
         county = gsub(x = county,
                       pattern = "dekalb",
                       replacement = "de kalb"))

# separate frames for regular and special election
f_regular <- f_senate %>%
  filter(candidate == "David Perdue" | candidate == "Jon Ossoff")

f_special <- f_senate %>%
  filter(candidate == "Raphael Warnock" |
           candidate == "Kelly Loeffler" |
           candidate == "Doug Collins")

# as well as separate frames for comparing dropoff
f_democrat <- f_senate %>%
  filter(candidate == "Jon Ossoff" | candidate == "Raphael Warnock")

f_republican <- f_senate %>%
  filter(candidate == "David Perdue" |
           candidate == "Kelly Loeffler" |
           candidate == "Doug Collins")

# county map ga
f_ga_county <- as_tibble(map_data("county") %>%
                           filter(region == "georgia"))


# plot 1: ossoff vs perdue county plot ----

# plot specific frame
f_p1_reg <- f_regular %>%
  group_by(county) %>%
  mutate(t_votes = sum(total_votes),
         d_pct = total_votes / t_votes) %>%
  filter(party == "DEM") %>%
  select(-total_votes, -state, -candidate, -party, - t_votes)

f_p1_reg <- f_ga_county %>%
  left_join(f_p1_reg,
            by = c("subregion" = "county"))

# add plot
p1_reg <- ggplot() +
  geom_polygon(data = f_p1_reg,
               aes(x = long,
                   y = lat, 
                   group = group,
                   fill = d_pct),
               color = "white")

# add theme & formatting
p1_reg <- p1_reg + 
  dd_theme +
  labs(title = "Georgia Regular Senate Election Results",
       subtitle = "Two-party vote share, Jon Ossoff v. David Perdue",
       caption = "Data from https://www.kaggle.com/unanimad/us-election-2020",
       x = NULL,
       y = NULL) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(color = dd_black),
        legend.title = element_text(color = dd_black,
                                    face = "bold")) +
  scale_fill_gradientn(colors = c(dd_red_dark,
                                  dd_red,
                                  dd_cream, 
                                  dd_blue,
                                  dd_blue_dark),
                       values = c(0, 0.2, 0.5, 0.8, 1),
                       limits = c(0, 1),
                       name = "%-Dem",
                       breaks = c(0.2, 0.5, 0.8),
                       labels = c("20%", "50%", "80%"))
  
ggsave(path = "plots",
       filename = "p1_reg.png",
       plot = p1_reg,
       width = 6,
       height = 6,
       units = "in",
       dpi = 500)

# plot 2: ossoff vs perdue dot plot ----

# plot specific frame
f_p2_reg <-f_regular %>%
  group_by(county) %>%
  mutate(t_votes = sum(total_votes)) %>%
  filter(party == "DEM") %>%
  select(county, t_votes)

f_p2_reg <- f_p1_reg %>%
  left_join(f_p2_reg,
            by = c("subregion" = "county"))

# add avg long/lat for counties
f_p2_reg <- f_p2_reg %>%
  group_by(group) %>%
  mutate(avglong = mean(long),
         avglat = mean(lat))

f_p2a_reg <- f_p2_reg %>%
  select(avglong, avglat, t_votes, d_pct, subregion) %>%
  ungroup() %>%
  distinct(subregion, .keep_all = TRUE)
  

# add plot
p2_reg <- ggplot() +
  geom_polygon(data = f_p2_reg,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = "white",
               color = "grey") +
  geom_point(data = f_p2a_reg,
             aes(x = avglong,
                 y = avglat,
                 group = group,
                 size = t_votes,
                 color = d_pct),
             alpha = 0.7) +
  scale_size(range = c(0, 20),
             guide = FALSE)

# add theme and formatting
p2_reg <- p2_reg + 
  dd_theme +
  labs(title = "Georgia Regular Senate Election Results",
       subtitle = "Two-party vote share, Jon Ossoff v. David Perdue",
       caption = "Data from https://www.kaggle.com/unanimad/us-election-2020",
       x = NULL,
       y = NULL) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(color = dd_black),
        legend.title = element_text(color = dd_black,
                                    face = "bold")) +
  scale_color_gradientn(colors = c(dd_red_dark,
                                  dd_red,
                                  dd_cream, 
                                  dd_blue,
                                  dd_blue_dark),
                       values = c(0, 0.2, 0.5, 0.8, 1),
                       limits = c(0, 1),
                       name = "%-Dem",
                       breaks = c(0.2, 0.5, 0.8),
                       labels = c("20%", "50%", "80%"))

ggsave(path = "plots",
       filename = "p2_reg.png",
       plot = p2_reg,
       width = 6,
       height = 6,
       units = "in",
       dpi = 500)

# plot 3: warnock vs republicans county plot ----

# plot frame
as_tibble(f_special)

f_p3_spc <- f_special %>%
  group_by(county) %>%
  mutate(t_votes = sum(total_votes),
         d_pct = total_votes / t_votes) %>%
  filter(party == "DEM") %>%
  select(county, d_pct)

f_p3_spc <- f_ga_county %>%
  left_join(f_p3_spc,
            by = c("subregion" = "county"))

# add plot
p3_spc <- ggplot() +
  geom_polygon(data = f_p3_spc,
               aes(x = long,
                   y = lat, 
                   group = group,
                   fill = d_pct),
               color = "white")

# add theme and formatting
p3_spc <- p3_spc + 
  dd_theme +
  labs(title = "Georgia Special Senate Election Results",
       subtitle = "Two-party vote share, Raphael Warnock v. Republicans",
       caption = "Data from https://www.kaggle.com/unanimad/us-election-2020",
       x = NULL,
       y = NULL) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(color = dd_black),
        legend.title = element_text(color = dd_black,
                                    face = "bold")) +
  scale_fill_gradientn(colors = c(dd_red_dark,
                                   dd_red,
                                   dd_cream, 
                                   dd_blue,
                                   dd_blue_dark),
                        values = c(0, 0.2, 0.5, 0.8, 1),
                        limits = c(0, 1),
                        name = "%-Dem",
                        breaks = c(0.2, 0.5, 0.8),
                        labels = c("20%", "50%", "80%"))

ggsave(path = "plots",
       filename = "p3_spc.png",
       plot = p3_spc,
       width = 6,
       height = 6,
       units = "in",
       dpi = 500)

# plot 4: warnock vs republicans dot plot ----

# plot specific frame
f_p4_spc <- f_special %>%
  group_by(county) %>%
  mutate(t_votes = sum(total_votes)) %>%
  filter(party == "DEM") %>%
  select(county, t_votes)

f_p4_spc <- f_p3_spc %>%
  left_join(f_p4_spc,
            by = c("subregion" = "county"))

# add avg long/lat for counties
f_p4_spc <- f_p4_spc %>%
  group_by(group) %>%
  mutate(avglong = mean(long),
         avglat = mean(lat))

f_p4a_spc <- f_p4_spc %>%
  select(avglong, avglat, t_votes, d_pct, subregion) %>%
  ungroup() %>%
  distinct(subregion, .keep_all = TRUE)


# add plot
p4_spc <- ggplot() +
  geom_polygon(data = f_p4_spc,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = "white",
               color = "grey") +
  geom_point(data = f_p4a_spc,
             aes(x = avglong,
                 y = avglat,
                 group = group,
                 size = t_votes,
                 color = d_pct),
             alpha = 0.7) +
  scale_size(range = c(0, 20),
             guide = FALSE)

# add theme and formatting
p4_spc <- p4_spc + 
  dd_theme +
  labs(title = "Georgia Special Senate Election Results",
       subtitle = "Two-party vote share, Raphael Warnock v. Republicans",
       caption = "Data from https://www.kaggle.com/unanimad/us-election-2020",
       x = NULL,
       y = NULL) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(color = dd_black),
        legend.title = element_text(color = dd_black,
                                    face = "bold")) +
  scale_color_gradientn(colors = c(dd_red_dark,
                                   dd_red,
                                   dd_cream, 
                                   dd_blue,
                                   dd_blue_dark),
                        values = c(0, 0.2, 0.5, 0.8, 1),
                        limits = c(0, 1),
                        name = "%-Dem",
                        breaks = c(0.2, 0.5, 0.8),
                        labels = c("20%", "50%", "80%"))

ggsave(path = "plots",
       filename = "p4_spc.png",
       plot = p4_spc,
       width = 6,
       height = 6,
       units = "in",
       dpi = 500)

# plot 5: warnock vs loeffler county plot ----

f_p5_spc <- f_special %>%
  filter(candidate != "Doug Collins") %>%
  group_by(county) %>%
  mutate(t_votes = sum(total_votes),
         d_pct = total_votes / t_votes) %>%
  filter(party == "DEM") %>%
  select(county, d_pct)

f_p5_spc <- f_ga_county %>%
  left_join(f_p5_spc,
            by = c("subregion" = "county"))

# add plot
p5_spc <- ggplot() +
  geom_polygon(data = f_p5_spc,
               aes(x = long,
                   y = lat, 
                   group = group,
                   fill = d_pct),
               color = "white")

# add theme and formatting
p5_spc <- p5_spc + 
  dd_theme +
  labs(title = "Georgia Special Senate Election Results",
       subtitle = "Two-party vote share, Raphael Warnock v. Kelly Loeffler",
       caption = "Data from https://www.kaggle.com/unanimad/us-election-2020",
       x = NULL,
       y = NULL) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(color = dd_black),
        legend.title = element_text(color = dd_black,
                                    face = "bold")) +
  scale_fill_gradientn(colors = c(dd_red_dark,
                                  dd_red,
                                  dd_cream, 
                                  dd_blue,
                                  dd_blue_dark),
                       values = c(0, 0.2, 0.5, 0.8, 1),
                       limits = c(0, 1),
                       name = "%-Dem",
                       breaks = c(0.2, 0.5, 0.8),
                       labels = c("20%", "50%", "80%"))

ggsave(path = "plots",
       filename = "p5_spc.png",
       plot = p5_spc,
       width = 6,
       height = 6,
       units = "in",
       dpi = 500)

# plot 6: warnock vs loeffler dot plot ----

# plot specific frame
f_p6_spc <- as_tibble(f_special) %>%
  filter(candidate!= "Doug Collins") %>%
  group_by(county) %>%
  mutate(t_votes = sum(total_votes)) %>%
  filter(party == "DEM") %>%
  select(county, t_votes)

f_p6_spc <- f_p5_spc %>%
  left_join(f_p6_spc,
            by = c("subregion" = "county"))

# add avg long/lat for counties
f_p6_spc <- f_p6_spc %>%
  group_by(group) %>%
  mutate(avglong = mean(long),
         avglat = mean(lat))

f_p6a_spc <- f_p6_spc %>%
  select(avglong, avglat, t_votes, d_pct, subregion) %>%
  ungroup() %>%
  distinct(subregion, .keep_all = TRUE)


# add plot
p6_spc <- ggplot() +
  geom_polygon(data = f_p6_spc,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = "white",
               color = "grey") +
  geom_point(data = f_p6a_spc,
             aes(x = avglong,
                 y = avglat,
                 group = group,
                 size = t_votes,
                 color = d_pct),
             alpha = 0.7) +
  scale_size(range = c(0, 20),
             guide = FALSE)

# add theme and formatting
p6_spc <- p6_spc + 
  dd_theme +
  labs(title = "Georgia Special Senate Election Results",
       subtitle = "Two-party vote share, Raphael Warnock v. Kelly Loeffler",
       caption = "Data from https://www.kaggle.com/unanimad/us-election-2020",
       x = NULL,
       y = NULL) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(color = dd_black),
        legend.title = element_text(color = dd_black,
                                    face = "bold")) +
  scale_color_gradientn(colors = c(dd_red_dark,
                                   dd_red,
                                   dd_cream, 
                                   dd_blue,
                                   dd_blue_dark),
                        values = c(0, 0.2, 0.5, 0.8, 1),
                        limits = c(0, 1),
                        name = "%-Dem",
                        breaks = c(0.2, 0.5, 0.8),
                        labels = c("20%", "50%", "80%"))

ggsave(path = "plots",
       filename = "p6_spc.png",
       plot = p6_spc,
       width = 6,
       height = 6,
       units = "in",
       dpi = 500)

# plot 7: dem v rep dropoff ----

f_democrat <- as_tibble(f_democrat) %>% 
  group_by(county) %>%
  mutate(t_vote = sum(total_votes)) %>%
  ungroup() %>%
  mutate(o_vote = t_vote - total_votes,
         w_pct = total_votes/o_vote) %>%
  filter(candidate == "Raphael Warnock") %>%
  select(county, w_pct, t_vote)

f_p7 <- as_tibble(f_republican) %>%
  arrange(county) %>%
  group_by(county) %>%
  mutate(tot_vote = sum(total_votes)) %>%
  ungroup() %>%
  mutate(r_vote = tot_vote - total_votes,
         r_pct = r_vote/total_votes) %>%
  filter(candidate == "David Perdue") %>%
  select(county, r_pct, tot_vote)
  
f_p7 <- f_democrat %>%
  left_join(f_p7, by = "county") %>%
  mutate(total_votes = t_vote + tot_vote) %>%
  select(-t_vote, -tot_vote) %>%
  mutate(score = w_pct - r_pct) %>%
  arrange(desc(total_votes))

# add plot
p7 <- ggplot(f_p7,
             aes(x = w_pct,
                 y = r_pct,
                 size = total_votes,
                 color = score)) +
  geom_point(alpha = 0.5)

# add theme formatting
p7 <- p7 + 
  dd_theme +
  labs(title = "Special Election Voter Retention",
       subtitle = "Comparing Warnock's retention with both republicans.",
       x = "Democratic votes retained",
       y = "Republican votes retained",
       caption = "Data from https://www.kaggle.com/unanimad/us-election-2020") +
  geom_abline(linetype = "dashed",
              size = 0.75) +
  scale_size(range = c(0, 25),
             guide = FALSE) +  
  scale_color_gradientn(colors = c(dd_red_dark,
                                   dd_red,
                                   "grey",
                                   dd_blue,
                                   dd_blue_dark),
                        values = c(0, 0.35, 0.5, 0.65, 1),
                        limits = c(-1, 1),
                        name = element_blank(),
                        breaks = NULL) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.2, 0.8)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.2, 1))

ggsave(path = "plots",
       filename = "p7_static.png",
       plot = p7,
       width = 9,
       height = 6,
       units = "in",
       dpi = 500)

# plot 8: warnock vs. loeffler dropoff ----

f_p8 <- as_tibble(f_republican) %>%
  filter(candidate != "Doug Collins") %>%
  group_by(county) %>%
  mutate(tot_votes = sum(total_votes),
         p_votes = tot_votes - total_votes,
         l_pct = total_votes/p_votes) %>%
  ungroup() %>%
  filter(candidate == "Kelly Loeffler") %>%
  select(county, tot_votes, l_pct)

f_p8 <- f_democrat %>%
  left_join(f_p8, by = "county") %>%
  mutate(total_votes = t_vote + tot_votes,
         score = w_pct - l_pct) %>%
  select(-t_vote, -tot_votes)

# add plot 
p8 <- ggplot(f_p8,
       aes(x = w_pct,
           y = l_pct,
           size = total_votes,
           color = score)) +
  geom_point(alpha = 0.5)

# add theme and formattign
p8 <- p8 + 
  dd_theme +
  labs(title = "Special Election Voter Retention",
       subtitle = "Comparing Warnock's retention with just Loeffler.",
       x = "Democratic votes retained",
       y = "Republican votes retained",
       caption = "Data from https://www.kaggle.com/unanimad/us-election-2020") +
  geom_abline(linetype = "dashed",
              size = 0.75,
              color = dd_gray) +
  scale_size(range = c(0, 25),
             guide = FALSE) +  
  scale_color_gradientn(colors = c(dd_red_dark,
                                   dd_red,
                                   "grey",
                                   dd_blue,
                                   dd_blue_dark),
                        values = c(0, 0.35, 0.5, 0.65, 1),
                        limits = c(-1, 1),
                        name = element_blank(),
                        breaks = NULL) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.2, 0.8)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.2, 1))

ggsave(path = "plots",
       filename = "p8_static.png",
       plot = p8,
       width = 9,
       height = 6,
       units = "in",
       dpi = 500)

# gganimation of 7/8 ----
f_p7 <- f_p7 %>%
  mutate(race = "both republicans.")

f_p8 <- f_p8 %>%
  mutate(race = "just Loeffler.")

colnames(f_p8) <- colnames(f_p7)
f_p9 <- rbind(f_p7, f_p8)

f_p9 <- f_p9 %>%
  arrange(county)

# base plot
p9 <- ggplot(f_p9,
       aes(x = w_pct,
           y = r_pct,
           size = total_votes,
           color = score)) +
  geom_point(alpha = 0.5)

# formatting and annotations
p9 <- p9 + 
  dd_theme +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 8)) +
  labs(title = "Special Election Voter Retention",
       subtitle = "Comparing Warnock's retention with {closest_state}",
       x = "Democratic votes retained",
       y = "Republican votes retained",
       caption = "Data from https://www.kaggle.com/unanimad/us-election-2020") +
  geom_abline(linetype = "dashed",
              size = 0.75,
              color = dd_gray) +
  scale_size(range = c(0, 25),
             guide = FALSE) +  
  scale_color_gradientn(colors = c(dd_red_dark,
                                   dd_red,
                                   "grey",
                                   dd_blue,
                                   dd_blue_dark),
                        values = c(0, 0.35, 0.5, 0.65, 1),
                        limits = c(-1, 1),
                        name = element_blank(),
                        breaks = NULL) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.2, 0.8)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.2, 1))

# gganimating
p9 <- p9 + 
  transition_states(state = race,
                    transition_length = 3,
                    state_length = 2) +
  ease_aes("cubic-in-out")

p9 <- animate(plot = p9,
        renderer = gifski_renderer(),
        duration = 6,
        fps = 30,
        width = 9,
        height = 6,
        units = "in",
        res = 150)

anim_save(path = "plots",
          animation = p9,
          filename = "p9_anim.gif")

# search history plot ----

# import google search history frame
f_goog <- as_tibble(read.csv("data/multiTimeline.csv"))

colnames(f_goog) <- c("Day", "Collins", "Loeffler")

f_goog <- f_goog %>%
  mutate(Collins = as.numeric(Collins),
         Loeffler = as.numeric(Loeffler),
         Day = as.Date(Day, "%m/%d/%Y"))

# plotting
p_goog <- ggplot(f_goog) +
  geom_line(aes(x = Day,
                y = Collins),
            color = dd_orange,
            size = 1) +
  geom_line(aes(x = Day,
                y = Loeffler),
            color = dd_red,
            size = 1)

p_goog <- p_goog +
  dd_theme +
  labs(title = "Relative Interest up to Election Day",
       subtitle = "Goolge search history of \"Kelly Loeffler\" and \"Doug Collins\"",
       caption = "Data from trends.google.com",
       x = NULL,
       y = NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

p_goog <- p_goog +
  annotate(geom = "label",
           x = as.Date("2020-10-1"),
           y = 100,
           hjust = "left",
           vjust = "top",
           label = "Loeffler      \nCollins      ",
           fontface = "bold",
           family = "mono",
           fill = dd_orange,
           alpha = 0.25,
           label.size = 0) +
  annotate(geom = "text",
           x = as.Date("2020-10-7"),
           y = 98,
           size = 10,
           label = "-",
           fontface = "bold",
           family = "mono",
           color = dd_red) +
  annotate(geom = "text",
           x = as.Date("2020-10-7"),
           y = 92.5,
           size = 10,
           label = "-",
           fontface = "bold",
           family = "mono",
           color = dd_orange)

ggsave(path = "plots",
       filename = "p_goog.png",
       plot = p_goog,
       width = 9,
       height = 6,
       units = "in",
       dpi = 500)

# state results plot ----

f_res <- tibble(candidate = c("Warnock", "Loeffler", "Collins", "Other"),
                pct = c(0.329, 0.259, 0.2, 0.212))

p_res <- f_res %>%
  mutate(candidate = fct_reorder(candidate, pct)) %>%
  ggplot(aes(x = candidate,
             y = pct,
             fill = candidate)) +
  geom_col() +
  coord_flip() + 
  scale_fill_manual(values = c(dd_orange, dd_gray, dd_red, dd_blue)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  dd_theme +
  labs(title = "GA Special Election Results",
       subtitle = "Voteshare of major candidates",
       caption = "Data from https://www.nytimes.com/interactive/2020/11/03/us/elections/results-georgia-senate-special.html",
       x = NULL,
       y = NULL) +
  theme(legend.position = "none")

ggsave(path = "plots",
       filename = "p_res.png",
       plot = p_res,
       width = 9,
       height = 6,
       units = "in",
       dpi = 500)

# overall retention plot ----
f_ret <- tibble(w_pct = rep(1617035/2374519, 2),
                r_pct = c((1273214 + 980454)/2462617,
                          1273214/2462617))

p_ret <- f_ret %>%
  mutate(score = w_pct - r_pct) %>%
  ggplot(aes(x = w_pct,
             y = r_pct,
             color = score)) + 
  geom_point(alpha = 0.5,
             size = 8) +
  scale_color_gradientn(colors = c(dd_red_dark,
                                   dd_red,
                                   "grey",
                                   dd_blue,
                                   dd_blue_dark),
                        values = c(0, 0.35, 0.5, 0.65, 1),
                        limits = c(-1, 1),
                        name = element_blank(),
                        breaks = NULL) +
  geom_abline(linetype = "dashed",
              size = 0.75,
              color = dd_gray) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.5, 0.95)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.65, 0.71)) +
  dd_theme +
  labs(title = "Special Election Voter Retention",
       subtitle = "Comparing relative improvement to regular election",
       x = "Democratic votes retained",
       y = "Republican votes retained",
       caption = "Data from https://www.nytimes.com/interactive/2020/11/03/us/elections/results-georgia-senate-special.html") +
  annotate(geom = "label",
           label = "Warnock retention vs\nrepublican retention",
           x = 0.66,
           y = 0.95,
           hjust = "left",
           vjust = "top",
           family = "mono",
           fontface = "bold",
           label.size = 0,
           fill = dd_orange,
           alpha = 0.25) +
  annotate(geom = "label",
           label = "Warnock retention vs\nLoeffler retention",
           x = 0.66,
           y = 0.55,
           hjust = "left",
           vjust = "top",
           family = "mono",
           fontface = "bold",
           label.size = 0,
           fill = dd_orange,
           alpha = 0.25) +
  annotate(geom = "curve",
           x = 0.6809,
           y = 0.89,
           xend = 0.6809,
           yend = 0.54,
           curvature = 0,
           size = 1.1,
           arrow = arrow(length = unit(3, "mm")),
           color = dd_gray) +
  annotate(geom = "text",
           x = 0.69,
           y = 0.71,
           label = "Republican relative improvement",
           family = "mono",
           fontface = "bold",
           color = dd_black,
           hjust = "left",
           angle = 3.5) +
  annotate(geom = "text",
           x = 0.69,
           y = 0.678,
           label = "Democratic relative improvement",
           hjust = "left",
           family = "mono",
           fontface = "bold",
           color = dd_black,
           angle = 3.5)

ggsave(path = "plots",
       filename = "p_ret.png",
       plot = p_ret,
       width = 9,
       height = 6,
       units = "in",
       dpi = 500)


# test area ----

# hi!