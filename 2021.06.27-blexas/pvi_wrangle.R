# libraries ----
library(tidyverse)
library(zoo)
library(geofacet)

# theme ----
source("dd_theme_elements/dd_theme_elements.R")

# import data ----
f_1960 <-
  read_csv("2021.06.27-blexas/1960_election_by_state.csv") %>%
  rename(state = State,
         d_votes = `Democratic Votes`,
         r_votes = `Republican Votes`) %>%
  select(state, d_votes, r_votes) %>%
  filter(state != "TOTALS:") %>%
  mutate(year = 1960) %>%
  relocate(year)

f_1964 <- 
  read_csv("2021.06.27-blexas/1964_election_by_state.csv") %>%
  rename(state = State,
         d_votes = `Democratic Votes`,
         r_votes = `Republican Votes`) %>%
  mutate(d_votes = if_else(state == "Alabama", `Unpledged Electors Unpledged Democratic Number`, d_votes)) %>%
  select(state, d_votes, r_votes) %>%
  filter(state != "TOTALS:") %>%
  mutate(year = 1964,
         state = str_replace(state, "D.C.", "District of Columbia"),
         d_votes = str_remove_all(d_votes,","),
         d_votes = as.numeric(d_votes)) %>%
  relocate(year) 

f_1968 <- 
  read_csv("2021.06.27-blexas/1968_election_by_state.csv") %>%
  rename(state = State,
         d_votes = `Democrat Votes`,
         r_votes = `Republican Votes`) %>%
  select(state, d_votes, r_votes) %>%
  filter(state != "TOTALS:") %>%
  mutate(state = str_replace(state, "D.C.", "District of Columbia"),
         year = 1968) %>%
  relocate(year)

f_1972 <- 
  read_csv("2021.06.27-blexas/1972_election_by_state.csv") %>%
  rename(state = State,
         d_votes = `Democrat Votes`,
         r_votes = `Republican Votes`) %>%
  select(state, d_votes, r_votes) %>%
  filter(state != "TOTALS:") %>%
  mutate(state = str_replace(state, "D.C.", "District of Columbia"),
         state = if_else(str_detect(state, "Maine") == TRUE, "Maine", state)) %>%
  group_by(state) %>%
  mutate(d_tot = sum(d_votes),
         r_tot = sum(r_votes)) %>%
  ungroup() %>%
  mutate(d_votes = d_tot,
         r_votes = r_tot) %>%
  select(-d_tot, -r_tot) %>%
  distinct(state, .keep_all = TRUE) %>%
  mutate(year = 1972) %>%
  relocate(year)

f_1976 <- 
  read_csv("2021.06.27-blexas/1976_election_by_state.csv") %>%
  rename(state = State,
         d_votes = `Democrat Votes`,
         r_votes = `Republican Votes`) %>%
  select(state, d_votes, r_votes) %>%
  filter(state != "TOTALS:") %>%
  mutate(state = str_replace(state, "D.C.", "District of Columbia"),
         state = if_else(str_detect(state, "Maine") == TRUE, "Maine", state)) %>%
  group_by(state) %>%
  mutate(d_tot = sum(d_votes),
         r_tot = sum(r_votes)) %>%
  ungroup() %>%
  mutate(d_votes = d_tot,
         r_votes = r_tot) %>%
  select(-d_tot, -r_tot) %>%
  distinct(state, .keep_all = TRUE) %>%
  mutate(year = 1976) %>%
  relocate(year)

f_1980 <- 
  read_csv("2021.06.27-blexas/1980_election_by_state.csv") %>%
  rename(state = State,
         d_votes = `Democrat Votes`, 
         r_votes = `Republican Votes`) %>%
  select(state, d_votes, r_votes) %>%
  filter(state != "TOTALS:") %>%
  mutate(state = str_replace(state, "D.C.", "District of Columbia"),
         state = if_else(str_detect(state, "Maine") == TRUE, "Maine", state)) %>%
  group_by(state) %>%
  mutate(d_tot = sum(d_votes),
         r_tot = sum(r_votes)) %>%
  ungroup() %>%
  mutate(d_votes = d_tot,
         r_votes = r_tot) %>%
  select(-d_tot, -r_tot) %>%
  distinct(state, .keep_all = TRUE) %>%
  mutate(year = 1980) %>%
  relocate(year)

f_1984 <-
  read_csv("2021.06.27-blexas/1984_election_by_state.csv") %>%
  rename(state = State,
         d_votes = `Democrat Votes`, 
         r_votes = `Republican Votes`) %>%
  select(state, d_votes, r_votes) %>%
  filter(state != "TOTALS:") %>%
  mutate(state = str_replace(state, "D.C.", "District of Columbia"),
         state = if_else(str_detect(state, "Maine") == TRUE, "Maine", state)) %>%
  group_by(state) %>%
  mutate(d_tot = sum(d_votes),
         r_tot = sum(r_votes)) %>%
  ungroup() %>%
  mutate(d_votes = d_tot,
         r_votes = r_tot) %>%
  select(-d_tot, -r_tot) %>%
  distinct(state, .keep_all = TRUE) %>%
  mutate(year = 1984) %>%
  relocate(year)

f_1988 <- 
  read_csv("2021.06.27-blexas/1988_election_by_state.csv") %>%
  rename(state = State,
         d_votes = `Democrat Votes`, 
         r_votes = `Republican Votes`) %>%
  select(state, d_votes, r_votes) %>%
  filter(state != "TOTALS:") %>%
  mutate(state = str_replace(state, "D.C.", "District of Columbia"),
         state = if_else(str_detect(state, "Maine") == TRUE, "Maine", state)) %>%
  group_by(state) %>%
  mutate(d_tot = sum(d_votes),
         r_tot = sum(r_votes)) %>%
  ungroup() %>%
  mutate(d_votes = d_tot,
         r_votes = r_tot) %>%
  select(-d_tot, -r_tot) %>%
  distinct(state, .keep_all = TRUE) %>%
  mutate(year = 1988) %>%
  relocate(year)

f_1992 <- 
  read_csv("2021.06.27-blexas/1992_election_by_state.csv") %>%
  rename(state = State,
         d_votes = `Democrat Votes`, 
         r_votes = `Republican Votes`) %>%
  select(state, d_votes, r_votes) %>%
  filter(state != "TOTALS:") %>%
  mutate(state = str_replace(state, "D.C.", "District of Columbia"),
         state = if_else(str_detect(state, "Maine") == TRUE, "Maine", state),
         state = if_else(str_detect(state, "Nebraska") == TRUE, "Nebraska", state)) %>%
  group_by(state) %>%
  mutate(d_tot = sum(d_votes),
         r_tot = sum(r_votes)) %>%
  ungroup() %>%
  mutate(d_votes = d_tot,
         r_votes = r_tot) %>%
  select(-d_tot, -r_tot) %>%
  distinct(state, .keep_all = TRUE) %>%
  mutate(year = 1992) %>%
  relocate(year)

f_1996 <- 
  read_csv("2021.06.27-blexas/1996_election_by_state.csv") %>%
  rename(state = State,
         d_votes = `Democrat Votes`, 
         r_votes = `Republican Votes`) %>%
  select(state, d_votes, r_votes) %>%
  filter(state != "TOTALS:") %>%
  mutate(state = str_replace(state, "D.C.", "District of Columbia"),
         state = if_else(str_detect(state, "Maine") == TRUE, "Maine", state),
         state = if_else(str_detect(state, "Nebraska") == TRUE, "Nebraska", state)) %>%
  group_by(state) %>%
  mutate(d_tot = sum(d_votes),
         r_tot = sum(r_votes)) %>%
  ungroup() %>%
  mutate(d_votes = d_tot,
         r_votes = r_tot) %>%
  select(-d_tot, -r_tot) %>%
  distinct(state, .keep_all = TRUE) %>%
  mutate(year = 1996) %>%
  relocate(year)

f_2000 <- 
  read_csv("2021.06.27-blexas/2000_election_by_state.csv") %>%
  rename(state = State,
         d_votes = `Democrat Votes`, 
         r_votes = `Republican Votes`) %>%
  select(state, d_votes, r_votes) %>%
  filter(state != "Totals") %>%
  mutate(state = str_replace(state, "D.C.", "District of Columbia"),
         state = str_remove(state, "\\*"),
         state = if_else(str_detect(state, "Maine") == TRUE, "Maine", state),
         state = if_else(str_detect(state, "Nebraska") == TRUE, "Nebraska", state)) %>%
  group_by(state) %>%
  mutate(d_tot = sum(d_votes),
         r_tot = sum(r_votes)) %>%
  ungroup() %>%
  mutate(d_votes = d_tot,
         r_votes = r_tot) %>%
  select(-d_tot, -r_tot) %>%
  distinct(state, .keep_all = TRUE) %>%
  mutate(year = 2000) %>%
  relocate(year)

f_2004 <- 
  read_csv("2021.06.27-blexas/2004_election_by_state.csv") %>%
  rename(state = State,
         d_votes = `Democrat Votes`, 
         r_votes = `Republican Votes`) %>%
  select(state, d_votes, r_votes) %>%
  filter(state != "U.S Total") %>%
  mutate(state = str_replace(state, "D.C.", "District of Columbia"),
         state = if_else(str_detect(state, "Maine") == TRUE, "Maine", state),
         state = if_else(str_detect(state, "Nebraska") == TRUE, "Nebraska", state)) %>%
  group_by(state) %>%
  mutate(d_tot = sum(d_votes),
         r_tot = sum(r_votes)) %>%
  ungroup() %>%
  mutate(d_votes = d_tot,
         r_votes = r_tot) %>%
  select(-d_tot, -r_tot) %>%
  distinct(state, .keep_all = TRUE) %>%
  mutate(year = 2004) %>%
  relocate(year)

f_2008 <-
  read_csv("2021.06.27-blexas/2008_election_by_state.csv") %>%
  rename(state = State,
         d_votes = `Democrat Votes`, 
         r_votes = `Republican Votes`) %>%
  select(state, d_votes, r_votes) %>%
  filter(state != "U.S. Total") %>%
  mutate(state = str_replace(state, "D.C.", "District of Columbia"),
         state = if_else(str_detect(state, "Maine") == TRUE, "Maine", state),
         state = if_else(str_detect(state, "Nebraska") == TRUE, "Nebraska", state)) %>%
  group_by(state) %>%
  mutate(d_tot = sum(d_votes),
         r_tot = sum(r_votes)) %>%
  ungroup() %>%
  mutate(d_votes = d_tot,
         r_votes = r_tot) %>%
  select(-d_tot, -r_tot) %>%
  distinct(state, .keep_all = TRUE) %>%
  mutate(year = 2008) %>%
  relocate(year)

f_2012 <- 
  read_csv("2021.06.27-blexas/2012_election_by_state.csv") %>%
  rename(state = State,
         d_votes = `Democrat Votes`, 
         r_votes = `Republican Votes`) %>%
  select(state, d_votes, r_votes) %>%
  filter(state != "U.S. Total") %>%
  mutate(state = str_replace(state, "D.C.", "District of Columbia"),
         state = if_else(str_detect(state, "Maine") == TRUE, "Maine", state),
         state = if_else(str_detect(state, "Nebraska") == TRUE, "Nebraska", state)) %>%
  group_by(state) %>%
  mutate(d_tot = sum(d_votes),
         r_tot = sum(r_votes)) %>%
  ungroup() %>%
  mutate(d_votes = d_tot,
         r_votes = r_tot) %>%
  select(-d_tot, -r_tot) %>%
  distinct(state, .keep_all = TRUE) %>%
  mutate(year = 2012) %>%
  relocate(year)

f_2016 <- 
  read_csv("2021.06.27-blexas/2016_election_by_state.csv") %>%
  rename(state = State,
         d_votes = `Democrat Votes`, 
         r_votes = `Republican Votes`) %>%
  select(state, d_votes, r_votes) %>%
  filter(state != "Total") %>%
  mutate(state = str_replace(state, "D.C.", "District of Columbia"),
         state = if_else(str_detect(state, "Maine") == TRUE, "Maine", state),
         state = if_else(str_detect(state, "Nebraska") == TRUE, "Nebraska", state)) %>%
  group_by(state) %>%
  mutate(d_tot = sum(d_votes),
         r_tot = sum(r_votes)) %>%
  ungroup() %>%
  mutate(d_votes = d_tot,
         r_votes = r_tot) %>%
  select(-d_tot, -r_tot) %>%
  distinct(state, .keep_all = TRUE) %>%
  mutate(year = 2016) %>%
  relocate(year)

f_2020 <- 
  read_csv("2021.06.27-blexas/2020_election_by_state.csv") %>%
  rename(state = State,
         d_votes = `Democrat Votes`, 
         r_votes = `Republican Votes`) %>%
  select(state, d_votes, r_votes) %>%
  filter(state != "Total") %>%
  mutate(state = str_replace(state, "D.C.", "District of Columbia"),
         state = if_else(str_detect(state, "Maine") == TRUE, "Maine", state),
         state = if_else(str_detect(state, "Nebraska") == TRUE, "Nebraska", state)) %>%
  group_by(state) %>%
  mutate(d_tot = sum(d_votes),
         r_tot = sum(r_votes)) %>%
  ungroup() %>%
  mutate(d_votes = d_tot,
         r_votes = r_tot) %>%
  select(-d_tot, -r_tot) %>%
  distinct(state, .keep_all = TRUE) %>%
  mutate(year = 2020) %>%
  relocate(year)

# merge & cleanup environment ----
f_elections <- 
  bind_rows(f_1960, f_1964, f_1968, f_1972,
            f_1976, f_1980, f_1984, f_1988,
            f_1992, f_1996, f_2000, f_2004,
            f_2008, f_2012, f_2016, f_2020)

rm(f_1960, f_1964, f_1968, f_1972,
   f_1976, f_1980, f_1984, f_1988,
   f_1992, f_1996, f_2000, f_2004,
   f_2008, f_2012, f_2016, f_2020)

# wrangle ----

# get pvi
f_elections <-
  f_elections %>%
  group_by(year) %>%
  mutate(d_tot = sum(d_votes),
         r_tot = sum(r_votes)) %>%
  ungroup() %>%
  mutate(d_pct_nat = d_tot/(d_tot + r_tot),
         d_pct_state = d_votes/(d_votes + r_votes)) %>%
  select(year, state, d_pct_nat, d_pct_state) %>%
  arrange(state, year) %>%
  group_by(state) %>%
  mutate(d_pct_nat2 = rollmean(d_pct_nat, 2, fill = NA, align = "right"),
         d_pct_state2 = rollmean(d_pct_state, 2, fill = NA, align = "right")) %>%
  ungroup() %>%
  mutate(cpvi = d_pct_state2 - d_pct_nat2) 

# get 10 closest states
v_close_states <- 
  f_elections %>%
  filter(year == 2020) %>%
  mutate(d_close = abs(d_pct_state - 0.5)) %>%
  arrange(d_close) %>%
  slice_head(n = 10) %>%
  select(state) %>%
  pull(state)

# cleaning up f_elections
f_cpvi <- 
  f_elections %>%
  select(year, state, cpvi)

# TX/all pvi plot ----
f_cpvi %>%
  group_by(year) %>%
  mutate(cpvi_avg = mean(cpvi)) %>%
  ungroup() %>%
  select(year, cpvi_avg) %>%
  distinct(year, .keep_all = TRUE) %>%
  drop_na() %>%
  mutate(state = "cpvi_avg") %>%
  relocate(state, .after = year) %>%
  rename(cpvi = cpvi_avg) %>%
  bind_rows(f_cpvi) %>%
  mutate(group = if_else(state == "cpvi_avg" | state == "Texas", state, "other")) %>%
  ggplot(aes(x = year,
             y = cpvi,
             group = state,
             color = group,
             alpha = group,
             size = group)) +
  geom_line() +
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             color = dd_black,
             size = 1) +
  scale_size_manual(values = c(1.1, 0.9, 1.1)) +
  scale_color_manual(values = c(dd_orange, dd_gray, dd_blue_dark)) +
  scale_alpha_manual(values = c(1, 0.15, 1)) +
  scale_y_continuous(labels = scales::percent_format()) +
  dd_theme +
  theme(legend.position = "none") +
  labs(title = "Texas approaches PVI parity",
       subtitle = "<span style= color:'#353F86'>**Texas' PVI**</span> and <span style= color:'#F48847'>**Average PVI of all states**</span> since 1964.",
       x = NULL,
       y = NULL)

# save
ggsave("2021.06.27-blexas/p1.png",
       width = 9,
       height = 6,
       units = "in",
       dpi = 500)

# pvi of close states ----
f_cpvi %>%
  mutate(close = if_else(state %in% v_close_states, "close", "not")) %>%
  group_by(year) %>%
  mutate(cpvi_avg_all = mean(cpvi)) %>%
  ungroup() %>% 
  filter(close == "close") %>%
  group_by(year) %>%
  mutate(cpvi_avg_close = mean(cpvi)) %>%
  ungroup() %>%
  distinct(year, .keep_all = TRUE) %>%
  select(-state, -cpvi, -close) %>%
  pivot_longer(cols = starts_with("cpvi"),
               names_to = "state",
               values_to = "cpvi") %>%
  drop_na() %>%
  bind_rows(f_cpvi) %>%
  mutate(close = if_else(state %in% v_close_states, "close", "not"),
         close = if_else(state == "cpvi_avg_close" | state == "cpvi_avg_all", state, close)) %>%
  ggplot(aes(x = year,
             y = cpvi,
             group = state,
             color = close,
             alpha = close,
             size = close)) +
  geom_line() +
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             color = dd_black,
             size = 1) +
  scale_color_manual(values = c(dd_purple, dd_orange, dd_purple, dd_gray)) +
  scale_alpha_manual(values = c(0.4, 1, 1, 0.15)) +
  scale_size_manual(values = c(0.9, 1.1, 1.1, 0.9)) +
  dd_theme +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(legend.position = "none") +
  labs(title = "The closest states have a Republican advantage",
       subtitle = "**<span style= color:'#D755A6'>PVI of 2020 battleground states*</span>** and <span style= color:'#F48847'>**Average PVI of all states**</span> since 1964.",
       x = NULL,
       y = NULL,
       caption = "*10 states with the smallest margin of victory in 2020:<br>GA, AZ, WI, PA, NC, NV, MI, FL, TX, and MN")

# save
ggsave("2021.06.27-blexas/p2.png",
       width = 9,
       height = 6,
       units = "in",
       dpi = 500)

# pvi geofacet ----
f_abbrevs <- 
  read_csv("2021.06.27-blexas/state_abbrevs.csv")

f_cpvi %>%
  filter(state != "District of Columbia") %>%
  left_join(f_abbrevs, by = "state") %>%
  ggplot(aes(x = year,
             y = cpvi)) +
  geom_line() +
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             color = dd_black) +
  dd_theme +
  scale_x_continuous(breaks = c(1960, 2020)) +
  facet_geo(~ state, 
            grid = "us_state_without_DC_grid3",
            label = "code") +
  theme(strip.text = element_text(family = dd_font, color = "white"),
        strip.background = element_rect(fill = dd_gray),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(title = "How does your state compare?",
       subtitle = "PVI of each state since 1964",
       x = NULL,
       y = NULL,
       caption = "District of Columbia removed for clarity.")

# save
ggsave("2021.06.27-blexas/p3.png",
       width = 9,
       height = 6,
       units = "in",
       dpi = 500)

