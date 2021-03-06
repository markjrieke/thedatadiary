# Libraries ----
library(ggplot2)
library(dplyr)
library(plotly)
library(grid)
library(scales)

# I ended up not using the plotly & grid
# libraries, but kept them here for record's
# sake.

# Minwage vs Livwage plot ----

# Import, clean, & plot CPI data
cpi_import <- read.csv("cpi_accessed_1_4_21.csv")
cpi_import$Date <- as.Date(cpi_import$Date)

# Create vect with "CPI" label
cpi_vect <- rep("CPI", len = nrow(cpi_import))

# Minimum Wage for non-farm workers
minwage_import <- read.csv("minwage_accessed_1_4_21.csv")
minwage_import$observation_date <- as.Date(minwage_import$observation_date)

# Create vector with "minwage" label
minwage_vect <- rep("Minimum Wage", len = nrow(minwage_import))

# Living wage, per MIT Lab, in 2018, was $16.14 
# (Assuming this is applied to Jan 2018)

liv_wage_18 <- 16.14

# Adjusting the living wage for inflation

# Jan 2018 on row 853 of cpi_import
inf_converter <- cpi_import[853,2]

inf_factor <- cpi_import$CPI / inf_converter
liv_wage_vect <- inf_factor * liv_wage_18

# Create a new frame (not imported) based on CPI import
liv_wage_frame <- cpi_import
liv_wage_frame$liv_wage <- liv_wage_vect

liv_wage_frame <- liv_wage_frame %>%
  select(!CPI)

# Create a vector with "liv_wage" label
liv_wage_cat_vect <- rep("Living Wage", len = nrow(liv_wage_frame))

liv_wage_frame$category <- liv_wage_cat_vect

# importing poverty threshold data
poverty_import <- read.csv("poverty_threshold_1_4_21.csv")
poverty_import$Year <- as.Date(paste(poverty_import$Year,
                                     "-1-1",
                                     sep = ""))

poverty_import$category <- rep("Poverty Threshold", len = nrow(poverty_import))

# adjust to 2 working adults, 2080 working hrs per year
poverty_import$Threshold <- poverty_import$Threshold / 2 / 2080

# creating new summary frame
minwage_adjusted <- minwage_import
minwage_adjusted <- minwage_adjusted %>%
  filter(observation_date > as.Date("1946-12-1"),
         observation_date < as.Date("2020-12-1"))

# readjusting inflation factor from 2018 to Nov 2020
minwage_adjusted$CPI <- cpi_import$CPI
inf_converter_20 <- cpi_import[887,2]
inf_factor_20 <- minwage_adjusted$CPI / inf_converter_20
minwage_adjusted$minwage_corr <- minwage_adjusted$FEDMINNFRWG / inf_factor_20

# extrapolating out low/high
pov_thresh_78 = min(poverty_import$Threshold)

poverty_extrap_low <- minwage_adjusted

poverty_extrap_low <- poverty_extrap_low %>%
  select(observation_date, CPI) %>%
  filter(observation_date < as.Date("1978-1-2")) %>%
  mutate(inf_factor_78 = CPI / max(CPI)) %>%
  mutate(pov_thresh_adj = inf_factor_78 * pov_thresh_78) %>%
  mutate(category = "pov_thresh_low") %>%
  select(observation_date, pov_thresh_adj, category)

pov_thresh_19 = max(poverty_import$Threshold)

poverty_extrap_high <- minwage_adjusted

poverty_extrap_high <- poverty_extrap_high %>%
  select(observation_date, CPI) %>%
  filter(observation_date >= "2019-1-1") %>%
  mutate(inf_factor_19 = CPI / min(CPI)) %>%
  mutate(pov_thresh_adj = inf_factor_19 * pov_thresh_19) %>%
  mutate(category = "pov_thresh_high") %>%
  select(observation_date, pov_thresh_adj, category)


# Create summary data frame 
master_summary <- minwage_import
master_summary$category <- minwage_vect

# Rename columns to match for rbinding frames
colname_vect <- c("date", "value", "category")

colnames(master_summary) <- colname_vect
colnames(liv_wage_frame) <- colname_vect
colnames(poverty_import) <- colname_vect
colnames(poverty_extrap_low) <- colname_vect
colnames(poverty_extrap_high) <- colname_vect

master_summary <- rbind(master_summary, 
                        liv_wage_frame,
                        poverty_import,
                        poverty_extrap_low,
                        poverty_extrap_high)

master_summary <- master_summary %>%
  filter(date >= "1947-01-01" & 
           date < "2020-12-01")


# ggplotting 
liv_wage_color <- "#F4A606"
min_wage_color <- "#A606F4"
pov_wage_color <- "#06F4A6"
font_sizer <- 4.5

# create summary plot w/custom colors
summary_plot <- ggplot(master_summary, 
                       aes(x = date, 
                           y = value, 
                           color = category)) + 
  geom_line(size = 1,
            aes(linetype = category)) +
  labs(title = "Nuclear Family Living Wage and Poverty Threshold", 
       subtitle = "From 1947 - 2020",
       x = NULL,
       y = NULL) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = dollar_format()) +
  scale_color_manual(values = c(liv_wage_color,
                                min_wage_color,
                                pov_wage_color,
                                pov_wage_color,
                                pov_wage_color)) + 
  scale_linetype_manual(values = c("solid",
                                   "solid",
                                   "twodash",
                                   "twodash",
                                   "solid"))

# add annotations
summary_plot <- summary_plot +
  annotate(geom = "text",
           label = "Living Wage:\n$ 16.92 /hr",
           x = as.Date("2007-1-1"),
           y = 15.25,
           color = liv_wage_color,
           size = font_sizer,
           fontface = "bold") +
  annotate(geom = "curve",
           x = as.Date("2010-1-1"),
           y = 16,
           xend = as.Date("2019-1-1"),
           yend = 16.92,
           curvature = -0.2,
           size = 1.2,
           color = liv_wage_color,
           arrow = arrow(length = unit(3, "mm"))) +
  annotate(geom = "text",
           label = "Minimum Wage:\n$ 7.25 /hr",
           x = as.Date("2010-1-1"),
           y = 9,
           color = min_wage_color,
           size = font_sizer,
           fontface = "bold") +
  annotate(geom = "curve",
           x = as.Date("2015-1-1"),
           y = 9,
           xend = as.Date("2020-1-1"),
           yend = 7.4,
           curvature = -0.2,
           size = 1.2,
           color = min_wage_color,
           arrow = arrow(length = unit(3, "mm"))) +
  annotate(geom = "text",
           label = "Poverty Threshold:\n$ 6.43 /hr",
           x = as.Date("2010-1-1"),
           y = 3,
           color = pov_wage_color,
           size = font_sizer,
           fontface = "bold") +
  annotate(geom = "curve", 
           x = as.Date("2016-1-1"),
           y = 3.5,
           xend = as.Date("2020-1-1"),
           yend = 6,
           curvature = 0.25,
           size = 1.2,
           color = pov_wage_color,
           arrow = arrow(length = unit(3, "mm")))

summary_plot

# Minwage vs inflation plot ----

# creating plot of adjusted minwage

minwage_adj_plot <- ggplot(minwage_adjusted, 
                           aes(x = observation_date,
                               y = minwage_corr)) +
  geom_line(size = 1,
            color = min_wage_color) +
  labs(title = "US Minimum Wage, Adjusted for Inflation",
       subtitle = "From 1947 - 2020",
       x = NULL,
       y = NULL) +
  scale_y_continuous(labels = dollar_format())

# add annotations

minwage_adj_plot <- minwage_adj_plot + 
  annotate(geom = "text",
           label = "$ 12.20 /hr",
           x = as.Date("1983-1-1"),
           y = 11.5,
           color = min_wage_color,
           size = font_sizer,
           fontface = "bold") +
  annotate(geom = "curve",
           x = as.Date("1981-1-1"),
           y = 11.7,
           xend = as.Date("1969-1-1"),
           yend = 12.2,
           curvature = 0.3,
           size = 1.2,
           color = min_wage_color,
           arrow = arrow(length = unit(3, "mm"))) + 
  annotate(geom = "text",
           label = "$ 7.25 /hr",
           x = as.Date("2014-1-1"),
           y = 6,
           color = min_wage_color,
           size = font_sizer,
           fontface = "bold") + 
  annotate(geom = "curve",
           x = as.Date("2017-1-1"),
           y = 6.2,
           xend = as.Date("2020-11-1"),
           yend = 7,
           curvature = 0.3,
           size = 1.2,
           color = min_wage_color,
           arrow = arrow(length = unit(3, "mm")))

minwage_adj_plot

# state living/min wage plot ----

# importing state data
state_data_import <- read.csv("livwage_mit_labs_1_26_21.csv")

state_data <- state_data_import %>%
  filter(Category != "povertywage") %>%
  arrange(desc(Value))

# reorder voodoo to order plot by living wage
state_data$State <- factor(state_data$State, levels = unique(state_data$State))

# plot
state_minliv_plot <- ggplot(state_data, aes(x = Value,
                                            y = reorder(State, desc(State)),
                                            color = Category,
                                            text = sprintf("%s, $ %s",
                                                           State,
                                                           number(Value,
                                                                  accuracy = 0.01)))) +
  labs(title = "State Living & Minimum Wage",
       subtitle = "per MIT Labs 2020 Update",
       x = NULL,
       y = NULL) +
  theme(legend.position = "none") + 
  geom_vline(xintercept = 7.25,
             linetype = "dashed",
             color = min_wage_color,
             size = 1) +
  geom_point(size = 3) + 
  scale_color_manual(values = c(liv_wage_color,
                                min_wage_color)) + 
  scale_x_continuous(labels = dollar_format())

state_minliv_plot

# interactive plot (maybe will use?)
state_interactive <- ggplotly(state_minliv_plot, tooltip = "text")

Sys.setenv("plotly_username" = "markjrieke")
Sys.setenv("plotly_api_key" = "MHe7j1Xjz6N5ogwuIg6I")
api_create(state_interactive, filename = "2021-01-31-interactive")

# projection plot ----

# set annual inflation at 1.6%
annual_inf = 0.016

# create projected frames to add to the master frame
date_vect <- seq(as.Date("2020-11-1"), as.Date("2030-1-1"), by = "months")

temp_summary <- master_summary %>%
  filter(category == "Living Wage") %>%
  select(value)

livwageproj <- max(temp_summary$value)

temp_summary <- master_summary %>%
  filter(category == "pov_thresh_high") %>%
  select(value)

povwageproj <- max(temp_summary$value)

livwageproj_frame <- as.data.frame(date_vect)
povwageproj_frame <- livwageproj_frame

livwageproj_frame <- livwageproj_frame %>% 
  mutate(value = livwageproj * (1 + annual_inf/12)^(row_number()-1)) %>%
  mutate(category = "livwage projected")

povwageproj_frame <- povwageproj_frame %>%
  mutate(value = povwageproj * (1 + annual_inf/12)^(row_number()-1)) %>%
  mutate(category = "povwage projected")

minwageproj_dates <- as.Date(c("2020-11-1", "2021-1-1", "2022-1-1", 
                               "2023-1-1", "2024-1-1", "2025-1-1", "2030-1-1"))
minwageproj_values <- c(7.25, 9.5, 11, 12.5, 14, 15, 15)
minwageproj_category <- rep("minwage projected", len = 7)

minwageproj_frame <- data.frame(minwageproj_dates,
                                minwageproj_values,
                                minwageproj_category)

minwageproj_frame

colnames(livwageproj_frame) <- colname_vect
colnames(povwageproj_frame) <- colname_vect
colnames(minwageproj_frame) <- colname_vect

projected_summary <- rbind(master_summary, 
                           livwageproj_frame,
                           povwageproj_frame,
                           minwageproj_frame)

# ggplotting
projection_plot <- ggplot(projected_summary, 
                          aes(x = date, 
                              y = value,
                              color = category)) +
  geom_line(size = 1,
            aes(linetype = category)) +
  scale_y_continuous(labels = dollar_format()) + 
  labs(title = "Nuclear Family Living Wage and Poverty Threshold",
       subtitle = "From 1947 - 2030 (projected)",
       x = NULL,
       y = NULL) + 
  theme(legend.position = "none") +
  scale_color_manual(values = c(liv_wage_color,
                                liv_wage_color,
                                min_wage_color,
                                min_wage_color,
                                pov_wage_color,
                                pov_wage_color,
                                pov_wage_color,
                                pov_wage_color)) + 
  scale_linetype_manual(values = c("solid",
                                   "dashed",
                                   "solid",
                                   "dashed",
                                   "twodash",
                                   "twodash",
                                   "solid",
                                   "dashed"))

projection_plot <- projection_plot + 
  annotate(geom = "text",
           label = "$ 15.00 /hr\nby 2025",
           x = as.Date("2014-1-1"),
           y = 10,
           color = min_wage_color,
           size = font_sizer,
           fontface = "bold") +
  annotate(geom = "curve",
           x = as.Date("2014-1-1"),
           y = 11,
           xend = as.Date("2023-6-1"),
           yend = 15,
           curvature = -0.3,
           size = 1.2,
           color = min_wage_color,
           arrow = arrow(length = unit(3, "mm")))

projection_plot

# test area----



