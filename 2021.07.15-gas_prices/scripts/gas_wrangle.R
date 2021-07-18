# libraries ----
library(tidyverse)
library(lubridate)

# theme ----
source("dd_theme_elements/dd_theme_elements.R")

# cpi
f_cpi <- 
  read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=CPIAUCSL&scale=left&cosd=1947-01-01&coed=2021-06-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2021-07-18&revision_date=2021-07-18&nd=1947-01-01") %>%
  rename(date = DATE,
         cpi = CPIAUCSL) %>%
  arrange(date)

# cpi as of 6/1/2021
v_cpi <- 
  f_cpi %>%
  slice_tail(n = 1) %>%
  pull(cpi)

# get cpi adjusted to each month
f_cpi <- 
  f_cpi %>%
  mutate(cpi_adj = v_cpi/cpi) 

# gas prices merged w/cpi
# read_csv("https://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=pet&s=emm_epm0_pte_nus_dpg&f=w")
f_gas <- 
  read_csv("2021.07.15-gas_prices/data/Weekly_U.S._All_Grades_All_Formulations_Retail_Gasoline_Prices.csv",
         skip = 4) %>%
  rename(date = `Week of`,
         price = `Weekly U.S. All Grades All Formulations Retail Gasoline Prices Dollars per Gallon`) %>%
  mutate(date = mdy(date)) %>%
  arrange(date) %>%
  mutate(date = floor_date(date, unit = "month")) %>%
  distinct(date, .keep_all = TRUE) %>%
  left_join(f_cpi, by = "date") %>%
  mutate(gas_adj = price * cpi_adj) %>%
  select(-starts_with("cpi")) %>%
  pivot_longer(cols = c(price, gas_adj),
               values_to = "gas_price",
               names_to = "category")

# gas prices unmerged
f_gas_weekly <-
  read_csv("2021.07.15-gas_prices/data/Weekly_U.S._All_Grades_All_Formulations_Retail_Gasoline_Prices.csv",
           skip = 4) %>%
  rename(date = `Week of`,
         price = `Weekly U.S. All Grades All Formulations Retail Gasoline Prices Dollars per Gallon`) %>%
  mutate(date = mdy(date)) %>%
  arrange(date)

# plots ----
f_gas_weekly %>%
  filter(date >= "2021-01-01") %>%
  ggplot(aes(x = date,
             y = price)) +
  geom_line(color = dd_blue_dark,
            size = 1) +
  dd_theme +
  labs(title = "Prices at the Pump",
       subtitle = "Price of Gasoline since Biden assumed the Presidency",
       caption = "US Energy Information Administration<br>Weekly US All Grades All Formulations Retail Gasoline Prices",
       x = NULL,
       y = NULL) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_date(breaks = "1 month", 
               labels = scales::date_format("%b")) 

ggsave(file = "2021.07.15-gas_prices/p1.png",
       width = 9,
       height = 6,
       units = "in",
       dpi = 500)

f_gas %>%
  filter(category == "gas_adj",
         date >= "2017-01-01") %>%
  ggplot(aes(x = date,
             y = gas_price)) +
  geom_line(color = dd_blue_dark,
            size = 1) +
  annotate(geom = "rect",
           xmin = as.Date(-Inf),
           xmax = as.Date("2021-01-20"),
           ymin = -Inf,
           ymax = Inf,
           fill = dd_red,
           alpha = 0.2) +
  annotate(geom = "rect",
           xmin = as.Date("2021-01-20"),
           xmax = as.Date(Inf),
           ymin = -Inf,
           ymax = Inf,
           fill = dd_blue,
           alpha = 0.2) +
  dd_theme +
  labs(title = "Prices at the Pump",
       subtitle = "Inflation Adjusted Price of Gasoline during the <span style=color:'#D75565'>**Trump**</span>
       and <span style=color:'#5565D7'>**Biden**</span> Presidencies",
       x = NULL,
       y = NULL,
       caption = "US Energy Information Administration<br>Weekly US All Grades All Formulations Retail Gasoline Prices") +
  scale_y_continuous(labels = scales::dollar_format())

ggsave("2021.07.15-gas_prices/p2.png",
       width = 9,
       height = 6,
       units = "in",
       dpi = 500)


