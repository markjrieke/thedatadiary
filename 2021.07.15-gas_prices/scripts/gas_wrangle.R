# libraries ----
library(tidyverse)
library(lubridate)

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

# gas prices
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

# plots ----
f_gas %>%
  filter(date >= "2021-01-01") %>%
  ggplot(aes(x = date,
             y = gas_price, 
             color = category)) +
  geom_line()





?as_date
