# libraries ----
library(tidyverse)
library(lubridate)

# import data ----

# gas prices
# read_csv("https://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=pet&s=emm_epm0_pte_nus_dpg&f=w")
read_csv("2021.07.15-gas_prices/data/Weekly_U.S._All_Grades_All_Formulations_Retail_Gasoline_Prices.csv",
         skip = 4) %>%
  rename(date = `Week of`,
         price = `Weekly U.S. All Grades All Formulations Retail Gasoline Prices Dollars per Gallon`) %>%
  mutate(date = mdy(date)) %>%
  arrange(date)

?as_date
