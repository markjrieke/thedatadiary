# libraries ----
library(tidyverse)

# set path ----
path <- "2021.09.26-president_momentum/"

# read in data ----

# demographics
demo_aapi <- read_csv(paste0(path, "data/aapi_pct.csv"))
demo_black <- read_csv(paste0(path, "data/black_pct.csv"))
demo_hisp <- read_csv(paste0(path, "data/mexican_hispanic_latino_pct.csv"))
demo_nat_amer <- read_csv(paste0(path, "data/native_american_pct.csv"))
demo_white <- read_csv(paste0(path, "data/non_hisp_white_pct.csv"))

# economics
econ_cpi <- read_csv(paste0(path, "data/cpi_seasonal.csv"))
econ_gdp <- read_csv(paste0(path, "data/fred_real_gdp.csv"))

# elections
prez_mit <- read_csv(paste0(path, "data/mit_labs_1976_2020_president.csv"))
prez_wiki <- read_csv(paste0(path, "data/wiki_1956_1972_president.csv"))
prez_nat <- read_csv(paste0(path, "data/national_2pv_results.csv"))

# helpers
help_state <- read_csv(paste0(path, "data/lookups/state_lookups.csv"))

# wrangle demographics ----

# function to wrangle
demo_wrangle <- function(x, demo_name) {
  
  # create vector to assign to years
  years <- rep(seq(1950, 2020, 1), 52)
  
  # initial reformatting 
  demo_frame <- 
    x %>%
    rename(state = `state/territory`) %>%
    left_join(help_state, by = "state") %>%
    select(-state) %>%
    rename(state = state_map) %>%
    pivot_longer(cols = c(starts_with("1"), starts_with("2")),
                 names_to = "year",
                 values_to = "pct") %>%
    filter(year >= 1950) %>%
    mutate(pct = str_remove(pct, "%")) %>%
    mutate(across(-state, as.numeric),
           pct = pct/100) 
  
  # linear interpolation
  demo_frame <- 
    demo_frame %>%
    group_by(state) %>%
    arrange(state, year, .by_group = TRUE) %>%
    summarise(interpolated_pct = approx(year, pct, seq(1950, 2020, 1))$y) %>%
    ungroup() %>%
    bind_cols(as_tibble(years)) %>%
    rename(year = value) %>%
    relocate(year, .after = state) %>%
    mutate(demo = demo_name) %>%
    filter(state != "Puerto Rico")
  
  return(demo_frame)
  
}

# merge into master col
demographics <- 
  bind_rows(
    demo_aapi %>% demo_wrangle("aapi"),
    demo_black %>% demo_wrangle("black"),
    demo_hisp %>% demo_wrangle("hispanic"),
    demo_nat_amer %>% demo_wrangle("native_american"),
    demo_white %>% demo_wrangle("white")
  )

# clean up envirnoment
rm(demo_aapi, demo_black, demo_hisp, demo_nat_amer, demo_white)








                                            