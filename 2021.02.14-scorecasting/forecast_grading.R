# libraries ----
install.packages("writexl")

library(dplyr)
library(writexl)

# import & clean main dataframe ----
f_main <- read.csv("presidential_forecasts.csv")
f_result <- read.csv("presidential_results.csv")

f_main <- f_main %>%
  select(!race) %>%
  mutate(win_act = f_result[match(f_main$state,
                                  f_result$state),
                            "result"]) %>%
  mutate(b_conf = if_else(win_proj == "biden",
                          conf,
                          1 - conf)) %>%
  mutate(evs = f_result[match(f_main$state,
                              f_result$state),
                        "evs"])

# replace index/match with 1/0 for biden/trump
f_main <- f_main %>%
  mutate(win_act = if_else(win_act == "biden",
                           1,
                           0)) %>%
  mutate(brier = ((win_act - b_conf)^2)) %>%
  mutate(brier_wt = brier * evs) %>%
  select(!win_proj) %>%
  select(!conf)

# create betting cols

f_main <- f_main %>%
  mutate(bet = 200 * abs(b_conf - 0.5)) %>%
  mutate(winning = if_else(abs(b_conf - win_act) <= 0.5,
                           bet,
                           -bet))

# individual forecast frames ----

f_jhk <- f_main %>%
  filter(forecast == "JHK", state != "Nat") %>%
  select(state, brier, brier_wt, winning)

f_econ <- f_main %>%
  filter(forecast == "economist", state != "Nat") %>%
  select(state, brier, brier_wt, winning)

f_538 <- f_main %>%
  filter(forecast == 538, state != "Nat") %>%
  select(state, brier, brier_wt, winning)

f_bite <- f_main %>%
  filter(forecast == "bitecofer", state != "Nat") %>%
  select(state, brier, brier_wt, winning)

# creating summary table ----

brier_jhk <- sum(f_jhk$brier) / nrow(f_jhk)
brier_econ <- sum(f_econ$brier) / nrow(f_econ)
brier_538 <- sum(f_538$brier) / nrow(f_538)
brier_bite <- sum(f_bite$brier) / nrow(f_bite)

brier_wt_jhk <- sum(f_jhk$brier_wt) / nrow(f_jhk)
brier_wt_econ <- sum(f_econ$brier_wt) / nrow(f_econ)
brier_wt_538 <- sum(f_538$brier_wt) / nrow(f_538)
brier_wt_bite <- sum(f_bite$brier_wt) / nrow(f_bite)

win_jhk <- sum(f_jhk$winning)
win_econ <- sum(f_econ$winning)
win_538 <- sum(f_538$winning)
win_bite <- sum(f_bite$winning)

win_avg_jhk <- win_jhk / nrow(f_jhk)
win_avg_econ <- win_econ / nrow(f_econ)
win_avg_538 <- win_538 / nrow(f_538)
win_avg_bite <- win_bite / nrow(f_bite)

v_fore <- c("JHK", "Economist", "FiveThirtyEight", "Bitecofer")
v_brier <- c(brier_jhk, brier_econ, brier_538, brier_bite)
v_brier_wt <- c(brier_wt_jhk, brier_wt_econ, brier_wt_538, brier_wt_bite)
v_win_avg <- c(win_avg_jhk, win_avg_econ, win_avg_538, win_avg_bite)

f_summary <- as.data.frame(cbind(v_fore, v_brier, v_brier_wt, v_win_avg))
colnames(f_summary) <- c("", "Brier Score", "Weighted Brier Score", "Average Winnings")

write_xlsx(f_summary, "forecast_summary_frame.xlsx")

# test area ----

# hi!