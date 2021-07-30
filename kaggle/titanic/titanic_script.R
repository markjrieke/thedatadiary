# libraries ----
library(tidyverse)
library(tidymodels)

source("dd_theme_elements/dd_theme_elements.R")

# import data ----
f_titanic <- 
  read_csv("kaggle/titanic/titanic_train.csv")

# initial view
f_titanic <- 
  f_titanic %>%
  rename_all(.funs = str_to_lower) %>%
  select(-ticket, -cabin, -name)

# eda ----

# pretty similar age profiles for men/women
f_titanic %>%
  ggplot(aes(x = age,
             fill = sex)) +
  geom_density(alpha = 0.5)

# big spike of survival for children
f_titanic %>%
  mutate(survived = as_factor(survived)) %>%
  ggplot(aes(x = age,
             fill = survived)) + 
  geom_density(alpha = 0.5)

# though more so for men
f_titanic %>%
  mutate(survived = as_factor(survived)) %>%
  ggplot(aes(x = age,
             fill = survived)) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(~sex)

# binning may be necessary
f_titanic %>%
  ggplot(aes(x = age)) +
  geom_histogram() +
  facet_wrap(~sex)

# change age/survived to facets
f_titanic <-
  f_titanic %>%
  mutate(age = case_when(age <= 4 ~ "0 - 4",
                         age <= 18 ~ "5 - 18",
                         age <= 25 ~ "18 - 25",
                         age <= 35 ~ "26 - 35",
                         age <= 45 ~ "36 - 45",
                         age <= 55 ~ "46 - 55",
                         age <= 65 ~ "56 - 65",
                         age > 65 ~ "65+",
                         TRUE ~ "Unknown"),
         survived = as_factor(survived))

# more women survived, despite there being more male passengers
f_titanic %>%
  ggplot(aes(x = age,
             fill = survived)) +
  geom_bar() +
  facet_wrap(~sex)

# most people traveling alone
f_titanic %>%
  ggplot(aes(x = parch,
             fill = survived)) +
  geom_histogram()

# higher fares unsurprisingly survived more
f_titanic %>%
  ggplot(aes(x = fare,
             fill = survived)) +
  geom_density(alpha = 0.5) +
  scale_x_log10()

# replace nas in embarked
f_titanic <- 
  f_titanic %>% 
  mutate(embarked = replace_na(embarked, "Unknown"))

# last bit of wrangling
f_titanic <- 
  f_titanic %>%
  select(-passengerid)

# model setup ----

# splits
set.seed(1540)

titanic_split <- 
  f_titanic %>%
  initial_split(prop = 0.8, 
                strata = survived)

titanic_train <- training(titanic_split)
titanic_test <- testing(titanic_split)

# recipe ----
titanic_recipe <- 
  recipe(survived ~ ., data = titanic_train) %>%
  step_mutate(sibsp = as.character(sibsp),
              parch = as.character(parch)) %>%
  step_other(all_nominal_predictors(), threshold = 0.05) %>%
  step_dummy(all_nominal_predictors())

# specifications ----
spec_logistic <- 
  logistic_reg() %>%
  set_engine("glm")

spec_forest <- 
  rand_forest() %>%
  set_engine("ranger") %>%
  set_mode("classification")

# model fits ----
logistic_fit <- 
  workflow() %>%
  add_recipe(titanic_recipe) %>%
  add_model(spec_logistic) %>%
  fit(data = titanic_train)

forest_fit <- 
  workflow() %>%
  add_recipe(titanic_recipe) %>%
  add_model(spec_forest) %>%
  fit(data = titanic_train)

# training preds/roc auc ----
logistic_training_preds <- 
  logistic_fit %>%
  predict(new_data = titanic_train,
          type = "prob")

titanic_train %>%
  bind_cols(logistic_training_preds) %>%
  select(survived, dplyr::starts_with(".pred")) %>%
  bind_cols(
    logistic_fit %>% predict(new_data = titanic_train, type = "class")
  ) %>%
  roc_curve(truth = survived, estimate = .pred_0) %>%
  autoplot()

forest_training_preds <- 
  forest_fit %>%
  predict(new_data = titanic_train,
          type = "prob")

titanic_train %>%
  bind_cols(forest_training_preds) %>%
  select(survived, dplyr::starts_with(".pred")) %>%
  bind_cols(
    forest_fit %>% predict(new_data = titanic_train, type = "class")
  ) %>%
  roc_curve(truth = survived, estimate = .pred_0) %>%
  autoplot()

# test preds/roc auc
logistic_roc <- 
  titanic_test %>%
  bind_cols(
    logistic_fit %>% predict(new_data = titanic_test, type = "prob"),
    logistic_fit %>% predict(new_data = titanic_test, type = "class")
  ) %>%
  select(survived, starts_with(".pred")) %>%
  roc_curve(truth = survived, estimate = .pred_0)

forest_roc <- 
  titanic_test %>%
  bind_cols(
    forest_fit %>% predict(new_data = titanic_test, type = "prob"),
    forest_fit %>% predict(new_data = titanic_test, type = "class")
  ) %>%
  select(survived, starts_with(".pred")) %>%
  roc_curve(truth = survived, estimate = .pred_0) 

logistic_roc %>%
  select(-.threshold) %>%
  mutate(model = "Logistic") %>%
  bind_rows(
    forest_roc %>%
      select(-.threshold) %>%
      mutate(model = "Forest")
  ) %>%
  ggplot(aes(x = 1-specificity,
             y = sensitivity,
             color = model)) +
  geom_path(size = 0.9) +
  scale_color_manual(values = c(dd_red, dd_blue)) +
  geom_abline(linetype = "dashed") +
  dd_theme +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white")) +
  labs(title = "Kaggle's Titanic Dataset",
       subtitle = "ROC AUC of a <span style=color:'#5567D7'>**Logistic Regression**</span>
       and <span style=color:'#D75565'>**Random Forest Model**</span>.",
       caption = "Kaggle's Titanic Dataset:<br>kaggle.com/c/titanic/data") +
  coord_equal()

ggsave("kaggle/titanic/roc_auc.png",
       width = 9,
       height = 6,
       units = "in",
       dpi = 500)

# recreate initial mutations w/kaggle's test data ----
f_titanic_test <-
  read_csv("kaggle/titanic/titanic_test.csv")

f_titanic_test <- 
  f_titanic_test %>%
  rename_all(.funs = str_to_lower) %>%
  select(-ticket, -cabin, -name)

# change age/survived to facets
f_titanic_test <-
  f_titanic_test %>%
  mutate(age = case_when(age <= 4 ~ "0 - 4",
                         age <= 18 ~ "5 - 18",
                         age <= 25 ~ "18 - 25",
                         age <= 35 ~ "26 - 35",
                         age <= 45 ~ "36 - 45",
                         age <= 55 ~ "46 - 55",
                         age <= 65 ~ "56 - 65",
                         age > 65 ~ "65+",
                         TRUE ~ "Unknown"))

f_titanic_test <- 
  f_titanic_test %>% 
  mutate(embarked = replace_na(embarked, "Unknown"))

# fit forest to entire training set ----

# build recipe
kaggle_recipe <- 
  f_titanic %>%
  recipe(survived ~ .) %>%
  step_mutate(sibsp = as.character(sibsp),
              parch = as.character(parch)) %>%
  step_other(all_nominal_predictors(), threshold = 0.05) %>%
  step_dummy(all_nominal_predictors())

# fit model
kaggle_fit <- 
  workflow() %>%
  add_recipe(kaggle_recipe) %>%
  add_model(spec_forest) %>%
  fit(data = f_titanic)

# impute fare data for passenger 1044
v_fare <- 
  f_titanic_test %>%
  filter(!is.na(fare)) %>%
  summarise(fare = mean(fare)) %>% 
  pull(fare)

f_titanic_test <- 
  f_titanic_test %>%
  mutate(fare = if_else(is.na(fare), v_fare, fare))

# apply predictions & save
f_titanic_test %>%
  bind_cols(
    kaggle_fit %>% predict(new_data = f_titanic_test, type = "class")
  ) %>%
  select(passengerid, .pred_class) %>%
  rename(PassengerId = passengerid,
         Survived = .pred_class) %>%
  write_csv("kaggle/titanic/rieke_titanic_submission.csv")





