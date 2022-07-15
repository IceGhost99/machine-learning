library(tidyverse)
library(tidymodels)
d <- read_csv("data/ngsschat-processed-data.csv")
d

train_test_split <- initial_split(d, prop = .99)
data_train <- training(train_test_split)
data_test <- testing(train_test_split)

my_rec <- recipe(code ~ ., data = data_train)

my_mod <-
  logistic_reg() %>% 
  set_engine("glm") %>%
  set_mode("classification")
# specify workflow
# 
my_wf <-
  workflow() %>%
  add_model(my_mod) %>% 
  add_recipe(my_rec)

fitted_model <- fit(my_wf, data = data_train)
final_fit <- last_fit(fitted_model, train_test_split)

final_fit %>%
  collect_metrics()

final_fit %>% collect_predictions()