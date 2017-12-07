wc <- fivethirtyeight::weather_check %>%
  select(female, ck_weather, age) %>%
  mutate(female = fct_recode(factor(female), 
                             "Female" = "TRUE", 
                             "Male" = "FALSE"),
         ck_weather = fct_recode(factor(ck_weather), 
                                 "Check" = "TRUE", 
                                 "No Check" = "FALSE")) %>%
  mutate(female = fct_relevel(female, "Female"),
         ck_weather = fct_relevel(ck_weather, "Check"))