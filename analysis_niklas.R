# analysis Niklas

library(tidyverse)

combined <- read_rds("data/bothwaves.R")

combined |> 
  filter(w1sq16 != 99) |> 
  group_by(cntry) |> 
  summarize(mean = mean(w1sq16, na.rm = TRUE)) |> 
  arrange(desc(mean))
table(combined$w1sq16)

install.packages("psych")   # Only run once
library(psych)

subdata_alpha <- combined |> 
  select(w2eq11, w2eq12, w2eq13, w2eq14) |> 
  mutate(across(everything(), ~na_if(., 9)))

subdata_alpha <- combined |> 
  select(w2eq11, w2eq13)

subdata_alpha_perceptions <- combined |> 
  filter(cntry == "AT") |> 
  select(w2eq1, w2eq2, w2eq3, w2eq4) |> 
  mutate(across(everything(), ~na_if(., 9)))


alpha(subdata_alpha_perceptions)

vars <- c("w2eq1", "w2eq11", "w2eq12", "w1eq10")

austria <- combined |> 
  filter(cntry == "AT") |> 
  mutate(across(all_of(vars), ~na_if(., 9)))

test <- austria |> 
  select(w2eq1, w2eq11) |> 
  filter(w2eq1 != is.na(w2eq1) & w2eq11 != is.na(w2eq11))

cor(test$w2eq1, test$w2eq11, method = "spearman")

m1 <- lm(w2eq1 ~ w2eq11 + w2eq12 + w1eq10, data = austria)

summary(m1)

add.m

austria |> 
  select(starts_with("w2eq") | starts_with("w1eq")) |> 
  summarize(across(everything(), ~sum(is.na(.)), .names = "na_{.col}"))

