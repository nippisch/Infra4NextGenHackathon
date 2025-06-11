# data preprocessing

library(tidyverse)

wave1 <- read_csv("data/CRON3W1e01.1.csv")
wave2 <- read_csv("data/CRON3W2e01.1.csv")

combined <- full_join(wave1, wave2, join_by(idno))

write_rds(combined, file = "data/bothwaves_raw.R")

combined <- read_rds("data/bothwaves_raw.R")

combined <- combined |> 
  mutate(cntry = ifelse(!is.na(cntry.x), cntry.x, cntry.y)) |> 
  mutate(age = ifelse(!is.na(age.x), age.x, age.y)) |> 
  mutate(agegroup35 = ifelse(!is.na(agegroup35.x), agegroup35.x, agegroup35.y)) |> 
  mutate(essround = ifelse(!is.na(essround.x), essround.x, essround.y)) |> 
  mutate(gndr = ifelse(!is.na(gndr.x), gndr.x, gndr.y)) |> 
  mutate(eduyrs = ifelse(!is.na(eduyrs.x), eduyrs.x, eduyrs.y)) |> 
  mutate(eisced = ifelse(!is.na(eisced.x), eisced.x, eisced.y)) |> 
  mutate(netusoft = ifelse(!is.na(netusoft.x), netusoft.x, netusoft.y)) |> 
  mutate(mnactic = ifelse(!is.na(mnactic.x), mnactic.x, mnactic.y)) |> 
  mutate(hincfel = ifelse(!is.na(hincfel.x), hincfel.x, hincfel.y)) |> 
  mutate(hinctnta = ifelse(!is.na(hinctnta.x), hinctnta.x, hinctnta.y)) |> 
  mutate(region = ifelse(!is.na(region.x), region.x, region.y)) |> 
  mutate(ctzcntr = ifelse(!is.na(ctzcntr.x), ctzcntr.x, ctzcntr.y)) |> 
  mutate(vote = ifelse(!is.na(vote.x), vote.x, vote.y)) |> 
  mutate(mode = ifelse(!is.na(mode.x), mode.x, mode.y)) |> 
  mutate(yrbrn = ifelse(!is.na(yrbrn.x), yrbrn.x, yrbrn.y))

combined <- combined |> 
  select(-ends_with(".x")) |> 
  select(-ends_with(".y"))

write_rds(combined, file = "data/bothwaves.R")

