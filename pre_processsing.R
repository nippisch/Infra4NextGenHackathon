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

combined <- read_rds(file = "data/bothwaves.R")

relevant_vars <- c("w2eq1", # income inequality increased or decresed past 5 years (5: increased a lot)
                   "w2eq11", # in favour or against a basic income scheme (5: strongly against)
                   "w2eq12", # in favour or against increasing availability of good-quality childcare (5: strongly against)
                   "w2eq13", # in favour or against increasing spending on social assistance to the poor (5: strongly against)
                   "w2eq14", # In favour or against guaranteeing stable retirement pensions for future generations (5: strongly against)
                   "w1eq10", # Importance of hard work for getting ahead in life (5: not important at all)
                   "w1eq3", # Tension between poor and rich people in country (4: a lot of tension)
                   "w1eq15", # Reducing differences in income is government's responsibility (10: entirely government's responsibility)
                   "idno", # Respondent's identification number
                   "cntry", # Country
                   "age", # Age
                   "w2weight", # wave 2 survey weight
                   "w1weight", # wave 1 survey weight
                   "gndr", # Gender (1: Male, 2: Female)
                   "eduyrs", # Years of full-time education completed
                   "eisced", # Highest level of education
                   "netusoft", # Internet use, how often (5: every day)
                   "mnactic", # main activity last 7 days (nominal)
                   "hincfel", # Feeling about household's income nowadays (4: Very difficult on present income)
                   "hinctnta", # Household's total net income (1: 1st decile, 10: 10th decile)
                   "region", # Region
                   "ctzcntr", # Citizen of country
                   "vote" # Voted last national election (1: yes, 2: no, 3: not eligible to vote)
)

relevant <- combined |> 
  select(all_of(relevant_vars))

relevant <- relevant |> 
  mutate(w2eq1 = na_if(w2eq1, 9),
         w2eq11 = na_if(w2eq11, 9),
         w2eq12 = na_if(w2eq12, 9),
         w2eq13 = na_if(w2eq13, 9),
         w2eq14 = na_if(w2eq14, 9),
         w1eq10 = na_if(w1eq10, 9),
         w1eq3 = na_if(w1eq3, 9),
         w1eq15 = na_if(w1eq15, 99),
         age = na_if(age, 999),
         gndr = na_if(gndr, 9),
         eduyrs = na_if(eduyrs, 77),
         eduyrs = na_if(eduyrs, 88),
         eduyrs = na_if(eduyrs, 99),
         eisced = na_if(eisced, 77),
         eisced = na_if(eisced, 88),
         eisced = na_if(eisced, 99),
         netusoft = na_if(netusoft, 7),
         netusoft = na_if(netusoft, 8),
         netusoft = na_if(netusoft, 9),
         mnactic = na_if(mnactic, 66),
         mnactic = na_if(mnactic, 77),
         mnactic = na_if(mnactic, 88),
         mnactic = na_if(mnactic, 99),
         hincfel = na_if(hincfel, 7),
         hincfel = na_if(hincfel, 8),
         hincfel = na_if(hincfel, 9),
         hinctnta = na_if(hinctnta, 77),
         hinctnta = na_if(hinctnta, 88),
         hinctnta = na_if(hinctnta, 99),
         ctzcntr = na_if(ctzcntr, 7),
         ctzcntr = na_if(ctzcntr, 8),
         ctzcntr = na_if(ctzcntr, 9),
         vote = na_if(vote, 7),
         vote = na_if(vote, 8),
         vote = na_if(vote, 9)
         )

relevant <- relevant |> 
  mutate(female = ifelse(gndr == 2, 1, 0), # setting male = 0, female = 1
         vote = ifelse(vote == 2, 0, ifelse(vote == 3, 2, vote))) # setting yes = 1, no = 0, not eligible = 2

write_rds(relevant, file = "data/relevant_recoded.R")
