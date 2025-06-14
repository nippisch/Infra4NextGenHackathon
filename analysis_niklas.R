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

subdata_alpha <- relevant |> 
  select(w2eq11, w2eq12, w2eq13, w2eq14) |> 
  mutate(across(everything(), ~na_if(., 9)))

subdata_alpha <- relevant |> 
  select(w2eq11, w2eq13)

subdata_alpha_perceptions <- combined |> 
  filter(cntry == "AT") |> 
  select(w2eq1, w2eq2, w2eq3, w2eq4) |> 
  mutate(across(everything(), ~na_if(., 9)))


alpha(subdata_alpha)

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

table(relevant$cntry)

relevant1 <- relevant |> 
  filter(!is.na(w2eq1) & !is.na(w2eq11))
cor(relevant1$w2eq1, relevant1$w2eq11)

relevant1 <- relevant |> 
  filter(!is.na(w2eq11) & !is.na(w1eq10))
cor(relevant1$w2eq11, relevant1$w1eq10)

relevant <- read_rds("data/relevant_recoded.R")

relevant <- relevant |> 
  mutate(w1eq10 = 6 - w1eq10)

# Step 2: Define countries
countries <- c("PT", "SI")

# Step 3: Filter and create binary variables
relevant_bin <- relevant %>%
  filter(w2eq13 != 3, w2eq1 != 3) %>%
  mutate(
    policy_bin = ifelse(w2eq13 %in% c(1,2), 1L, 0L),
    income_bin = ifelse(w2eq1   %in% c(4,5), 1L, 0L)
  ) %>%
  filter(!is.na(policy_bin), !is.na(income_bin), cntry %in% countries)

# Step 4: Nest by country
nested <- relevant_bin %>%
  group_by(cntry) %>%
  nest()

# Step 5: Fit model with additional predictors
results_svy <- nested %>%
  mutate(
    design = map(data, ~ as_survey_design(.x, weights = w2weight)),
    model  = map(design, ~ svyglm(policy_bin ~ income_bin + age + female + w1eq10 + w1eq8,
                                  design = .x,
                                  family = quasibinomial())),
    tidy   = map(model, ~ broom::tidy(.x) %>%
                   filter(term %in% c("income_bin", "age", "female", "w1eq10", "w1eq8")))
  ) %>%
  unnest(tidy) %>%
  transmute(
    cntry,
    term,
    beta = estimate,
    sd   = std.error,
    p    = p.value,
    sig  = ifelse(p < 0.05, 1L, 0L)
  )
results_svy <- results_svy %>%
  arrange(desc(beta)) %>%
  mutate(cntry = factor(cntry, levels = cntry))

# Step 6: Plot with error bars
results_svy %>%
  ggplot(aes(x = beta, y = term, color = cntry)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(
    xmin = beta - 1.96 * sd,
    xmax = beta + 1.96 * sd
  ), height = 0.3, position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "",
    y = "",
    title = "",
    color = "Country"
  ) +
  scale_color_manual(
    name = "Country",
    values = c("PT" = "#1F77B4", "SI" = "#AB110F")
  ) +
  scale_y_discrete(labels = c(
    "Age \n (in years)",
    "Female \n (1: Yes)",
    "Perceived \n income inequality \n (1: Increased)",
    "Importance of hard \n work to getting \n ahead in live \n (5: Essential)",
    "Feelings towards \n differences in wealth \n (10: extremely angry)"
  )) +
  theme_linedraw() +
  theme(
    panel.background = element_rect(fill = 'white', colour = 'white'),
    plot.background = element_rect(fill = '#FFF7F5', colour = '#FFF7F5'),
    axis.text.x = element_text(colour="#22444b"),
    axis.text.y = element_text(colour="#22444b"),
    legend.position = "bottom")


## Binary Regression with weighted data and big model
relevant    <- read_rds("data/relevant_recoded.R")

countries   <- c("AT","BE","CZ","FI","FR","GB","HU","IS","PL","PT","SI")

relevant_bin <- relevant %>%
  filter(w2eq13 != 3, w2eq1 != 3) %>%
  mutate(
    policy_bin = ifelse(w2eq13 %in% c(1,2), 1L, 0L),
    income_bin = ifelse(w2eq1   %in% c(4,5), 1L, 0L)
  ) %>%
  filter(!is.na(policy_bin), !is.na(income_bin), cntry %in% countries)

nested <- relevant_bin %>%
  group_by(cntry) %>%
  nest()

results_svy <- nested %>%
  mutate(
    design = map(data,    ~ as_survey_design(.x, weights = w2weight)),
    model  = map(design,  ~ svyglm(policy_bin ~ income_bin+ age + female + w1eq10 + w1eq8 + hinctnta + eisced,
                                   design = .x,
                                   family = quasibinomial())),
    # in svyglm for binomial and poisson you have to use quasibinomial() to aviod a warning about non-integer
    ## numbers of successes. The quasi version of the family objects give the same point estimates and st. errors
    tidy   = map(model,   ~ broom::tidy(.x) %>% filter(term == "income_bin"))
  ) %>%
  unnest(tidy) %>%
  transmute(
    cntry,
    beta = estimate,
    sd   = std.error,
    p    = p.value,
    sig  = ifelse(p < 0.05, 1L, 0L)
  )

results_svy <- results_svy %>%
  arrange(desc(beta)) %>%
  mutate(cntry = factor(cntry, levels = cntry))

country_labels <- c(
  "SI" = "SI - Slovenia",
  "AT" = "AT - Austria",
  "IS" = "IS - Iceland",
  "FI" = "FI - Finland",
  "GB" = "GB - United Kingdom",
  "FR" = "FR - France",
  "PL" = "PL - Poland",
  "CZ" = "CZ - Czechia",
  "BE" = "BE - Belgium",
  "HU" = "HU - Hungary",
  "PT" = "PT - Portugal"
)

results_svy %>%
  ggplot(aes(x = beta, y = reorder(cntry, beta))) +
  geom_point(colour = "#AB110F", size = 3) +
  geom_errorbarh(aes(
    xmin = beta - 1.96 * sd,
    xmax = beta + 1.96 * sd
  ), height = 0.3, colour = "#AB110F", size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x     = "",
    y = "",
    title = ""
  ) +
  scale_x_continuous(limits = c(-2.5, 4),
                     breaks = seq(-2.5, 2.5, by = 1),
                     expand = c(0,0.1)) +
  scale_y_discrete(
    labels = country_labels) +
  theme_linedraw() +
  theme(
    panel.background = element_rect(fill = 'white', colour = 'white'),
    plot.background = element_rect(fill = '#FFF7F5', colour = '#FFF7F5'),
    axis.text.x = element_text(colour="#22444b"),
    axis.text.y = element_text(colour="#22444b"))

