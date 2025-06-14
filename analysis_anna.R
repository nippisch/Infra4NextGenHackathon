# analysis Anna

library(readr)
library(readxl)
library(survey)
library(srvyr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)
library(showtext)
library(ggrounded)
library(purrr)

font_add_google("Montserrat", "montserrat")

relevant <- read_rds(file = "data/relevant_recoded.R")


# Figure 1 ----

wid_data <- read_excel("data/WID_Data_13062025-143407.xlsx")

colnames(wid_data) <- c("percentile", "year", "FR - France", "GB - United Kingdom",
                        "AT - Austria", "BE - Belgium", "CZ - Czechia", "FI - Finland",
                        "HU - Hungary", "IS - Iceland", "PL - Poland", "PT - Portugal",
                        "SI - Slovenia")

country_colours <- c("AT - Austria" = "#332288",
                     "BE - Belgium" = "#88CCEE",
                     "CZ - Czechia" = "#6699CC",
                     "FI - Finland" = "#117733",
                     "FR - France" = "#999933",
                     "GB - United Kingdom" = "#DDCC77", 
                     "HU - Hungary" = "#CC6677",
                     "IS - Iceland" = "#AA4499",
                     "PL - Poland" = "#D55E00",
                     "PT - Portugal" = "#661100", 
                     "SI - Slovenia" = "#44AA99")


wid_data_long <- wid_data %>%
  select(-percentile) %>%
  pivot_longer(
    cols = -year,
    names_to  = "country",
    values_to = "gini"
  )

wid_data_long %>%
  ggplot(aes(x = year, y = gini, colour = country)) +
  geom_smooth(method = "loess",
              se = FALSE,
              span = 0.2,
              size = 1.2) +
  scale_colour_manual(values = country_colours) +
  scale_y_continuous(limits = c(0.3, 0.55), expand = c(0,0)) +
  scale_x_continuous(limits = c(1998, 2023), expand = c(0,0)) +
  labs(
    title = NULL,
    x     = NULL,
    y     = "Gini coefficient",
    colour = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor       = element_blank(),
    panel.grid.major       = element_line(colour = "grey90"),
    legend.position        = "bottom",
    plot.title             = element_text(face = "bold", hjust = 0.5)
  ) +
  theme_linedraw() +
  theme(# panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = 'white', colour = 'white'),
    plot.background = element_rect(fill = '#FFF7F5', colour = '#FFF7F5'),
    legend.background = element_rect(fill = '#FFF7F5', colour = '#FFF7F5'),
    legend.key = element_rect(fill = '#FFF7F5', colour = '#FFF7F5'),
    axis.text.x = element_text(colour = "#22444b", size = 14),
    axis.text.y = element_text(colour = "#22444b", size = 14),
    axis.title.y = element_text(colour = "#22444b", size = 20),
    legend.text = element_text(colour = "#22444b", size = 14),
    legend.position = "bottom",
    legend.box = "horizontal")





# Figure 2 ----
svy_dat <- relevant %>%
  filter(!is.na(w2eq13)) %>% 
  as_survey_design(weights = w2weight) # declare survey design

svy_summary <- svy_dat %>% # compute weighted counts for each category of w2eq13
  group_by(w2eq13) %>%
  summarise(
    weighted_n = survey_total(),             # weighted count
    pct        = survey_mean(vartype = "ci") # weighted proportion + CI
  )

# plot of weighted counts
ggplot(svy_summary, aes(x = w2eq13, y = weighted_n)) +
  geom_col() +
  labs(
    x = "w2eq13 response",
    y = "Weighted count",
    title = "Weighted Distribution of w2eq13 (Wave 2)"
  )


# percentage plot
svy_pct <- svy_dat %>%
  group_by(w2eq13) %>%
  summarise(
    wt_n     = survey_total(vartype = "ci"),    # weighted count + CI
  ) %>%
  ungroup() %>%
  mutate(
    pct      = wt_n / sum(wt_n),                # proportion
    pct_low  = wt_n_low  / sum(wt_n),           # lower CI on proportion
    pct_upp  = wt_n_upp  / sum(wt_n)            # upper CI on proportion
  )

ggplot(svy_pct, aes(x = w2eq13, y = pct)) +
  geom_col_rounded(fill = "#AB110F") +
  geom_text(aes(label = paste(round(pct * 100, 0), "%")), position = position_dodge(0.9),
            vjust = 2, size = 4, color = "white") +
  # geom_errorbar(aes(ymin = pct_low, ymax = pct_upp), width = 0.2) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 0.4),
                     breaks = seq(0, 0.4, by = 0.1),
                     expand = c(0,0)) +
  scale_x_continuous(
    breaks = 1:5,
    labels = c(
      "Strongly agree",
      "Agree",
      "Neither agree\nnor disagree",
      "Disagree",
      "Strongly disagree"
    )
  ) +
  labs(
    #x     = "The state should increase the spending on social assistance on the poor",
    x = "",
    y     = "% of respondents"
  ) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = '#FFF7F5', colour = '#FFF7F5'),
        plot.background = element_rect(fill = '#FFF7F5', colour = '#FFF7F5'),
        axis.text.x = element_text(colour = "#22444b"),
        axis.text.y = element_text(colour = "#22444b"),
        axis.title.y = element_text(colour = "#22444b"))



# Figure 3 ----
## Binary Regression with weighted data
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
    model  = map(design,  ~ svyglm(policy_bin ~ income_bin,
                                   design = .x,
                                   family = quasibinomial())),
    # in svyglm for binomial and poisson you have to use quasibinomial() to aviod a warning about non-integer
    ## numbers of successes. The quasi version of the family objects give the same point estimates and st. errors
    tidy   = map(model,   ~ broom::tidy(.x) %>% filter(term == "income_bin"))
  ) %>%
  select(cntry, tidy) %>%
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
  scale_x_continuous(limits = c(-2.5, 2.5),
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
