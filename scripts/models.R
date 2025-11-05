library(tidyverse)
library(brms)
conflicted::conflict_prefer_all(c("dplyr"), quiet = TRUE)
conflicted::conflict_prefer_all(c("purrr"), quiet = TRUE)
conflicted::conflict_prefer_all(c("tidyr"), quiet = TRUE)
conflicted::conflict_prefer_all(c("brms"), quiet = TRUE)

ess_def <- readr::read_csv2("data/ess_definition.csv",
  show_col_types = FALSE
)

# Laad de data

data_ana <- readr::read_csv2("interim/data_ana.csv",
  show_col_types = FALSE
)
data_agg <- readRDS("interim/data_agg.rds")
data_avg <- readRDS("interim/data_avg.rds")
data_avg_respondent <- readRDS("interim/data_avg.RDS")

data_cml <- data_agg |>
  arrange(case_short, cluster_short, ess_short, f_score) |>
  group_by(case_short, cluster_short, ess_short) |>
  mutate(
    resp_cml = cumsum(n_responses),
    frac_cml = cumsum(n_responses) / sum(n_responses)
  )

############################################################################
## GEWOGEN GEMIDDELDE
############################################################################

model_simpmeans <- brm(
  m_score | weights(n_respondents) ~ 1 + ess_short + (1 | case_short),
  data = data_avg,
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 2)", class = "b"), # scores lopen van -1 tot 3, dus een 95% variatie van -5 tot +5 zou meer dan voldoende moeten zijn
    set_prior("normal(0, 1.5)", class = "sigma"), # auto zero-truncated
    set_prior("normal(0, 1)", class = "sd")
  ), # auto zero-truncated, cases zullen nooit heel veel afwijken van elkaar
  chains = 4,
  cores = 4,
  iter = 3000,
  warmup = 1000,
  control = list(adapt_delta = 0.80) # 0.8 = default
)
saveRDS(model_simpmeans, file = "models/simplified_weighted_means.RDS")

############################################################################
## GEWOGEN GEMIDDELDE INDIV (7 cases)
############################################################################

model_means_resp <- brm(
  score ~ 1 + ess_short + (1 | case_short / respondent),
  data = data_avg_respondent,
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 2)", class = "b")
  ),
  chains = 4,
  cores = 4,
  iter = 3000,
  warmup = 1000,
  control = list(adapt_delta = 0.80)
)
saveRDS(model_means_resp, file = "models/respondent_weighted_means.RDS")
