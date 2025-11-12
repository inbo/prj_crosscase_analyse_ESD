library(tidyverse)
library(brms)
conflicted::conflict_prefer_all(c("dplyr"), quiet = TRUE)
conflicted::conflict_prefer_all(c("purrr"), quiet = TRUE)
conflicted::conflict_prefer_all(c("tidyr"), quiet = TRUE)
conflicted::conflict_prefer_all(c("dplyr"), quiet = TRUE)
conflicted::conflict_prefer_all(c("purrr"), quiet = TRUE)
conflicted::conflict_prefer_all(c("tidyr"), quiet = TRUE)
conflicted::conflict_prefer_all("stats", quiet = TRUE)
conflicted::conflict_prefer_all("brms", quiet = TRUE)


ess_def <- readr::read_csv2("data/ess_definition.csv",
  show_col_types = FALSE
)

# Laad de data

data_ana <- readr::read_csv2("interim/data_ana.csv",
  show_col_types = FALSE
)
data_respondent <- readRDS("interim/data_respondent.rds")
data_agg <- readRDS("interim/data_agg.rds")
data_avg <- readRDS("interim/data_avg.rds")
data_avg_respondent <- readRDS("interim/data_avg_respondent.RDS")


data_cml <- data_agg |>
  arrange(case_short, cluster_short, ess_short, f_score) |>
  group_by(case_short, cluster_short, ess_short) |>
  mutate(
    resp_cml = cumsum(n_responses),
    frac_cml = cumsum(n_responses) / sum(n_responses)
  )

data_cml_indiv <- data_respondent |>
  dplyr::filter(!is.na(respondent)) |>
  arrange(case_short, cluster_short, ess_short, score, respondent) |>
  dplyr::mutate(f_score = factor(score, ordered = TRUE, levels = c(-1, 0, 1, 2, 3)))


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
  control = list(adapt_delta = 0.95) # 0.8 = default
)
saveRDS(model_simpmeans, file = "models/simplified_weighted_means.RDS")

############################################################################
## GEWOGEN GEMIDDELDE INDIV (gekend resondent level)
############################################################################

# nog enkele divergent transitions proberen weg te werken, door extra priors en adapt delta



model_means_resp <- brm(
  score ~ 1 + ess_short + (1 | case_short / respondent),
  data = data_avg_respondent,
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 1.5)", class = "b"),
    set_prior("normal(0, 0.5)", class = "sd"),
    set_prior("normal(0, 0.5)", class = "sd", group = "case_short:respondent")
  ),
  chains = 4,
  cores = 4,
  iter = 3000,
  warmup = 1000,
  control = list(adapt_delta = 0.95)
)
saveRDS(model_means_resp, file = "models/respondent_weighted_means.RDS")


############################################################################
## Cumulatief logistisch model
#############################################################################

# weging nodig?
# weging op n_responses, die op zich al rekening houdt dat iedere respondent maar
#  als 1 meetelt, de scores over de unieke ecosysteemdiensten worden reeds verdeeld

my_priors_simple <- c(
  prior(normal(0, 1), class = sd),
  prior(normal(0, 2.5), class = b),
  prior(normal(0, 5), class = Intercept)
)

model_clm_simple <- brm(
  f_score | weights(n_responses) ~ ess_short + (1 | case_short),
  data = data_cml,
  family = cumulative("logit"),
  prior = my_priors_simple,
  chains = 4,
  iter = 4000,
  cores = 4
)
saveRDS(model_clm_simple, file = "models/simplified_cumlogit_model.RDS")


############################################################################
## Cumulatief logistisch model cases met respondent level
#############################################################################

# dit model is traag (enkele minuten) om te schatten
# weging op weight_respnse, om iedere respondent evenveel te laten meetellen
#  ongeacht het aantal unieke ecosysteemdiensten die beoordeeld heeft

my_priors_indiv <- c(
  prior(normal(0, 1), class = sd),
  prior(normal(0, 2.5), class = b),
  prior(normal(0, 5), class = Intercept)
)

model_clm_indiv <- brm(
  f_score | weights(weight_response) ~ ess_short + (1 | case_short / respondent),
  data = data_cml_indiv,
  family = cumulative("logit"),
  prior = my_priors_indiv,
  chains = 4,
  iter = 3000,
  cores = 4
)
saveRDS(model_clm_indiv, file = "models/respondent_cumlogit_model.RDS")
