library(tidyverse)
library(brms)
conflicted::conflict_prefer_all(c("purrr"), quiet = TRUE)
conflicted::conflict_prefer_all(c("tidyr"), quiet = TRUE)
conflicted::conflict_prefer_all(c("dplyr"), quiet = TRUE)
conflicted::conflict_prefer_all(c("brms"), quiet = TRUE)


# Load the data
data_ana <- readRDS("interim/data_ana.rds")

newdata <- data_ana %>%
  filter(!is.na(score)) %>%
  select( ess_cluster, cluster_short, ess_common, ess_short) %>%
  mutate(case = "global", case_short = "global", respondent = 0) |>
  distinct()

newdata_case <- data_ana %>%
  filter(!is.na(score)) %>%
  select(case, case_short, ess_cluster, cluster_short, ess_common, ess_short) %>%
  mutate(respondent = 0) |>
  distinct()

saveRDS(newdata, file = "models/newdata.RDS")
saveRDS(newdata_case, file = "models/newdata_case.RDS")

#geaggregeerde data
data_agg <- data_ana %>%
  group_by(case, ess_cluster, cluster_short, ess_common, ess_short) %>%
  summarise(mean_score = mean(score, na.rm = TRUE),
            n_respondents = n(),
            .groups = "drop")

### Gewogen gemiddelde

model_simpmeans <- brm(
  mean_score | weights(n_respondents) ~ 0 + ess_short + (1 | case),
  data = data_agg,
  family = gaussian(),
  prior = c(set_prior("normal(0, 5)", class = "b"),
            set_prior("cauchy(0, 2)", class = "sigma")),
  chains = 4,
  cores = 4,
  iter = 3000,
  warmup = 1000
)

set.seed(12345)
model_simpmeans_draws <- as_draws_df(model_simpmeans)

model_simpmeans_preds <-
  posterior_predict(model_simpmeans,
                    newdata = newdata,re_formula = NULL,
                    allow_new_levels = TRUE,
                    ndraws = 1000)

model_simpmeans_preds_case <-
  posterior_predict(model_simpmeans,
                    newdata = newdata_case,
                    ndraw = 1000)

saveRDS(model_simpmeans, file = "models/simplified_weighted_means.RDS")
saveRDS(model_simpmeans_draws, file = "models/simplified_weighted_means_draws.RDS")
saveRDS(model_simpmeans_preds, file = "models/simplified_weighted_means_preds.RDS")
saveRDS(model_simpmeans_preds_case, file = "models/simplified_weighted_means_preds_case.RDS")

### Cumlulatief logistisch model

model_simpcl <- brm(
  f_score ~ ess_short + (1 | case),
  data = data_ana,
  family = cumulative("logit"),
  chains = 4,
  iter = 2000,
  cores = 4
)

set.seed(12345)
model_simpcl_draws <- as_draws_df(model_simpcl)


model_simpcl_fit <-  fitted(model_simpcl,
                            newdata = newdata,
                            summary = FALSE,
                            allow_new_levels = TRUE)

model_simpcl_preds <-
  posterior_predict(model_simpcl,
                    newdata = newdata,re_formula = NULL,
                    allow_new_levels = TRUE,
                    ndraws = 1000)


model_simpcl_fit_case <-  fitted(model_simpcl,
                            newdata = newdata_case,
                            summary = FALSE)

model_simpcl_preds_case <-
  posterior_predict(model_simpcl,
                    newdata = newdata_case,
                    ndraw = 1000)

saveRDS(model_simpcl, file = "models/simplified_cumulative.RDS")
saveRDS(model_simpcl_draws, file = "models/simplified_cumulative_draws.RDS")
saveRDS(model_simpcl_preds, file = "models/simplified_cumulative_preds.RDS")
saveRDS(model_simpcl_preds_case, file = "models/simplified_cumulatives_preds_case.RDS")
saveRDS(model_simpcl_fit, file = "models/simplified_cumulative_fit.RDS")
saveRDS(model_simpcl_fit_case, file = "models/simplified_cumulative_fit_case.RDS")


### Hurdle model

data_ana$scorehu <- data_ana$score + 1
model_simp_hurdle <- brm(
  bf(scorehu ~ ess_short + (1 | case),
     hu ~ ess_short),
  data = data_ana,
  family = hurdle_lognormal(),
  chains = 4,
  iter = 2000,
  cores = 4
)

set.seed(12345)
model_simp_hurdle_draws <- as_draws_df(model_simp_hurdle)

model_simp_hurdle_fit <-  fitted(model_simp_hurdle,
                            newdata = newdata,
                            summary = FALSE,
                            allow_new_levels = TRUE)

model_simp_hurdle_preds <-
  posterior_predict(model_simp_hurdle,
                    newdata = newdata,re_formula = NULL,
                    allow_new_levels = TRUE,
                    ndraws = 1000)


model_simp_hurdle_fit_case <-  fitted(model_simp_hurdle,
                                 newdata = newdata_case,
                                 summary = FALSE)

model_simp_hurdle_preds_case <-
  posterior_predict(model_simp_hurdle,
                    newdata = newdata_case,
                    ndraw = 1000)

saveRDS(model_simp_hurdle, file = "models/simplified_hurdle.RDS")
saveRDS(model_simp_hurdle_draws, file = "models/simplified_hurdle_draws.RDS")
saveRDS(model_simp_hurdle_fit, file = "models/simplified_hurdle_fit.RDS")
saveRDS(model_simp_hurdle_preds, file = "models/simplified_hurdle_preds.RDS")
saveRDS(model_simp_hurdle_fit_case, file = "models/simplified_hurdle_fit_case.RDS")
saveRDS(model_simp_hurdle_preds_case, file = "models/simplified_hurdle_preds_case.RDS")



### Cumulatief logistisch model met respondent-niveau info

data_resp <- data_ana |>
  filter(!is.na(respondent)) |>
  mutate(case_respondent = paste(case, respondent, sep = "_"))

newdata_resp <- newdata |>
  mutate(case_respondent = "global_0") |>
  filter(ess_short %in% data_resp$ess_short)

newdata_resp_case <- newdata_case |>
  mutate(case_respondent = paste(case, "0", sep = "_")) |>
  filter(ess_short %in% data_resp$ess_short)


model_resp_cl <- brm(
  f_score ~ ess_short + (1 | case) + (1 | case_respondent),
  data = data_resp,
  family = cumulative("logit"),
  chains = 4,
  iter = 2000,
  cores = 4
)

set.seed(12345)
model_resp_cl_draws <- as_draws_df(model_resp_cl)

model_resp_cl_fit <-  fitted(model_resp_cl,
                                 newdata = newdata_resp,
                                 summary = FALSE,
                                 sample_new_levels = "gaussian")

model_resp_cl_preds <-
  posterior_predict(model_resp_cl,
                    newdata = newdata_resp,re_formula = NULL,
                    allow_new_levels = TRUE,
                    ndraws = 1000)

model_resp_cl_fit_case <-  fitted(model_resp_cl,
                                      newdata = newdata_resp_case,
                                      summary = FALSE)

model_resp_cl_preds_case <-
  posterior_predict(model_resp_cl,
                    newdata = newdata_resp_case,
                    ndraw = 1000)

saveRDS(model_resp_cl, file = "models/resp_cumulative.RDS")
saveRDS(model_resp_cl_draws, file = "models/resp_cumulative_draws.RDS")
saveRDS(model_resp_cl_fit, file = "models/resp_cumulative_fit.RDS")
saveRDS(model_resp_cl_preds, file = "models/resp_cumulative_preds.RDS")
saveRDS(model_resp_cl_fit_case, file = "models/resp_cumulative_fit_case.RDS")
saveRDS(model_resp_cl_preds_case, file = "models/resp_cumulative_preds_case.RDS")

