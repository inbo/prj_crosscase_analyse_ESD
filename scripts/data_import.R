
### load packages
library(here)
library(tidyverse)
library(readxl)
source(file.path("scripts", "_hulpfuncties.R"))
xls_gdrive_path <- file.path("G:", "Mijn Drive", "PROJECTEN",
                             "prj_crosscase_analyse_ESD")

### Load definitions

ess_def  <-
  read_csv2(here("data", "ess_definition.csv"), show_col_types = FALSE) |>
  distinct()
case_def <-
  read_csv2(here("data", "case_definition.csv"), show_col_types = FALSE)

case_levs <-
  case_def |>
  select(case, case_short, case_order) |>
  unique() |>
  arrange(case_order)

ess_levs <-
  ess_def |>
  select(ess_common, ess_short, ess_order)|>
  unique() |>
  arrange(ess_order)

cluster_levs <-
  ess_def |>
  select(ess_cluster, cluster_short, ess_order) |>
  group_by(ess_cluster, cluster_short) |>
  summarise(cluster_order= min(ess_order), .groups = "drop") |>
  arrange(cluster_order)


###  read the data from the excel file

data_raw <-
  read_excel(
    file.path(
      xls_gdrive_path,
      "data",
      "ESD cases_Primary data_18dec2024.xlsx"
    ),
    sheet = "Data",
    range = "A1:G6216"
  ) |>
  select(case = Case,
         respondent = Respondent,
         ess_cluster = Cluster,
         ess_common = ESD_common,
         ess_unique = ESD_uniek,
         score_original = Score_uniek,
         score_raw = Score)

data_orig <- data_raw |>
  mutate(
    ess_cluster =
      ifelse(is.na(ess_cluster) &
               ess_unique == "Waterinfiltratie en regulatie van waterstromen",
             "Water cycle related services",
             ess_cluster),
    ess_cluster =
      ifelse(ess_cluster == "Cultural, social & esthetic values",
             "Cultural, social & aesthetic values",
             ess_cluster),
    ess_cluster =
      ifelse(ess_cluster == "Recreation & tourism",
             "Recreation & Tourism",
             ess_cluster),
    ess_common =
      ifelse(ess_common %in% c("Food & feed production",
                               "Wood & fibre production"),
             "Bio-production",
             ess_common)) |>
  left_join(case_def, join_by(case)) |>
  left_join(ess_def, join_by(ess_cluster, ess_common, ess_unique))

#CHECK
stopifnot(
  "Rijen komen niet overeen" =
    nrow(data_raw) == nrow(data_orig),
  " Niet alle data kan aan metadata gekoppeld worden" =
    all(sapply(data_orig,
               \(x) sum(is.na(x)))[c("cluster_short", "ess_short")] == 0)
)

### Analysedata

data_ana <- data_orig |>
  filter(!is.na(score_raw)) |>
  filter(!(is.na(respondent) & case == "Maarkebeek")) |>  #fout in data
  mutate(score = ceiling(score_raw),
         f_score = ordered(score),
         case_short = factor(case_short,
                             levels = case_levs$case_short),
         cluster_short = factor(cluster_short,
                                levels = cluster_levs$cluster_short),
         ess_short = factor(ess_short,
                            levels = ess_levs$ess_short),
         ess_common = factor(ess_common,
                             levels = ess_levs$ess_common))


### Geaggregeerde data voor gewogen gemiddelde

# First summarize to get counts per unique combination
# Second summarize to get weighted means and standard errors

n_resp_per_case_ess_unique <- data_ana |>
  group_by(case_short, ess_short, ess_unique) |>
  summarise(n_respondents = n(), .groups = "drop_last")

n_resp_per_case_ess <- n_resp_per_case_ess_unique |>
  summarise(n_respondents = max(n_respondents), .groups = "drop_last")

n_resp_per_case <- n_resp_per_case_ess |>
  summarise(n_respondents = max(n_respondents), .groups = "drop")


data_wijgel <-
  data_ana |>
  filter(substring(case_short, 1, 3) %in% c("Wij", "Gel")) |>
  group_by(case_short, cluster_short, ess_short, ess_unique, score, f_score) |>
  summarise(aantal = n(), .groups = "drop") |>
  group_by(case_short, cluster_short, ess_short) |>
  mutate(n_tot = sum(aantal)) |>
  group_by(case_short, cluster_short, ess_short, score, f_score) |>
  summarise(n_responses = max(n_tot),
            aantal = sum(aantal),
            .groups = "drop") |>
  left_join(n_resp_per_case_ess, join_by(case_short, ess_short)) |>
  mutate(frac_score = aantal / n_responses)




#wat met NA values, misschien n_respondents anders berekenen
data_indiv <-
  data_ana |>
  filter(!(substring(case_short, 1, 3) %in% c("Wij", "Gel"))) |>
  group_by(case_short, cluster_short, ess_short, ess_unique, respondent, score, f_score) |>
  summarise(aantal = n(), .groups = "drop") |>
  group_by(case_short, cluster_short, ess_short, respondent) |>
  mutate(n_tot = sum(aantal)) |>
  group_by(case_short, cluster_short, ess_short, respondent, score, f_score) |>
  summarise(n_responses_indiv = max(n_tot),
            aantal = sum(aantal),
            .groups = "drop") |>
  left_join(n_resp_per_case_ess, join_by(case_short, ess_short)) |>
  mutate(frac_score_indiv = aantal / n_responses_indiv)


# terug op case niveau
data_indiv_aggregated <- data_indiv |>
  group_by(case_short, cluster_short, ess_short, score, f_score, n_respondents) |>
  summarise(
    aantal = sum(frac_score_indiv),
    .groups = "drop"
  ) |>
  mutate(frac_score = aantal / n_respondents)


data_avg <- bind_rows(data_wijgel, data_indiv_aggregated)



  # left_join(n_resp_per_case, join_by(case_short)) |>
  # group_by(case, case_short, ess_cluster, cluster_short, ess_short, ess_common) |>
  # summarise(mean_unique = mean(score, na.rm = TRUE),
  #           n_resp = max(n_resp_tot_case),
  #           sd_unique = sd(score, na.rm = TRUE),
  #           se_unique = sd(score, na.rm = TRUE) / sqrt(n()),
  #           .groups = "drop") |>
  # group_by(case, case_short, ess_cluster, cluster_short, ess_short, ess_common) |>
  # summarise(m_score = sum(mean_unique * n_resp) / sum(n_resp),
  #           se_score = sqrt(sum(se_unique^2 * n_resp^2) / sum(n_resp^2)),
  #           sd_score = se_score * sqrt(n_resp),
  #           n_respondents = mean(n_resp),
  #           .groups = "drop") |>
  # mutate(f_score = round(m_score))

#Data per respondent for which individual measurements are available
# data_rest_indiv <-
#   data_ana |>
#   filter(!substring(case, 1, 3) %in% c("Wij", "Gel"),
#          !is.na(score)) |>
#   left_join(n_resp_per_case, join_by(case_short)) |>
#   group_by(case, case_short, ess_cluster, cluster_short,
#            ess_short, ess_common, respondent) |>
#   summarise(n_respondents = 1,
#             n_ess_unique = n(),
#             m_score_i = mean(score, na.rm = TRUE),
#             sd_m_score = sd(score, na.rm = TRUE),
#             se_m_score = sd(score, na.rm = TRUE) / sqrt(n()),
#             .groups = "drop") |>
#   mutate(f_score = round(m_score_i))
#
# #Data averaged over individual respondent
# data_rest <-
#   data_rest_indiv |>
#   replace_na(list(se_m_score = 0, sd_m_score = 0)) |>
#   group_by(case, case_short, ess_cluster, cluster_short,
#            ess_short, ess_common) |>
#   summarise(n_respondents = n(),
#             n_ess_unique = mean(n_ess_unique),
#             m_score = sum(m_score_i)/n_respondents,
#             var_within = sum(n_respondents * sd_m_score^2) / sum(n_respondents),
#             var_between = var(m_score_i),
#             .groups = "drop") |>
#   mutate(sd_score = sqrt((var_within/n_ess_unique + var_between)),
#          se_score = sd_score / sqrt(n_respondents),
#          f_score = round(m_score))
#
# #Combine data
# data_avg <- bind_rows(data_wg, data_rest)


### verkennende data

max_possible_sd <- 2 #sd(c(rep(-1, 100000), rep(3, 100000)))

data_common <- data_ana |>
  group_by(case_short, cluster_short, ess_short, ess_unique, f_score) |>
  summarise(aantal = n(), .groups = "drop") |>
  group_by(case_short, cluster_short, ess_short) |>
  mutate(n_tot = sum(aantal),
         n_pers = max(aantal)) |>
  group_by(case_short, cluster_short, ess_short, f_score, n_pers) |>
  summarise(n_resp = max(n_tot),
            aantal = sum(aantal),
            .groups = "drop") |>
  mutate(fr_score = aantal / n_resp,
         nval_score = as.numeric(as.character(f_score))) |>
  group_by(case_short, cluster_short, ess_short, n_pers) |>
  mutate(mean_score = sum(fr_score * nval_score),
         sd_score = sd(rep(nval_score, rep(aantal))),
         consensus = 1 - (sd_score / max_possible_sd))


# wegschrijven gegenereerde data

if (!dir.exists("interim")) {
  dir.create("interim")
}
write_csv2(data_orig,   file = here("interim", "data_orig.csv"))
saveRDS(   data_raw,    file = here("interim", "data_raw.rds"))
saveRDS(   data_ana,    file = here("interim", "data_ana.rds"))
saveRDS(   data_avg,    file = here("interim", "data_avg.rds"))
saveRDS(   data_common, file = here("interim", "data_common.rds"))

