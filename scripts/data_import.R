### load packages
library(here)
library(tidyverse)
library(readxl)
source(file.path("scripts", "_hulpfuncties.R"))
xls_gdrive_path <- file.path(
  "G:", "Mijn Drive", "PROJECTEN",
  "prj_crosscase_analyse_ESD"
)

### Load definitions

ess_def <-
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
  select(ess_common, ess_short, ess_order) |>
  unique() |>
  arrange(ess_order)

cluster_levs <-
  ess_def |>
  select(ess_cluster, cluster_short, ess_order) |>
  group_by(ess_cluster, cluster_short) |>
  summarise(cluster_order = min(ess_order), .groups = "drop") |>
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
  select(
    case = Case,
    respondent = Respondent,
    ess_cluster = Cluster,
    ess_common = ESD_common,
    ess_unique = ESD_uniek,
    score_original = Score_uniek,
    score_raw = Score
  )

data_orig <- data_raw |>
  mutate(
    ess_cluster =
      ifelse(is.na(ess_cluster) &
        ess_unique == "Waterinfiltratie en regulatie van waterstromen",
      "Water cycle related services",
      ess_cluster
      ),
    ess_cluster =
      ifelse(ess_cluster == "Cultural, social & esthetic values",
        "Cultural, social & aesthetic values",
        ess_cluster
      ),
    ess_cluster =
      ifelse(ess_cluster == "Recreation & tourism",
        "Recreation & Tourism",
        ess_cluster
      ),
    ess_common =
      ifelse(ess_common %in% c(
        "Food & feed production",
        "Wood & fibre production"
      ),
      "Bio-production",
      ess_common
      )
  ) |>
  left_join(case_def, join_by(case)) |>
  left_join(ess_def, join_by(ess_cluster, ess_common, ess_unique))

# CHECK
stopifnot(
  "Rijen komen niet overeen" =
    nrow(data_raw) == nrow(data_orig),
  " Niet alle data kan aan metadata gekoppeld worden" =
    all(sapply(
      data_orig,
      \(x) sum(is.na(x))
    )[c("cluster_short", "ess_short")] == 0)
)

### Analysedata

data_ana <- data_orig |>
  filter(!is.na(score_raw)) |>
  filter(!(is.na(respondent) & case == "Maarkebeek")) |> # fout in data
  mutate(
    score = ceiling(score_raw),
    f_score = ordered(score),
    case_short = factor(case_short,
      levels = case_levs$case_short
    ),
    cluster_short = factor(cluster_short,
      levels = cluster_levs$cluster_short
    ),
    ess_short = factor(ess_short,
      levels = ess_levs$ess_short
    ),
    ess_common = factor(ess_common,
      levels = ess_levs$ess_common
    )
  )

### Enkel data met individuele responses (zonder los)

data_respondent <- data_ana |>
  filter(!is.na(respondent)) |>
  group_by(case_short, ess_short, respondent) |>
  mutate(n_ess_unique = n()) |>
  group_by(case_short, cluster_short, ess_short, respondent, score, n_ess_unique) |>
  summarise(responses_raw = n(), .groups = "drop") |>
  mutate(weight_response = responses_raw / n_ess_unique) |>
  select(-responses_raw, -n_ess_unique)


### Geaggregeerde data voor gewogen gemiddelde

# processing anonymous data
data_wijgel_prc <- data_ana |>
  filter(substring(case_short, 1, 3) %in% c("Wij", "Gel")) |>
  # ess_unique per ess_short
  group_by(case_short, ess_short) |>
  mutate(n_ess_unique = n_distinct(ess_unique)) |>
  # counts
  group_by(case_short, cluster_short, ess_short, score, f_score, n_ess_unique) |>
  summarise(responses_raw = n(), .groups = "drop") |>
  mutate(
    n_responses = responses_raw / n_ess_unique,
    data_type = "anonymous"
  ) |>
  select(-responses_raw, -n_ess_unique)


# processen data met individuele responses
data_indiv_prc <-
  data_ana |>
  filter(!(substring(case_short, 1, 3) %in% c("Wij", "Gel"))) |>
  # bepaal het aantal unieke ess beantwoord per respondent
  group_by(case_short, ess_short, respondent) |>
  mutate(n_ess_unique_scored = n()) |>
  # maak een weging per individueel antwoord
  mutate(response_weight = 1 / n_ess_unique_scored) |>
  # aggregeer per ess_short (CORRECTED GROUP_BY)
  group_by(
    case_short, cluster_short, ess_short,
    score, f_score
  ) |>
  # Nu telt het alle gewichten (0.5, 0.33, 1.0, etc.)
  # voor een specifieke score correct samen
  summarise(n_responses = sum(response_weight), .groups = "drop") |>
  mutate(data_type = "individual")


## >>> create aggregated data
# Bind the two processed datasets
data_agg <- bind_rows(data_indiv_prc, data_wijgel_prc) |>
  group_by(case_short, cluster_short, ess_short) |>
  mutate(tot_responses = sum(n_responses)) |>
  mutate(frac_score = n_responses / tot_responses)


## >>> Create averaged data
max_possible_sd <- 2 # sd(c(rep(-1, 1e6),rep(3,1e6)))

data_avg <- data_agg |>
  group_by(case_short, cluster_short, ess_short) |>
  mutate(
    tot_resp = sum(n_responses), # N
    mean_score = sum(score * n_responses) / tot_resp # weighted mean (x_bar)
  ) |>
  summarise(
    # Calculate the numerator for variance: Σ[w_i * (x_i - x_bar)^2]
    sum_sq_diff = sum(n_responses * (score - mean_score)^2),
    mean_score = first(mean_score), # ipv mean_score[1]
    tot_resp = first(tot_resp),
    .groups = "drop"
  ) |>
  mutate(
    # Calculate Weighted Variance (s²_w)
    weighted_variance = sum_sq_diff / (tot_resp - 1),
    sd_score = sqrt(weighted_variance),
    # Calculate Standard Error of the Weighted Mean (SE)
    se_score = sqrt(weighted_variance / tot_resp),
    consensus = 1 - sd_score / max_possible_sd # terug naar sd
  )


### Afgeleide datasets

# error calculation for mean ess over the cases
data_avg_ess <- data_avg |>
  group_by(cluster_short, ess_short) |>
  summarise(
    # --- Method 1: Pooled (Inverse-Variance) ---
    # This is the statistically best-weighted mean.
    # We use na.rm = TRUE to ignore cases where std_error was NA
    # variance_i = std_error^2,
    # weight_i = 1 / std_error^2,
    pooled_mean = sum(1 / se_score^2 * mean_score, na.rm = TRUE) /
      sum(1 / se_score^2, na.rm = TRUE),
    pooled_se = sqrt(1 / sum(1 / se_score^2, na.rm = TRUE)),

    # --- Method 2: Simple Mean + SE of Means ---
    # This treats all cases equally.
    simple_mean = mean(mean_score, na.rm = TRUE),

    # YOUR FIX: Use if_else() to handle n=1
    # This calculates the SE of the *means*, not the pooled SE.
    se_of_means = if_else(
      n() > 1,
      sd(mean_score, na.rm = TRUE) / sqrt(n()),
      NA_real_ # Set to NA if n=1, as you can't get SE of 1 case
    ),
    propagated_se = (1 / n()) * sqrt(sum(se_score^2)),
    n_cases = n(),
    .groups = "drop"
  )


# error calculation for mean cqse over the ess
data_avg_case <- data_avg |>
  group_by(case_short) |>
  summarise(
    # --- Method 1: Pooled (Inverse-Variance) ---
    # This is the statistically best-weighted mean.
    # We use na.rm = TRUE to ignore cases where std_error was NA
    # variance_i = std_error^2,
    # weight_i = 1 / std_error^2,
    pooled_mean = sum(1 / se_score^2 * mean_score, na.rm = TRUE) /
      sum(1 / se_score^2, na.rm = TRUE),
    pooled_se = sqrt(1 / sum(1 / se_score^2, na.rm = TRUE)),

    # --- Method 2: Simple Mean + SE of Means ---
    # This treats all cases equally.
    simple_mean = mean(mean_score, na.rm = TRUE),

    # YOUR FIX: Use if_else() to handle n=1
    # This calculates the SE of the *means*, not the pooled SE.
    se_of_means = if_else(
      n() > 1,
      sd(mean_score, na.rm = TRUE) / sqrt(n()),
      NA_real_ # Set to NA if n=1, as you can't get SE of 1 case
    ),
    propagated_se = (1 / n()) * sqrt(sum(se_score^2)),
    n_ess = n(),
    .groups = "drop"
  )

# data_avg_respondent

data_avg_respondent <- data_respondent |>
  group_by(case_short, cluster_short, ess_short, respondent) |>
  summarise(score = sum(weight_response * score))


# wegschrijven gegenereerde data

if (!dir.exists("interim")) {
  dir.create("interim")
}
write_csv2(data_orig, file = here("interim", "data_orig.csv"))
saveRDS(data_raw, file = here("interim", "data_raw.rds"))
saveRDS(data_ana, file = here("interim", "data_ana.rds"))
saveRDS(data_respondent, file = here("interim", "data_respondent.rds"))
saveRDS(data_agg, file = here("interim", "data_agg.rds"))
saveRDS(data_avg, file = here("interim", "data_avg.rds"))
saveRDS(data_avg_ess, file = here("interim", "data_avg_ess.rds"))
saveRDS(data_avg_case, file = here("interim", "data_avg_case.rds"))
saveRDS(data_avg_respondent, file = here("interim", "data_avg_respondent.rds"))
