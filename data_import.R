
### load packages

library(tidyverse)
library(readxl)
source("_hulpfuncties.R")
xls_gdrive_path <- "G:\\Mijn Drive\\PROJECTEN\\prj_crosscase_analyse_ESD"

### Load data

ess_interpretation <- read_csv2("data/ess_overview.csv",
  show_col_types = FALSE
)
case_def <- read_csv2("data/case_definition.csv",
  show_col_types = FALSE
)
cluster_def <- read_csv2("data/cluster_definition.csv",
  show_col_types = FALSE
)
ess_def <- ess_interpretation |>
  left_join(cluster_def,
            join_by(ess_cluster == cluster_name))

# read the data from the excel file
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
  # replace missing Cluster value for Kluisbos
  replace_na(list(Cluster = "Water cycle related services")) |>
  rename(
    case = "Case",
    respondent = "Respondent",
    ess_cluster = "Cluster",
    ess_common = "ESD_common",
    ess_unique = "ESD_uniek",
    score_original = "Score_uniek",
    score = "Score"
  ) |>
  mutate(
    ess_common = ifelse(ess_common %in% c(
      "Food & feed production",
      "Wood & fibre production"
    ),
    "Bio-production",
    ess_common
    ),
    ess_cluster = ifelse(ess_cluster == "Recreation & tourism",
      "Recreation & Tourism",
      ess_cluster
    ),
    score = ceiling(score)
  )

# Inlezen gedefinieerde categorisering van de ESS
data_cat_orig <-
  read_excel(
    file.path(
      xls_gdrive_path,
      "data",
      "ESD cases_Primary data_18dec2024.xlsx"
    ),
    sheet = "Classificatie van ESD"
  ) |>
  rename(
    ess_cluster = "ESD Cluster (bottom-up)",
    ess_common = "Subgroup",
    ess_std = "ESD gestandariseerde namen"
  )

### Primaire processing van de data

data_cat <- data_cat_orig |>
  fill(names(data_cat_orig)[1:2], .direction = "down") |>
  mutate(
    ess_std = ifelse(is.na(ess_std), ess_common, ess_std),
    ess_std = ifelse(ess_std %in% c("Wood & fibre production",
                                    "Food & feed production"),
      "Bio-production",
      ess_std
    ),
    ess_cluster = ifelse(ess_cluster == "Cultural, social & esthetic values",
      "Cultural, social & aesthetic values",
      ess_cluster
    ),
    ess_cluster = ifelse(ess_cluster == "Recreation & tourism",
      "Recreation & Tourism",
      ess_cluster
    )
  )

data_cat_wide <- data_cat |>
  pivot_longer(
    cols = -c(ess_cluster, ess_common, ess_std),
    names_to = "ess_unique",
    values_to = "ess_short"
  ) |>
  filter(!is.na(ess_short))

# check if the categorization in data_cat matches with data_raw

data_cat_clusters <- sort(unique(data_cat_wide$ess_cluster))

data_cat_uniques <- c(sort(unique(data_cat_wide$ess_std)), rep(NA, 5))
data_raw_uniques <- c(sort(unique(data_raw$ess_common)), rep(NA, 5))

cbind(
  cat = sort(unique(data_cat_wide$ess_cluster)),
  dat = sort(unique(data_raw$ess_cluster))
) |>
  as.data.frame() |>
  mutate(equal = cat == dat)

cbind(cat = data_cat_uniques, dat = data_raw_uniques) |>
  as.data.frame() |>
  mutate(equal = cat == dat) |>
  view()



### Klaarzetten analysedata

data_orig <- data_raw |>
  rename_with(tolower) |>
  select(
    case, respondent, ess_unique,
    ess_common, ess_cluster,
    score_original, score
  ) |>
  mutate(
    f_score = ordered(round(score)),
    ess_cluster = ifelse(ess_cluster == "Recreation & tourism",
      "Recreation & Tourism",
      ess_cluster
    ),
    ess_common = ifelse(
      ess_common %in% c("Food & feed production", "Wood & fibre production"),
      "bio-production",
      ess_common
    )
  ) |>
  left_join(ess_def |>
    select(ess_cluster, ess_common, ess_short, ess_order),
    by = c("ess_cluster", "ess_common")
  ) |>
  left_join(data_cat_wide |>
    select(ess_unique, ess_short, ess_std),
    by = c("ess_unique", "ess_short")
  ) |>
  left_join(case_def |>
    select(case, case_short, case_order),
    by = "case"
  )

# Analysedata aanmaken

data_ana_tmp <- data_orig %>%
  select(-ess_order) |>
  left_join(ess_def |> filter(ess_common %in% data_orig$ess_common),
            join_by(ess_cluster == ess_cluster,
                    ess_short == ess_short,
                    ess_common == ess_common))

case_levs <-
  data_ana_tmp |>
  select(case, case_short, case_order) |>
  unique() |>
  arrange(case_order)

ess_levs <-
  data_ana_tmp |>
  select(ess_common, ess_short, ess_order)|>
  unique() |>
  arrange(ess_order)

cluster_levs <-
  data_ana_tmp |>
  select(ess_cluster, cluster_short, ess_order) |>
  group_by(ess_cluster, cluster_short) |>
  summarise(cluster_order= min(ess_order), .groups = "drop") |>
  arrange(cluster_order)

data_ana <- data_ana_tmp |>
  mutate(case = factor(case,
                       levels = case_levs$case),
         case_short = factor(case_short,
                             levels = case_levs$case_short),
         cluster_short = factor(cluster_short,
                                levels = cluster_levs$cluster_short),
         ess_short = factor(ess_short,
                            levels = ess_levs$ess_short),
         ess_common = factor(ess_common,
                             levels = ess_levs$ess_common)) |>
  filter(!is.na(score)) |>
  select(case, case_short, respondent, ess_cluster, cluster_short,
         ess_unique, ess_common, ess_short,
         score_original, score, f_score)



### Geaggregeerde data voor gewogen gemiddelde

data_wg <-
  data_ana |>
  filter(substring(case, 1, 8) %in% c("De Wijer", "Gelinden")) |>
  # First summarize to get counts per unique combination
  group_by(case, case_short, ess_cluster, cluster_short, ess_short, ess_common) |>
  summarise(mean_unique = mean(score, na.rm = TRUE),
            n_resp = n(),
            sd_unique = sd(score, na.rm = TRUE),
            se_unique = sd(score, na.rm = TRUE) / sqrt(n()),
            .groups = "drop") |>

  # Second summarize to get weighted means and standard errors
  group_by(case, case_short, ess_cluster, cluster_short, ess_short, ess_common) |>
  summarise(m_score = sum(mean_unique * n_resp) / sum(n_resp),
            se_score = sqrt(sum(se_unique^2 * n_resp^2) / sum(n_resp^2)),
            n_respondents = mean(n_resp),
            .groups = "drop") |>
  mutate(f_score = round(m_score))

#Data per respondent for which individual measurements are available
data_rest_indiv <-
  data_ana|>
  filter(!substring(case, 1, 8) %in% c("De Wijer", "Gelinden"),
         !is.na(score)) |>
  group_by(case, case_short, ess_cluster, cluster_short,
           ess_short, ess_common, respondent) |>
  summarise(n_respondents = 1,
            n_ess_unique = n(),
            m_score_i = mean(score, na.rm = TRUE),
            sd_m_score = sd(score, na.rm = TRUE),
            se_m_score = sd(score, na.rm = TRUE) / sqrt(n()),
            .groups = "drop") |>
  mutate(f_score = round(m_score_i))

#Data averaged over individual respondent
data_rest <-
  data_rest_indiv |>
  replace_na(list(se_m_score = 0, sd_m_score = 0)) |>
  group_by(case, case_short, ess_cluster, cluster_short,
           ess_short, ess_common) |>
  summarise(n_respondents = n(),
            n_ess_unique = mean(n_ess_unique),
            m_score = sum(m_score_i)/n_respondents,
            var_within = sum(n_respondents * sd_m_score^2) / sum(n_respondents),
            var_between = var(m_score_i),
            .groups = "drop") |>
  mutate(se_score = sqrt((var_within/n_ess_unique + var_between)/n_respondents),
         f_score = round(m_score))

#Combine data
data_avg <- bind_rows(data_wg, data_rest)




# wegschrijven gegenereerde data

if (!dir.exists("interim")) {
  dir.create("interim")
}
readr::write_csv2(data_orig, file = "interim/data_orig.csv")
saveRDS(data_ana, file = "interim/data_ana.rds")
saveRDS(data_avg, file = "interim/data_avg.rds")

