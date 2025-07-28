library(tidyverse)

xls_gdrive_path <- file.path("G:", "Mijn Drive", "PROJECTEN",
                             "prj_crosscase_analyse_ESD")
data_raw <-
  read_excel(
    file.path(
      xls_gdrive_path,
      "data",
      "ESD cases_Primary data_18dec2024.xlsx"
    ),
    sheet = "Data",
    range = "A1:G6216"
  )

cluster_def <- read_csv2(file.path("data", "cluster_definition.csv"))
ess_def <- read_csv2(file.path("data", "ess_overview.csv"))
ess_unique_short <- read_csv2(file.path("data", "ess_with_short_names.csv"))
case_short <- read_csv2(file.path("data", "case_definition.csv"))

cases <- data_raw |>
  select(case = Case) |>
  distinct() |>
  left_join(case_short, join_by(case))

ess_in_raw <-
  data_raw |>
  select(ess_unique = ESD_uniek,
         ess_common = ESD_common,
         ess_cluster = Cluster) |>
  distinct() |>
  mutate(
    ess_cluster =
      ifelse(is.na(ess_cluster) &
               ess_unique == "Waterinfiltratie en regulatie van waterstromen",
             "Water cycle related services",
             ess_cluster)) |>
  left_join(cluster_def,
            join_by(ess_cluster == cluster_name)) |>
  left_join(ess_def |> select(-remark),
            join_by(ess_common, ess_cluster)) |>
  mutate(rnam = row_number()) |>
  left_join(ess_unique_short |>
              select(ess_cluster, ess_common, ess_unique, unique_short),
            join_by(ess_cluster, ess_common, ess_unique)) |>
  distinct() |>
  select(ess_cluster, cluster_short,
         ess_common, ess_short, ess_order,
         unique_short, ess_unique)

#Voeg records toe
Bio-production;Bio-production;Bio-production;bio-production;4;landbouw;Landbouwactiviteiten
Bio-production;Bio-production;Bio-production;bio-production;4;hout prd;houtproductie
Verander alles ESS_common "Wood & fibre production" naar "Bio-production"




write_csv2(ess_in_raw,
           file = file.path("data", "ess_definition.csv"))



