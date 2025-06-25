library(tidyverse)
library(brms)

# Load the data
data_ana <- readRDS("data/data_ana.rds")

#geaggregeerde data
data_agg <- data_ana %>%
  group_by(case, ess_cluster, cluster_short, ess_common, ess_short) %>%
  summarise(mean_score = mean(score, na.rm = TRUE),
            n_respondents = n(),
            .groups = "drop")


