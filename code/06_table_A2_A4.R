#' @name 06_table_A2_A4.R
#' @author Tim Fraser

## Table A2: Models of Social Welfare Regime Scores
load("table/table_soc.RData")
load("table/captions.RData")
get_ktable(tableno = "Table \\ref{tab:tablea2}", multiplier = 1) %>%
  saveRDS("table/table_A2.rds")


## Table A3: Models of Developmental Regime Scores
load("table/table_dev.RData")
load("table/captions.RData")
get_ktable(tableno = "Table \\ref{tab:tablea3}", multiplier = 2) %>%
  saveRDS("table/table_A3.rds")

## Table A4: Models of Middle Class Regime Scores
load("table/table_mid.RData")
load("table/captions.RData")
get_ktable(tableno = "Table \\ref{tab:tablea4}", multiplier = 3) %>%
  saveRDS("table/table_A4.rds")