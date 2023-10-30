#' @name 02_make_indices.R
#' @title MAKE URBAN REGIME INDICES
#' @author Tim Fraser

# 0. Packages ##################################
library(dplyr)
library(matrixStats)
library(tibble)
library(readr)

# 1. Indices ####################################

## 1.1 Function ####################################

get_index = function(data){
  data %>%
    # Log transform all spending indicators, to account for strong right skew of rates;
    # use a constant to ensure zeros don't get left out
    mutate_at(vars(agr:housing), list(~log(. + 1))) %>%
    # Clip indicators at 0.5th and 99.5th percentiles (making a 99% confidence interval)
    mutate_at(vars(agr:housing), list(~case_when(
      . < quantile(., probs = 0.005, na.rm = TRUE) ~ quantile(., probs = 0.005, na.rm = TRUE),
      . > quantile(., probs = 0.995, na.rm = TRUE) ~ quantile(., probs = 0.995, na.rm = TRUE),
      TRUE ~ .))) %>%
    # Calculate standard deviations from the mean
    mutate_at(vars(agr:housing), list(~scale(.))) %>%
    # Average into basic indicators
    mutate(regime_dev = matrixStats::rowMeans2(x = select(., agr, com_manuf, construction, roads_bridges) %>% 
                                                 as.matrix(), na.rm = TRUE),
           regime_mid = matrixStats::rowMeans2(x = select(., education, edu_social, health, planning, waste) %>% 
                                                 as.matrix(), na.rm = TRUE),
           regime_soc = matrixStats::rowMeans2(x = select(., welfare, welfare_aged, welfare_kids, unemployment, emergency, housing) %>% as.matrix(), na.rm = TRUE)) %>%
    select(year, muni_code, regime_dev, regime_soc, regime_mid) %>%
    return()
}




# 2. Make Index ####################################

### Population controlled
read_rds("raw_data/indicators.rds") %>%
  get_index() %>% 
  left_join(by = c("muni_code", "year"),
            y = read_rds("raw_data/indicators.rds")) %>%
  
  left_join(by = "muni_code", y = read_rds("raw_data/muni_code.rds"))  %>%
  mutate(pref = str_sub(muni_code, 1,2)) %>%
  # Get rownames
  mutate(id = paste(muni_code, year, sep = "-")) %>%
  #  select(id, change) %>%
  #  filter( duplicated(id))
  #  duplicated() %>% sum()
  #  tibble::remove_rownames() %>%
  tibble::column_to_rownames("id") %>%
  saveRDS("raw_data/indices.rds")

read_rds("raw_data/indices.rds") %>% head()
