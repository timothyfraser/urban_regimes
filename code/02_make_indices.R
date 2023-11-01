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


# 3. Make Categories ###################################


read_rds("raw_data/indices.rds")  %>%
  select(year, muni_code,  contains("regime")) %>%
  # Cut the 14 city-years where regime scores were not computed due to missing data
  filter(!is.na(regime_dev)) %>%
  pivot_longer(cols = contains("regime"), names_to = "type", values_to = "regime") %>%
  mutate(type = type %>% str_remove("_pop") %>% recode_factor(
    "regime_dev" = "Developmental", 
    "regime_mid" = "Middle Class",
    "regime_soc" = "Social Welfare")) %>%
  group_by(type) %>%
  mutate(class = (ntile(regime, 2) - 1)) %>%
  ungroup() %>%
  select(-regime) %>%
  pivot_wider(
    id_cols = c(year, muni_code),
    names_from = type,
    values_from = class) %>%
  mutate(type = case_when(
    Developmental == 1 & `Middle Class` == 1 & `Social Welfare` == 1 ~ "Social Welfare / Middle Class / Developmental",
    Developmental == 0 & `Middle Class` == 1 & `Social Welfare` == 1 ~ "Social Welfare / Middle Class",
    Developmental == 1 & `Middle Class` == 0 & `Social Welfare` == 1 ~ "Social Welfare / Developmental",
    Developmental == 0 & `Middle Class` == 0 & `Social Welfare` == 1 ~ "Social Welfare",
    Developmental == 1 & `Middle Class` == 1 & `Social Welfare` == 0 ~ "Middle Class / Developmental",
    Developmental == 0 & `Middle Class` == 1 & `Social Welfare` == 0 ~ "Middle Class",
    Developmental == 1 & `Middle Class` == 0 & `Social Welfare` == 0 ~ "Developmental",
    Developmental == 0 & `Middle Class` == 0 & `Social Welfare` == 0 ~ "Caretaker")) %>%
  mutate(type = factor(type, levels = c(
    "Social Welfare", "Social Welfare / Middle Class", "Social Welfare / Developmental",
    "Social Welfare / Middle Class / Developmental",
    "Middle Class / Developmental", "Middle Class", "Developmental", "Caretaker") %>% rev()))  %>%
  mutate(type = type %>% dplyr::recode(
    "Caretaker" = "Caretaker (C)",
    "Developmental" = "Developmental (D)",
    "Middle Class" = "Middle Class (MC)",
    "Middle Class / Developmental" = "Middle Class Hybrid (MC-D)",
    "Social Welfare / Middle Class / Developmental" = "Hybrid (SW-MC-D)",
    "Social Welfare / Developmental" = "Social Welfare Hybrid (SW-D)",
    "Social Welfare / Middle Class" = "Social Welfare Hybrid (SW-MC)",
    "Social Welfare" = "Social Welfare (SW)")) %>%
  saveRDS("raw_data/datcat.rds")



read_rds("raw_data/indices.rds")  %>%
  select(year, muni_code,  contains("regime")) %>%
  # Cut the 14 city-years where regime scores were not computed due to missing data
  filter(!is.na(regime_dev)) %>%
  pivot_longer(cols = contains("regime"), names_to = "type", values_to = "regime") %>%
  mutate(type = type %>% str_remove("_pop") %>% recode_factor(
    "regime_dev" = "Developmental", 
    "regime_mid" = "Middle Class",
    "regime_soc" = "Social Welfare")) %>%
  group_by(type) %>%
  mutate(class = if_else(regime > 0, 1, 0, missing = NA_real_)) %>%
  ungroup() %>%
  select(-regime) %>%
  pivot_wider(
    id_cols = c(year, muni_code),
    names_from = type,
    values_from = class) %>%
  mutate(type = case_when(
    Developmental == 1 & `Middle Class` == 1 & `Social Welfare` == 1 ~ "Social Welfare / Middle Class / Developmental",
    Developmental == 0 & `Middle Class` == 1 & `Social Welfare` == 1 ~ "Social Welfare / Middle Class",
    Developmental == 1 & `Middle Class` == 0 & `Social Welfare` == 1 ~ "Social Welfare / Developmental",
    Developmental == 0 & `Middle Class` == 0 & `Social Welfare` == 1 ~ "Social Welfare",
    Developmental == 1 & `Middle Class` == 1 & `Social Welfare` == 0 ~ "Middle Class / Developmental",
    Developmental == 0 & `Middle Class` == 1 & `Social Welfare` == 0 ~ "Middle Class",
    Developmental == 1 & `Middle Class` == 0 & `Social Welfare` == 0 ~ "Developmental",
    Developmental == 0 & `Middle Class` == 0 & `Social Welfare` == 0 ~ "Caretaker")) %>%
  mutate(type = factor(type, levels = c(
    "Social Welfare", "Social Welfare / Middle Class", "Social Welfare / Developmental",
    "Social Welfare / Middle Class / Developmental",
    "Middle Class / Developmental", "Middle Class", "Developmental", "Caretaker") %>% rev()))  %>%
  mutate(type = type %>% dplyr::recode(
    "Caretaker" = "Caretaker (C)",
    "Developmental" = "Developmental (D)",
    "Middle Class" = "Middle Class (MC)",
    "Middle Class / Developmental" = "Middle Class Hybrid (MC-D)",
    "Social Welfare / Middle Class / Developmental" = "Hybrid (SW-MC-D)",
    "Social Welfare / Developmental" = "Social Welfare Hybrid (SW-D)",
    "Social Welfare / Middle Class" = "Social Welfare Hybrid (SW-MC)",
    "Social Welfare" = "Social Welfare (SW)")) %>%
  saveRDS("raw_data/datcat2.rds")

