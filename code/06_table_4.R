#' @name 06_table_4.R
#' @author Tim Fraser

library(dplyr)
library(readr)
library(knitr)
library(kableExtra)
library(tidyr)

## 4.6 Table Key Cities

dat <- read_rds("raw_data/dataset.rds") %>%
  left_join(by = "muni_code", y = read_rds("raw_data/muni_code.rds")) %>%
  select(year, muni_code, muni, contains("regime")) %>%
  pivot_longer(cols = c(contains("regime")), names_to = "regime", values_to = "value") %>%
  left_join(by = c("year", "muni_code"), y = read_rds("raw_data/datcat.rds") %>%
              select(year, muni_code, type) %>% mutate(year = factor(year))) %>%
  mutate(percentile = ntile(value, 100)) %>%
  select(year, muni_code, muni, regime, value, percentile, type)

mysum <- bind_rows(
  # Kyoto - Social Welfare Hybrid (SW-MC)
  # dat %>%
  #   filter(str_detect(muni, "Kyoto-shi")) %>%
  #   filter(year %in% c(2000, 2018)),
  # 
  # Kobe - Middle Class Hybrid (MC-D) --> Social Welfare Hybrid (SW-MC)
  dat %>%
    filter(str_detect(muni, "Kobe-shi")) %>%
    filter(year %in% c(2000, 2018)),
  
  # Fukuoka - Middle Class Hybrid (MC-D) --> Social Welfare Hybrid (SW-MC)
  # dat %>%
  #   filter(str_detect(muni, "Fukuoka-shi")) %>%
  #   filter(year %in% c(2000, 2018)),
  
  # Sendai - Middle Class
  dat %>%
    filter(str_detect(muni, "Sendai-shi")) %>%
    filter(year %in% c(2000, 2018)),
  
  # Nagoya - Middle Class --> Social Welfare Hybrid (SW-MC)
  # dat %>%
  #   filter(str_detect(muni, "Nagoya-shi")) %>%
  #   filter(year %in% c(2000, 2018)),
  
  # Iida - Developmental --> Social Welfare Hybrid (SW-D)
  # dat %>%
  #   filter(str_detect(muni, "Iida-shi")) %>%
  #   filter(year %in% c(2000, 2018)),
  
  # Tomakomai - Social Welfare (SW-MC) --> Social Welfare (SW)
  # dat %>%
  #   filter(str_detect(muni, "Tomakomai-shi")) %>%
  #   filter(year %in% c(2000, 2018)),
  
  # Osaka - Hybrid (SW-MC-D) --> Social Welfare Hybrid (SW-C)
  # dat %>%
  #   filter(str_detect(muni, "Osaka-shi")) %>%
  #   filter(year %in% c(2000, 2018)),
  
  # Mitaka - Caretaker (WTF)
  dat %>%
    filter(str_detect(muni, "Mitaka-shi")) %>%
    filter(year %in% c(2000, 2018)),
  
  
  # Shinjuku - Middle Class (MC) --> Caretaker
  # dat %>%
  #   filter(str_detect(muni, "Shinjuku-ku")) %>%
  #   filter(year %in% c(2000, 2018)),
  
  
  # Kagoshima - Caretaker (C) --> Social Welfare (SW)
  # dat %>%
  #   filter(str_detect(muni, "Kagoshima-shi")) %>%
  #   filter(year %in% c(2000, 2018)),
  
  
  # Fukushima - Developmental (D) --> Caretaker (C)
  dat %>%
    filter(str_detect(muni, "Fukushima-shi")) %>%
    filter(year %in% c(2000, 2018)),
  
  # Kitakyushu --> Hybrid (SW-MC-D) --> Social Welfare Hybrid (SW-MC)
  # dat %>%
  #   filter(str_detect(muni, "Kitakyushu-shi")) %>%
  #   filter(year %in% c(2000, 2018)),
  
  # Muroran --> Social Welfare (SW) --> Social Welfare Hybrid (SW-MC)
  dat %>%
    filter(str_detect(muni, "Muroran-shi")) %>%
    filter(year %in% c(2000, 2018)),
  
  # Yokohama - Middle Class --> Social Welfare Hybrid (SW-MC)
  # dat %>%
  #   filter(str_detect(muni, "Yokohama-shi")) %>%
  #   filter(year %in% c(2000, 2018)),
  
  # Sapporo - Middle Class (MC) --> Social Welfare Hybrid (SW-MC)
  # dat %>%
  #   filter(str_detect(muni, "Sapporo-shi")) %>%
  #   filter(year %in% c(2000, 2018)),
  
  # Ishinomaki - Middle Class (MC) --> Hybrid (SW-MC-D)
  dat %>%
    filter(str_detect(muni, "Ishinomaki-shi")) %>%
    filter(year %in% c(2000, 2018)),
  
  # Minato-ku - Middle Class -> Social Welfare Hybrid (SW-MC)
  # dat %>%
  #   filter(str_detect(muni, "Minato-ku")) %>%
  #   filter(year %in% c(2000, 2018)),
  
  # Mushashino - Middle Class (MC) - Social Welfare Hybrid (SW-MC)
  # dat %>%
  #   filter(str_detect(muni, "Musashino-shi")) %>%
  #   filter(year %in% c(2000, 2018)),
  
  # Hiroshima Middle Class --> Middle Class Hybrid
  # dat %>%
  #   filter(str_detect(muni, "Hiroshima-shi")) %>%
  #   filter(year %in% c(2000, 2018)),
  
  # Genkai - Developmental (D) --> Hybrid (SW-MC-D)
  # dat %>%
  #   filter(str_detect(muni, "Genkai-cho")) %>%
  #   filter(year %in% c(2000, 2018)),
  
  # Fukui - Developmental (D) --> Social Welfare (SW)
  # dat %>%
  #   filter(str_detect(muni, "Fukui-shi")) %>%
  #   filter(year %in% c(2000, 2018))
)


tab <- mysum %>% 
  select(year, muni, regime, percentile, type) %>%
  mutate(regime = regime %>% str_remove("regime_")) %>%
  pivot_wider(id_cols = c(muni), names_from = c(regime, year), values_from = percentile) %>%
  left_join(by = c("muni"),
            y = mysum %>% select(year, muni, type) %>% distinct() %>%
              pivot_wider(id_cols = c(muni), names_from = c(year),
                          names_prefix = "type_", values_from = type)) %>%
  separate(col = muni, into = c("pref", "muni"), sep = " ") %>%
  mutate(pref = str_remove(pref, "-fu|-ken|-to"),
         muni = str_remove(muni, "-shi|-cho|-mura|-machi|-ku")) %>%
  mutate_at(vars(type_2018, type_2000), list(~str_extract(., "[(].+[)]") %>% str_remove_all("[(]|[)]"))) %>%
  mutate(Literature = muni %>% recode_factor(
    "Kobe" = "Nunokawa 2007; Funck 2007; Edgington 2010", 
    .default = "")) %>%
  arrange(type_2000) %>%
  select(Prefecture = pref, 
         Municipality = muni,
         Literature,
         `Type` = type_2000, `SW` = soc_2000, `MC` = mid_2000, `D` = dev_2000, 
         `Type ` = type_2018, `SW ` = soc_2018, `MC ` = mid_2018, `D ` = dev_2018) %>%
  saveRDS("raw_data/table_cases.rds")

rm(list= ls())

require(knitr)
require(dplyr)
require(readr)
require(kableExtra)
require(purrr)

tab <- read_rds("raw_data/table_cases.rds") %>%
  mutate(Literature = Municipality %>% recode_factor(
    "Mitaka" = "Steiner 1957; Fukushima & Yamaguchi 1997; Ohashi & Phelps 2021",
    "Fukushima" = "Otsuki et al. 2016; Abeysinghe et al. 2022",
    "Ishinomaki" = "Dimmer & Lindenberg 2014; Matthews 2017; Ji & Imai 2022",
    "Kobe" = "Nunokawa 2007; Funck 2007; Yasui 2007; Edgington 2010; Aldrich 2012; Maly et al. 2012", 
    "Sendai" = "Morris 2012; Tsuji 2017",
    "Muroran" = "Edgington 2013", 
    .default = "")) %>%
  filter(Municipality %in% c("Mitaka", "Fukushima", "Ishinomaki", "Kobe", "Sendai", "Muroran"))

viz = function(data){
  data %>%
    kbl(format = "latex", booktabs = TRUE, longtable = TRUE, linesep = "\\addlinespace",
        # You can write LaTEX in the caption, as long as you use \\, not \
        caption = "Table \\ref{tab:keycities}: \\label{tab:keycities}{Regime Change in Past Case Studies}") %>%
    kable_styling(latex_options = c("striped", "hold_position"), full_width = TRUE, font_size = 9) %>%
    add_header_above(header = c(" " = 3, "Regime in 2000" = 4, "Regime in 2018" = 4)) %>%
    column_spec(column = 1, width = "1.5cm") %>%
    column_spec(column = 2, width = "1.5cm") %>%
    column_spec(column = 3, width = "2.5cm") %>%
    # 2020
    column_spec(column = 4, width = "1.7cm") %>%
    column_spec(column = 5, width = "0.3cm") %>%
    column_spec(column = 6, width = "0.3cm") %>%
    column_spec(column = 7, width = "0.3cm") %>%
    # 2018
    column_spec(column = 8, width = "1.7cm") %>%
    column_spec(column = 9, width = "0.3cm") %>%
    column_spec(column = 10, width = "0.3cm") %>%
    column_spec(column = 11, width = "0.3cm") %>%
    footnote(number = c("SW = Social Welfare. MC = Middle Class. D = Developental. C = Caretaker.",
                        "Numbers rank city-year as a percentile (0-100) compared to all other city-years."),
             threeparttable = TRUE) %>%
    #str_replace_all(c("<sup>" = "$^{",   "</sup>" = "}$", "R2" = "R$^{2}$", 
    #                  "<b>" = "\textbf{", "</b>" = "}", "<sqrt>" = "$sqrt{", "</sqrt>" = "}$")) %>%
    knitr::asis_output() %>%
    return()
}

list(viz = viz, tab = tab) %>% 
  saveRDS("table/table_cities.rds")
rm(list = ls())

