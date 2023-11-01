#' @name 06_table_2.R
#' @author Tim Fraser
#' 

library(dplyr)
library(readr)
library(knitr)
library(kableExtra)
library(tidyr)

## 4.7 Table 2: Regime Class!

tab <- bind_rows(
  read_rds("raw_data/datcat.rds") %>% 
    group_by(type) %>%
    summarize(percent = round(n() / nrow(.)*100, 1),
              threshold = "Median"),
  read_rds("raw_data/datcat2.rds") %>% 
    group_by(type) %>%
    summarize(percent = round(n() / nrow(.)*100, 1),
              threshold = "Mean")) %>%
  pivot_wider(id_cols = c(type), names_from = threshold, values_from = percent) %>%
  mutate(abbr = str_extract(type, "[(].+[)]") %>% str_remove_all("[(]|[)]")) %>%
  left_join(by = "abbr", y = list(
    name = c("Social Welfare", "Middle Class", "Developmental", "Caretaker", 
             "Social Welfare Hybrid (1)", "Social Welfare Hybrid (2)", "Hybrid", "Middle Class Hybrid"),
    abbr = c("SW", "MC", "D", "C",  "SW-MC", "SW-D", "SW-MC-D", "MC-D"),
    soc =  c("High",  "Low",  "Low", "Low",  "High",     "High",    "High",       "Low"),
    mid =  c("Low",  "High",  "Low", "Low",  "High",     "Low",    "High",       "High"),
    dev =  c("Low",  "Low",  "High", "Low",  "Low",     "High",    "High",       "High"),
    order = 1:8) %>% as_tibble()) %>%
  arrange(order) %>%
  select(Name = name, `Code` = abbr, `Social Welfare` = soc, `Middle Class` = mid, `Developmental` = dev, 
         Median, Mean)

viz = function(data){
  data %>%
    kbl(format = "latex", booktabs = TRUE, longtable = TRUE, linesep = "\\addlinespace",escape = F,
        # You can write LaTEX in the caption, as long as you use \\, not \
        caption = "\\textbf{Table \\ref{tab:regimeclass}: \\label{tab:regimeclass}{Regime Classifications}}") %>%
    kable_styling(latex_options = c("hold_position"), full_width = FALSE, font_size = 9) %>%
    # Note: full_width must = FALSE when using background and color in kableExtra latex
    # striped must also not be a latex_option
    add_header_above(c(" " = 2, "Index Scores<sup>1</sup>" = 3, "Percentage of Cases\nby Threshold<sup>2</sup>" = 2)) %>%
    column_spec(
      column = 1, width = "3.9cm", 
      color = c("black", "white", "white", "black", "white", "white", "white", "white"),
      background = c("#ffb000", "#648FFF","#696880", "#ADADC9", "#DC267F", "#ff683b","#5542AD","#001FA2")) %>% 
    column_spec(2, width = "1.75cm") %>%
    column_spec(3, width = "1.5cm", background = if_else(data$`Social Welfare` == "High", "#C5C6D0", "white")) %>%
    column_spec(4, width = "1.5cm", background = if_else(data$`Middle Class` == "High", "#C5C6D0", "white")) %>%
    column_spec(5, width = "1.75cm", background = if_else(data$`Developmental` == "High", "#C5C6D0", "white")) %>%
    column_spec(6, width = "1.25cm") %>%
    column_spec(7, width = "1.25cm") %>%
    pack_rows("\nClassic Regimes", 1, 4, hline_before = FALSE, latex_gap_space = "0.10cm") %>%
    pack_rows("\nHybrid  Regimes", 5, 8, hline_before = TRUE, latex_gap_space = "0.25cm") %>%
    footnote(
      number = c("City-years classified based on three regime scores being above (High) or below (Low) threshold.",
                 "% of cases, when using median as threshold; robustness check uses mean as threshold."),
      threeparttable = TRUE)  %>%
    str_replace_all(c("<sup>" = "$^{",   "</sup>" = "}$")) %>%
    knitr::asis_output()
}

list(viz = viz, tab = tab) %>%
  saveRDS("table/table_regimeclass.rds")

tab %>%
  with(`Median` %>% `names<-`(Code)) %>%
  saveRDS("table/nums.rds")

rm(list = ls())

