#' @name 06_table_3.R
#' @author Tim Fraser

library(dplyr)
library(readr)
library(knitr)
library(kableExtra)
library(tidyr)

## 4.5 GLH Table

### GLH Functions (Generalized Linear Hypothesis Tests)


get_stat = function(x){
  bind_rows(
    get_glh(x, c("year2002 = 0")) %>% mutate(type = "2002"),
    get_glh(x, c("year2003 = 0")) %>% mutate(type = "2003"),
    get_glh(x, c("year2004 = 0")) %>% mutate(type = "2004"),
    get_glh(x, c("year2005 = 0")) %>% mutate(type = "2005"),
    get_glh(x, c("year2006 = 0")) %>% mutate(type = "2006"),
    get_glh(x, c("year2007 = 0")) %>% mutate(type = "2007"),
    get_glh(x, c("year2008 = 0")) %>% mutate(type = "2008"),
    get_glh(x, c("year2009 = 0")) %>% mutate(type = "2009"),
    get_glh(x, c("year2010 = 0")) %>% mutate(type = "2010"),
    # Average of pre-disaster period
    get_glh(x, c("(year2010 + year2009 + year2008 + year2007 + year2005 + year2004 + year2003 + year2002) / 8 = 0")) %>% 
      mutate(type = "Mean(2002:2010)"),
    
    get_glh(x, c("year2011 = 0"))%>% mutate(type = "2011"),
    get_glh(x, c("year2012 = 0"))%>% mutate(type = "2012"),
    get_glh(x, c("year2013 = 0"))%>% mutate(type = "2013"),
    get_glh(x, c("year2014 = 0"))%>% mutate(type = "2014"),
    get_glh(x, c("year2015 = 0"))%>% mutate(type = "2015"),
    get_glh(x, c("year2016 = 0"))%>% mutate(type = "2016"),
    get_glh(x, c("year2017 = 0"))%>% mutate(type = "2017"),
    get_glh(x, c("year2018 = 0"))%>% mutate(type = "2018"),
    # Average of Post Disaster Period
    get_glh(x, c("(year2018 + year2017 + year2016 + year2015 + year2014 + year2013 + year2012 + year2011) / 8 = 0")) %>%
      mutate(type = "Mean(2011:2018)"),
    
    # Comparison
    get_glh(x, "(year2018 + year2017 + year2016 + year2015 + year2014 + year2013 + year2012 + year2011) / 8 - (year2010 + year2009 + year2008 + year2007 + year2005 + year2004 + year2003 + year2002) / 8 = 0") %>%
      mutate(type = "Mean Post - Mean Pre"),
    # Total Gains
    get_glh(x, "year2018 - year2002 = 0") %>%
      mutate(type = "2018 - 2002"),
    # Gains since Fukushima
    get_glh(x, "year2018 - year2017 = 0") %>%
      mutate(type = "2018 - 2017"),
    get_glh(x, "year2018 - year2016 = 0") %>%
      mutate(type = "2018 - 2016"),
    get_glh(x, "year2018 - year2015 = 0") %>%
      mutate(type = "2018 - 2015"),
    get_glh(x, "year2018 - year2014 = 0") %>%
      mutate(type = "2018 - 2014"),
    get_glh(x, "year2018 - year2013 = 0") %>%
      mutate(type = "2018 - 2013"),
    get_glh(x, "year2018 - year2012 = 0") %>%
      mutate(type = "2018 - 2012"),
    get_glh(x, "year2018 - year2011 = 0") %>%
      mutate(type = "2018 - 2011"),
    get_glh(x, "year2018 - year2010 = 0") %>%
      mutate(type = "2018 - 2010")
  ) %>%   
    mutate(
      sign = if_else(estimate > 0, "+", ""),
      lower = estimate - std_error*1.96,
      upper = estimate + std_error*1.96,
      stars = gtools::stars.pval(p_value),
      p_value = round(p_value, 3),
      label = paste(sign, round(estimate, 2), stars, " (", round(std_error, 2), ")", sep = "")) %>%
    select(type, label, p_value, estimate, lower, upper) %>%
    return()
}

m9 <- read_rds("model/model_soc.RData")
load("table/table_functions.RData")
soc <- get_stat(m9)

m9 <- read_rds("model/model_dev.RData")
load("table/table_functions.RData")
dev <- get_stat(m9)

m9 <- read_rds("model/model_mid.RData")
load("table/table_functions.RData")
mid <- get_stat(m9)

bind_rows(
  soc %>% mutate(group = "Social Welfare"),
  mid %>% mutate(group = "Middle Class"),
  dev %>% mutate(group = "Developmental")
) %>%
  saveRDS("raw_data/effects.rds")

rm(list = ls())
#Frank Bretz, Torsten Hothorn and Peter Westfall (2010), Multiple Comparisons Using R, CRC Press, Boca Raton.
#Torsten Hothorn, Frank Bretz and Peter Westfall (2008), Simultaneous Inference in General Parametric Models. Biometrical Journal, 50(3), 346--363; See vignette("generalsiminf", package = "multcomp").

### GLH Table


data <- read_rds("raw_data/effects.rds") %>%
  mutate(type = case_when(
    type == "Mean Post - Mean Pre" ~ "Post - Pre",
    type == "Mean(2002:2010)" ~ "Mean Pre",
    type == "Mean(2011:2018)" ~ "Mean Post", 
    TRUE ~ type)) %>%
  pivot_wider(id_cols = c(type), names_from = group, values_from = c(label, estimate, lower, upper)) %>%
  magrittr::set_colnames(value = names(.) %>% str_replace_all(" ", "_")) %>%
  mutate(se_Social_Welfare = str_extract(label_Social_Welfare, "[(].+[)]") %>% str_remove_all("[(]|[)]"),
         se_Developmental = str_extract(label_Developmental, "[(].+[)]") %>% str_remove_all("[(]|[)]"),
         se_Middle_Class = str_extract(label_Middle_Class, "[(].+[)]") %>% str_remove_all("[(]|[)]")) %>%
  mutate(label_Social_Welfare = str_remove(label_Social_Welfare, " [(].+[)]"),
         label_Developmental = str_remove(label_Developmental, " [(].+[)]"),
         label_Middle_Class = str_remove(label_Middle_Class, " [(].+[)]")) %>%
  mutate(`95% CI` = "", `95% CI ` = "",`95% CI  ` = "") %>%
  slice(1:21) 


viz = function(data){
  data %>%
    select(`Year` = type, 
           `Estimate` = label_Social_Welfare, `SE` = se_Social_Welfare, `95% CI`,
           `Estimate ` = `label_Developmental`, `SE ` = se_Developmental, `95% CI `,
           `Estimate  ` = label_Middle_Class, `SE  ` = se_Middle_Class, `95% CI  `) %>%
    kbl(format = "latex", booktabs = TRUE, longtable = TRUE, linesep = "\\addlinespace",
        # You can write LaTEX in the caption, as long as you use \\, not \
        caption = paste("\\textbf{Table \\ref{tab:linearhyp}", ": Linear Hypothesis Tests of Temporal Effects}", sep = "")) %>%
    kable_styling(latex_options = c("striped", "hold_position"), full_width = TRUE, font_size = 8) %>%
    column_spec(column = 1, width = "2.25cm") %>%
    # Social Welfare
    column_spec(column = 2, width = "1.35cm") %>%
    column_spec(column = 3, width = "0.5cm") %>%
    column_spec(column = 4,
                image = spec_pointrange(
                  x = data$estimate_Social_Welfare, xmin = data$lower_Social_Welfare, xmax = data$upper_Social_Welfare, vline = 0)) %>%
    # Developmental
    column_spec(column = 5, width = "1.35cm") %>%
    column_spec(column = 6, width = "0.5cm") %>%
    column_spec(column = 7, 
                image = spec_pointrange(
                  x = data$estimate_Developmental, xmin = data$lower_Developmental, xmax = data$upper_Developmental, vline = 0)) %>%
    # Middle Class
    column_spec(column = 8, width = "1.35cm") %>%
    column_spec(column = 9, width = "0.5cm") %>%
    column_spec(column = 10, 
                image = spec_pointrange(
                  x = data$estimate_Middle_Class, xmin = data$lower_Middle_Class, xmax = data$upper_Middle_Class, vline = 0)) %>%
    pack_rows("\nPre-Disaster", 1, 10) %>% 
    row_spec(row = 10, bold = TRUE, background = "#D3D3D3") %>%
    pack_rows("\nPost-Disaster", 11, 19, hline_before = TRUE, latex_gap_space = "0.25cm") %>%
    row_spec(row = 19, bold = TRUE, background = "#D3D3D3") %>%
    pack_rows("\nAverage Treatment Effect", 20, 20, hline_before = TRUE, latex_gap_space = "0.25cm") %>%
    row_spec(row = 20, bold = TRUE, background = "#D3D3D3") %>%
    pack_rows("\nNet Gain over Time", 21, 21, hline_before = TRUE, latex_gap_space = "0.25cm") %>%
    row_spec(row = 21, bold = TRUE, background = "#D3D3D3") %>%
    add_header_above(c(" ", "Social Welfare\nRegime" = 3, "Developmental\nRegime" = 3, "Middle Class\nRegime" = 3)) %>%
    footnote(
      general = c("Statistical Significance: *** p < 0.001, ** p < 0.01, * p < 0.05, . p < 0.10. All p-values and asterisks reflect two-tailed hypothesis tests. Based on annual fixed effects from Models 8, 16, & 24 in Tables A2, A3, & A4, respectively."),
      threeparttable = TRUE) %>%
    str_replace_all(c("<sup>" = "$^{",   "</sup>" = "}$",
                      "R2" = "R$^{2}$", "<b>" = "\textbf{",  "</b>" = "}",
                      "<sqrt>" = "$sqrt{", "</sqrt>" = "}$")) %>%
    knitr::asis_output()  %>%
    return()
}

list(viz = viz, tab = data) %>%
  saveRDS("table/table_glh.rds")
rm(list = ls())

