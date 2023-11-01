#' @name 06_table_A1.R
#' @author Tim Fraser

## 4.1 Table A1: Descriptives ###################

logit = function(x){log(x / (1 - x)) %>% return()}

read_rds("raw_data/dataset.rds") %>%
  select(-year,-muni_code, -pref) %>%
  pivot_longer(cols = -c(), names_to = "variable", values_to = "value") %>%
  filter(variable != "social_capital") %>%
  filter(variable != "tsunami") %>%
  mutate(term = variable %>% recode_factor(
    "regime_soc" = "Social Welfare Regime",
    "regime_mid" = "Middle Class Regime",
    "regime_dev" = "Developmental Regime",
    "pop" = "Population",
    "inhabitable_area" = "Inhabitable Area (ha)",
    "age_elder" = "% Over Age 65",
    "income" = "Income per capita<sup>1</sup>",
    "rev" = "Revenue per capita<sup>1</sup>",
    "rev_external" = "% National & Prefectural Funding",
    "rev_to_exp" = "Real Term Budget Balance (+/-)",
    "deaths" = "Disaster Deaths<sup>2</sup>", 
    "damages" = "Disaster Damage<sup>2</sup>", 
    "tsunami" = "Hit by 2011 tsunami (1/0)",
    "dis_restoration" = "Disaster Recovery Spending Rate<sup>1</sup>",
    "dis_relief" = "Disaster Relief Spending Rate<sup>1</sup>",
    "LDP_Komeito_house" = "% LDP Coalition Votes: Lower House",
    "LDP_Komeito_pref" = "% LDP Coalition Votes: Prefecture",
    "bonding" = "Bonding Social Capital",
    "bridging" = "Bridging Social Capital",
    "linking" = "Linking Social Capital",
    "college" = "% Some College",
    "unemployed" = "% Unemployed",
    "migration" = "Total Migration per capita"),
    transformation = variable %>% recode_factor(
      "regime_soc" = "x",
      "regime_dev" = "x",
      "regime_mid" = "x",
      "pop" = "x<sup>1/10</sup>",
      "inhabitable_area" = "log(x)",
      "age_elder" = "logit(x)",
      "income" = "log(x)",
      "rev" = "sqrt(x)",
      "rev_external" = "logit(x)",
      "rev_to_exp" = "x",
      "deaths" = "x<sup>1/10</sup>", 
      "damages" = "x<sup>1/10</sup>", 
      "tsunami" = "x",
      "dis_restoration" = "x<sup>1/10</sup>",
      "dis_relief" = "x<sup>1/10</sup>",
      "LDP_Komeito_house" = "logit(x + 0.01)",
      "LDP_Komeito_pref" = "logit(x + 0.01)",
      "bonding" = "logit(x)",
      "bridging" = "quartiles(x)",
      "linking" = "logit(x)",
      "college" = "logit(x)",
      "unemployed" = "logit(x)",
      "migration" = "log(x)")) %>%
  group_by(variable) %>%
  mutate(scaled = scales::rescale(value, to = c(0, 1))) %>%
  group_by(variable) %>%
  mutate(trans = case_when(
    variable == "pop" ~ value^0.1,
    variable == "inhabitable_area" ~ log(value),
    variable == "income" ~ log(value),
    variable == "age_elder" ~ logit(value),
    variable == "rev" ~ sqrt(value),
    variable == "rev_external" ~ logit(value),
    variable == "deaths" ~ value^0.1,
    variable == "damages" ~ value^0.1,
    variable == "dis_restoration" ~ value^0.1,
    variable == "dis_relief" ~ value^0.1,
    variable == "LDP_Komeito_house" ~ logit(value + 0.01),
    variable == "LDP_Komeito_pref" ~ logit(value + 0.01),
    variable == "bonding" ~ logit(value),
    variable == "bridging" ~ ntile(value, 4) %>% as.numeric(),
    variable == "linking" ~ logit(value),
    variable == "college" ~ logit(value),
    variable == "unemployed" ~ logit(value),
    variable == "migration" ~ log(value),
    TRUE ~ value)) %>%
  mutate(trans = scales::rescale(trans, to = c(0, 1))) %>%
  ungroup() %>%
  saveRDS("raw_data/datalist.rds")



# myvalue <- read_rds("raw_data/datalist.rds") %>%
#   with(split(value, term))

.scaled <- read_rds("raw_data/datalist.rds") %>%
  # Truncate the zeros for our disaster variables,
  # to better show the value added of the transformation.
  mutate(identifier = case_when(
    variable %in% c("deaths", "damages", "dis_restoration", "dis_relief") &
      value == 0 ~ 1,   TRUE ~ 0)) %>%
  filter(identifier == 0) %>%
  with(split(scaled, term))

.trans <- read_rds("raw_data/datalist.rds") %>%
  # Truncate the zeros for our disaster variables,
  # to better show the value added of the transformation.
  mutate(identifier = case_when(
    variable %in% c("deaths", "damages", "dis_restoration", "dis_relief") &
      value == 0 ~ 1,   TRUE ~ 0)) %>%
  filter(identifier == 0) %>%
  filter(!is.infinite(trans)) %>%
  with(split(trans, term))

.n <- read_rds("raw_data/dataset.rds") %>% nrow()

data <- read_rds("raw_data/datalist.rds") %>%
  group_by(`Variable` = term,variable) %>%
  summarize(
    Mean = mean(value, na.rm = TRUE) %>% round(2),
    `SD` = sd(value,na.rm = TRUE) %>% round(2),
    Min = min(value, na.rm = TRUE) %>% round(2),
    Median = median(value,na.rm = TRUE) %>% round(2),
    Max = max(value, na.rm = TRUE) %>% round(2),
    N = sum(!is.na(value), na.rm = TRUE),
    `% Missing` = round(sum(is.na(value),na.rm= TRUE) * 100 / n(), 1),
    `Distribution<sup>3</sup>` = "",
    `Transformed<sup>3</sup>` = "",
    Transformation = unique(transformation)) %>%
  ungroup() %>%
  mutate_at(vars(`Mean`:Max), 
            list(~if_else(variable == "pop", 
                          true = round(., 0) %>% as.character(), 
                          false = as.character(.)))) %>%
  select(-variable)


viz = function(data, .n, .scaled, .trans){
  require(knitr)
  require(kableExtra)
  require(dplyr)
  data %>%
    kbl(format = "latex", booktabs = TRUE, longtable = TRUE,
        caption = paste0(
          "\\textbf{Table \\ref{tab:tablea1}: \\label{tab:tablea1}{Descriptive Statistics}} \\newline \\normalsize ",
          paste0(.n, " Japanese city-years (2000-2018)"))) %>%
    kable_styling(latex_options = c("striped", "hold_position"), full_width = FALSE, font_size = 9) %>%
    column_spec(column = 1, width = "4cm") %>%
    # #column_spec(2, bold = TRUE, width = "1.6cm") %>%
    column_spec(column = 9, image = spec_hist(.scaled)) %>%
    column_spec(column = 10, image = spec_hist(.trans)) %>%
    pack_rows("\nUrban Regime Indices", 1, 3, hline_before = TRUE, latex_gap_space = "0.25cm") %>%
    pack_rows("\nDemographics", 4, 7, hline_before = TRUE, latex_gap_space = "0.25cm") %>%
    pack_rows("\nRevenue", 8, 10, hline_before = TRUE, latex_gap_space = "0.25cm") %>%
    pack_rows("\nDisaster Conditions", 11, 12, hline_before = TRUE, latex_gap_space = "0.25cm") %>%
    pack_rows("\nDisaster Spending", 13, 14, hline_before = TRUE, latex_gap_space = "0.25cm") %>%
    pack_rows("\nPolitical Parties", 15, 16, hline_before = TRUE, latex_gap_space = "0.25cm") %>%
    pack_rows("\nCollective Action", 17, 19, hline_before = TRUE, latex_gap_space = "0.25cm") %>%
    pack_rows("\nExtra Controls", 20, 21, hline_before = TRUE, latex_gap_space = "0.25cm") %>%
    add_header_above(header = c(" ", "Statistics" = 5, "Observations" = 2)) %>%
    footnote(
      number = c(
        "Spending measured in 1,000s of yen.",
        "Disaster deaths and damages (# of buildings damaged) measured per 100,000 persons.",
        "Zeros from unaffected cities in disaster variable distributions omitted to show nonzero values clearly."),
      general = c("Categorical Variables include Year<sup>a</sup>, Prefecture<sup>b</sup>, and Tsunami<sup>c</sup>. Models use multiple imputation with 5 imputations to account for missing data."),
      alphabet = c("Years range from 2000 (n = 1428 cities) to 2018 (n = 1727). Mode is 2010 (n = 1732). In models, baseline year is 2000. Some municipalities consolidated over time, while others split. Dataset omits outliers including Fukushima Exclusion Zone cities from 2011-2018 and Yubari 2000-2018.",
                   "47 Prefectures. Modal prefecture is Hokkaido, with 178 cities (10%, n = 1,738) and 3,327 city-years (10%, n = 31,493). In models, the baseline prefecture is Hokkaido.",
                   "Tsunami hit 85 municipalities in 2011; cities labeled 2011-2018 as tsunami-affected (n = 680)."),
      footnote_order = c("general",  "alphabet", "number"),
      threeparttable = TRUE
    ) %>%
    str_replace_all(c("<sup>" = "$^{",   "</sup>" = "}$",
                      "R2" = "R$^{2}$", "<b>" = "\textbf{",  "</b>" = "}",
                      "<sqrt>" = "$sqrt{", "</sqrt>" = "}$")) %>%
    knitr::asis_output()
}

list(viz = viz, data = data, .n = .n, .scaled = .scaled, .trans = .trans) %>%
  saveRDS("table/table_A1.rds")

rm(list = ls())

