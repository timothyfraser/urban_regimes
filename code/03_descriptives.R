#' @name 03_descriptives.R
#' @author Tim Fraser
#' 

# 0. Packages #################################
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

## Correlations

# - Goal 1: Show that these indices have distinct profiles.
# - Goal 2: Show that these indices actually correlate with their components.

# Let's create a "viz" folder for holding visualizations
dir.create("viz")

# 1. Correlations ##############################################

## 1.1 Load Data ###########
dat <- read_rds("raw_data/indices.rds")  %>% 
  select(year, muni_code, 
         regime_dev, agr, com_manuf, construction, roads_bridges,
         regime_mid, education, edu_social, health, planning, waste,
         regime_soc, welfare,welfare_aged, welfare_kids, unemployment, emergency, housing)

## 1.2 Correlations #######
dat %>%
  # Log transform the spending rates
  #mutate_at(vars(agr, com_manuf, construction, roads_bridges,
  #               education, edu_social, health, planning, waste, 
  #               welfare,welfare_aged, welfare_kids, unemployment, emergency, housing), 
  #          list(~log(. + 1))) %>%
  pivot_longer(cols = c(contains("regime")), names_to = "regime", values_to = "y") %>%
  pivot_longer(cols = -c(year, muni_code, regime, y), names_to = "covariate", values_to = "x") %>%
  mutate(zoom = case_when(
    regime == "regime_dev" & covariate %in% c("agr", "com_manuf", "construction", "roads_bridges") ~ 1,
    regime == "regime_mid" & covariate %in% c("education", "edu_social", "health", "planning", "waste") ~ 1,
    regime == "regime_soc" & covariate %in% c("welfare", "welfare_aged", "welfare_kids",  "unemployment", "emergency", "housing") ~ 1,
    TRUE ~ 0)) %>%
  filter(zoom == 1) %>%
  group_by(regime, covariate) %>%
  summarize(r = cor(x, y, use = "pairwise.complete.obs"),
            label = round(r, 2)) %>%
  saveRDS("raw_data/correlations.rds")

read_rds("raw_data/correlations.rds") %>%
  ggplot(mapping = aes(x = covariate, y = r)) +
  facet_wrap(~regime, scales = "free_x") +
  geom_col() +
  geom_text(mapping = aes(label = round(r, 2)), vjust = 0)


## 1.3 Function to get many correlations ########################
get_cor = function(data, vars){
  
  x <- data %>%
    select(any_of(vars)) %>%
    cor(use = "pairwise.complete.obs")
  
  x[upper.tri(x, diag = FALSE)] <- NA
  
  x %>%
    as_tibble(rownames = "from") %>%
    pivot_longer(cols = -c(from), names_to = "to", values_to = "r") %>%
    #filter(from != to) %>%
    filter(!is.na(r)) %>%
    return()
}

## 1.4 Many Correlations #########################
bind_rows(
  get_cor(dat, vars = c("agr", "com_manuf", "construction", "roads_bridges")),
  get_cor(dat, vars = c("education", "edu_social", "health", "planning", "waste")),
  get_cor(dat, vars = c("welfare", "welfare_aged", "welfare_kids",  "unemployment", "emergency", "housing")), 
  .id = "regime") %>%
  mutate_at(vars(from,to), list(~factor(., levels = unique(.) %>% sort())) ) %>%
  ggplot(mapping = aes(x = from, y = to, fill = r, label = round(r, 2))) +
  geom_tile() +
  geom_text() +
  facet_wrap(~regime, scales = "free") +
  scale_fill_gradient2(low = "#DC267F", mid = "white", high = "#648FFF", limits = c(-1, 1))



# 2. Cronbach's Alpha #########################
dat %>% 
  select(regime_dev:roads_bridges) %>%
  ltm::cronbach.alpha(na.rm = TRUE)
# 0.621

dat %>% 
  select(agr:roads_bridges) %>%
  mutate_all(list(~log(. + 1))) %>%
  mutate_all(list(~ntile(., 100))) %>%
  ltm::cronbach.alpha(na.rm = TRUE)

dat %>% 
  select(education:waste) %>%
  mutate_all(list(~log(. + 1))) %>%
  mutate_all(list(~ntile(., 100))) %>%
  ltm::cronbach.alpha(na.rm = TRUE)
# A little lower, but indeed 

dat %>% 
  select(welfare:housing) %>%
  mutate_all(list(~log(. + 1))) %>%
  mutate_all(list(~ntile(., 100))) %>%
  ltm::cronbach.alpha(na.rm = TRUE)
# Further, we broke the data into 100 equally sized percentiles, and calculated cronbach's alpha to test how reliably indicators scale together.

# To be expected, the middle class regime has high variability, but others are excellent.






# 5. Multiple Regimes #########################

## 5.1 Bars ########################

tally <- read_rds("raw_data/datcat.rds") %>%
  group_by(type, year) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(percent = count / sum(count))

g3 <- tally %>% 
  ggplot(mapping = aes(x = year, y = percent, fill = type)) +
  geom_col(position = "fill", color = "white", size = 0.5) +
  labs(y = "% of Cities by Urban Regime", x = "Year", fill = "Regime Score") +
  theme_classic(base_size = 14) +
  theme(panel.border = element_rect(fill = NA, color = "black"),
        panel.spacing = unit(0.5, "cm"),
        legend.position = "right") +
  scale_fill_manual(values = c(
    "#696880",  #"#D2F3D1", # Caretaker
    "#ADADC9", #"#98D4F1", # Developmental
    "#648FFF", # Middle Class
    "#001FA2", # MC-D
    "#5542AD", # SW-MC-D
    "#DC267F", # SW-MC
    "#ff683b", # SW-D
    "#ffb000" # Social Welfare
  )) +
  scale_y_continuous(breaks = c(0, .25, .5, .75, 1), 
                     labels = c(0, 25, 50, 75, 100))

ggsave(g3, filename = "viz/percent_bar.png", width = 7, height = 4, dpi = 500)


## 5.3 Areas V2 ##################

tally <- read_rds("raw_data/datcat2.rds") %>%
  group_by(type, year) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(percent = count / sum(count))

g3 <- tally %>% 
  ggplot(mapping = aes(x = year, y = percent, fill = type)) +
  geom_col(position = "fill", color = "white", size = 0.5) +
  labs(y = "% of Cities by Urban Regime", x = "Year", fill = "Regime Score") +
  theme_classic(base_size = 14) +
  theme(panel.border = element_rect(fill = NA, color = "black"),
        panel.spacing = unit(0.5, "cm"),
        legend.position = "right") +
  scale_fill_manual(values = c(
    "#696880",  #"#D2F3D1", # Caretaker
    "#ADADC9", #"#98D4F1", # Developmental
    "#648FFF", # Middle Class
    "#001FA2", # MC-D
    "#5542AD", # SW-MC-D
    "#DC267F", # SW-MC
    "#ff683b", # SW-D
    "#ffb000" # Social Welfare
  )) +
  scale_y_continuous(breaks = c(0, .25, .5, .75, 1), 
                     labels = c(0, 25, 50, 75, 100))

mylabels <- tally %>% 
  group_by(year) %>%
  arrange(desc(type)) %>%
  mutate(percent = cumsum(percent)) %>%
  ungroup() %>%
  
  filter(str_detect(type, "SW")) %>%
  filter(year %in% seq(from = 2000, to = 2018, by = 2)) %>%
  mutate(label = round(percent, 2)*100)

g4 <- tally %>% 
  ggplot(mapping = aes(x = year, y = percent, fill = type)) +
  geom_area(color = "darkgrey", size = 0.1) +
  geom_area(data = . %>% filter(str_detect(type, "SW")), color = "white", size = 0.75) +
  labs(y = "% of Cities by Urban Regime", x = "Year", fill = "Urban Regime") +
  theme_classic(base_size = 14) +
  theme(panel.border = element_rect(fill = NA, color = "#373737"),
        axis.line = element_blank(),
        axis.ticks = element_line(color = "#373737"),
        panel.spacing = unit(0.5, "cm"),
        legend.position = "right") +
  scale_fill_manual(values = c(
    "#696880", "#ADADC9", "#648FFF", "#001FA2", 
    "#5542AD", "#DC267F", "#ff683b", "#ffb000"), 
    guide = guide_legend(override.aes = list(color = NA))) +
  scale_color_manual(
    breaks = c("Social Welfare (SW)", "Social Welfare Hybrid (SW-MC)",
               "Social Welfare Hybrid (SW-D)", "Hybrid (SW-MC-D)"),
    values = c("#4B3A96", "#A0195B", "#AD4526", "#BD8900") %>% rev(), 
    guide = "none") +
  scale_y_continuous(breaks = c(0, .25, .5, .75, 1), 
                     labels = c(0, 25, 50, 75, 100), limits = c(0,1), expand = expansion(0)) +
  scale_x_continuous(limits = c(2000, 2018), expand = expansion(0))  +
  shadowtext::geom_shadowtext(data = mylabels %>% filter(year != 2000),
                              mapping = aes(x = year, y = percent, color = type, label = label),
                              bg.r = 0.15, bg.color = "white", size = 3.25, vjust = 0, hjust = 1) +
  shadowtext::geom_shadowtext(data = mylabels %>% filter(year == 2000) %>%
                                filter(type != "Social Welfare Hybrid (SW-MC)"),
                              mapping = aes(x = year, y = percent, color = type, label = label),
                              bg.r = 0.15, bg.color = "white", size = 3.25, vjust = 0, hjust = 0)

ggsave(g4, filename = "viz/percent_area2.png", width = 7, height = 4, dpi = 500)

rm(list = ls())





## Regions #####################


dat <- read_rds("raw_data/datcat.rds") %>%
  mutate(region = case_when(
    str_sub(muni_code, 1,2) %in% str_pad(1, 2,"left","0") ~ "Hokkaido",
    str_sub(muni_code, 1,2) %in% str_pad(2:7, 2,"left","0") ~ "Tohoku",
    str_sub(muni_code, 1,2) %in% str_pad(8:14, 2,"left","0") ~ "Kanto",
    str_sub(muni_code, 1,2) %in% str_pad(15:23, 2,"left","0") ~ "Chubu",
    str_sub(muni_code, 1,2) %in% str_pad(24:30, 2,"left","0") ~ "Kansai",
    str_sub(muni_code, 1,2) %in% str_pad(31:35, 2,"left","0") ~ "Chugoku",
    str_sub(muni_code, 1,2) %in% str_pad(36:39, 2,"left","0") ~ "Shikoku",
    str_sub(muni_code, 1,2) %in% str_pad(40:47, 2,"left","0") ~ "Kyushu")) %>%
  mutate(abbr = str_extract(type, "[(].+[)]") %>% str_remove_all("[(]|[)]")) %>%
  left_join(by = "muni_code", y = read_rds("raw_data/muni_code.rds")) %>%
  mutate(pref = str_extract(muni, ".* ") %>% str_remove("-ken|-fu|-to"))


dat %>% 
  group_by(year, region) %>%
  summarize(
    region_total = n(),
    sw3 = sum(str_detect(abbr, "SW")),
    sw2 = sum(str_detect(abbr, "SW") & str_detect(abbr, "SW-MC-D", negate = TRUE)),
    sw1 = sum(abbr == "SW")) %>%
  mutate_at(vars(contains("sw")), list(~round(./region_total*100, 1))) %>%
  ungroup() %>% arrange(desc(sw3)) %>%
  saveRDS("raw_data/map_regions.rds")


mypref <- bind_rows(
  dat %>% filter(year == 2000) %>%
    group_by(year, pref) %>%
    summarize(sw3 = (sum(str_detect(abbr, "SW")) / nrow(.) ),
              sw2 = (sum(str_detect(abbr, "SW") & str_detect(abbr, "SW-MC-D", negate = TRUE)) / nrow(.) ),
              sw1 = (sum(abbr == "SW") / nrow(.) )) %>%
    mutate_at(vars(contains("sw")), list(~round(.*100, 1))) %>%
    ungroup() %>% arrange(desc(sw3)),
  
  dat %>% filter(year == 2010) %>%
    group_by(year, pref) %>%
    summarize(sw3 = (sum(str_detect(abbr, "SW")) / nrow(.) ),
              sw2 = (sum(str_detect(abbr, "SW") & str_detect(abbr, "SW-MC-D", negate = TRUE)) / nrow(.) ),
              sw1 = (sum(abbr == "SW") / nrow(.) )) %>%
    mutate_at(vars(contains("sw")), list(~round(.*100, 1))) %>%
    ungroup() %>% arrange(desc(sw3)),
  
  dat %>% filter(year == 2018) %>%
    group_by(year, pref) %>%
    summarize(sw3 = (sum(str_detect(abbr, "SW")) / nrow(.) ),
              sw2 = (sum(str_detect(abbr, "SW") & str_detect(abbr, "SW-MC-D", negate = TRUE)) / nrow(.) ),
              sw1 = (sum(abbr == "SW") / nrow(.) )) %>%
    mutate_at(vars(contains("sw")), list(~round(.*100, 1))) %>%
    ungroup() %>% arrange(desc(sw3))
)
remove(mypref)


# 7. Sample Selection ##################################

# I ran a series of models, shown in later sections. 
# Let's jump into the fully specified model,
# to identify any problem cases before we get in too deep. 
# Problem cases means city-years that aren't very comparable 
# to the rest of the country. This means, they have one or two 
# attributes that are super funky, giving them tremendous leverage 
# over our predictions. We generally want to keep those kinds of places 
# out of the regression model, so that our predictions actually 
# fit the rest of the 30,000+ city-years we are modeling.

# Packages 
library(tidyverse)
library(broom)
library(lmtest)

dat <- read_rds("raw_data/indices.rds")  %>%
  mutate(LDP_Komeito_house = voteshare_LDP_house + voteshare_Komeito_house) %>%
  select(year, muni_code, muni, pref, contains("regime"),
         pop, inhabitable_area, rev, income, age_elder, contains("dis_"),
         contains("rev"),total_spending, damages, deaths, tsunami,
         migration, college, bonding, bridging, linking, 
         #LDP_Komeito, voter_turnout_pref, 
         voter_turnout_house, LDP_Komeito_house) %>%
  # A few cases are missing. Omit them so you can do likelihood ratio tests
  na.omit()

# First, let's run the full specified model!
m0 <- dat %>%
  lm(formula = regime_soc ~ factor(year) +
       log(pop + 1) + log(inhabitable_area) + log(rev) + log(income) + 
       log(age_elder / (1 - age_elder)) + log(rev_external / (1 - rev_external)) + rev_to_exp +
       log(deaths + 1) + log(damages + 1) + tsunami +
       log(dis_restoration + 1) + log(dis_relief + 1) + pref +
       regime_dev + regime_mid) %>% 
  fortify()

# Wowser! Something's happening there!
m0 %>%
  ggplot(mapping = aes(x = .cooksd, y = .stdresid)) +
  geom_jitter()

# These are the high leverage sites
m0 %>%
  tibble::rownames_to_column(var = "id") %>%
  filter(id %in% myoutliers)


# [Yubari, Hokkaido](https://ja.wikipedia.org/wiki/%E5%A4%95%E5%BC%B5%E5%B8%82) (```muni_code = 01209```) has SUPER wacky revenue to expenditure rates, probably because it is known for experiencing intense depopulation and aging. But most have a rev_to_exp rate of 0 or above; only some have negative values. These three city-years have a rev_to_exp ratio of -700 or so, while no other city has one less than -40, and most are above 0. Let's cut Yubari's city-years as outliers; they really are weird and shouldn't be used as a source of comparison. Cutting them should drop our cook's distance down below 0.03.

# Wowser! Something's happening there!
m0 %>%
  ggplot(mapping = aes(x = rev_to_exp, y = .cooksd)) +
  geom_jitter() +
  geom_line()


# Linking vs. LDP/Komeito Votes 

test <- read_rds("/cloud/project/raw_data/indices.rds")  %>%
  mutate(LDP_Komeito_house = voteshare_LDP_house + voteshare_Komeito_house,
         LDP_Komeito_pref = voteshare_LDP_pref + voteshare_Komeito_pref) %>%
  select(muni_code, year,bonding, bridging, linking, LDP_Komeito_house, LDP_Komeito_pref, 
         voteshare_winner_pref, voteshare_winner_house) %>%
  filter(muni_code != "01209")  


test %>% 
  select(bonding, bridging, linking, LDP_Komeito_house, LDP_Komeito_pref) %>%
  cor(use = "pairwise.complete.obs") %>% round(2)


test %>% 
  select(LDP_Komeito_house, LDP_Komeito_pref) %>%
  cor(use = "pairwise.complete.obs") %>% round(2)

# Not mission critical; since we don't use both directly in the model
test %>% 
  select(LDP_Komeito_house:voteshare_winner_house) %>%
  cor(use = "pairwise.complete.obs") %>% round(2)



