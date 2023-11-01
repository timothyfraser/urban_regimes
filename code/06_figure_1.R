#' @name 06_figure_1.R
#' @author Tim Fraser

library(dplyr)
library(ggplot2)
library(readr)

# dat <- read_rds("raw_data/indices.rds")  %>%
#   select(year, muni_code, contains("regime")) %>%
#   pivot_longer(cols = contains("regime"), 
#                names_to = "type", values_to = "regime") %>%
#   mutate(type = type  %>% recode_factor(
#     "regime_soc" = "Social Welfare",
#     "regime_mid" = "Middle Class",
#     "regime_dev" = "Developmental"))

dat = read_rds("raw_data/indices.rds") %>%
  select(
    year, muni_code,
    regime_dev, regime_mid, regime_soc,
    agr, com_manuf, construction, roads_bridges,
    education, edu_social, health, planning, waste,
    welfare, welfare_aged, welfare_kids, unemployment, emergency, housing
  ) %>%
  # Log transform all spending indicators, to account for strong right skew of rates;
  # use a constant to ensure zeros don't get left out
  mutate_at(vars(agr:housing), list(~scale(log(. + 1))))

d = dat %>%
  # Pivot
  tidyr::pivot_longer(cols = c(regime_dev, regime_mid, regime_soc, agr:housing), names_to = "var", values_to = "value") %>%
  # Get average
  group_by(year, var) %>%
  summarize(mu = mean(value, na.rm = TRUE),
            sigma = sd(value, na.rm = TRUE) ) %>%
  ungroup() %>%
  mutate(type = case_when(
    var %in% c("regime_dev", "agr", "com_manuf", "construction", "roads_bridges") ~ "dev",
    var %in% c("regime_mid", "education", "edu_social", "health", "planning", "waste") ~ "mid",
    var %in% c("regime_soc", "welfare", "welfare_aged", "welfare_kids", "unemployment", "emergency", "housing") ~ "soc",
    TRUE ~ NA_character_
  )) %>%
  mutate(
    color = type %>% recode_factor(
      "soc" = "#ffb000",
      "mid" = "#648fff",
      "dev" = "#696880"),
    label = type  %>% recode_factor(
    "soc" = "Social Welfare Regime Indicators",
    "mid" = "Middle Class Regime Indicators",
    "dev" = "Developmental Regime Indicators")) 

# Calculate Error stats using raw data
e = bind_rows(
  dat %>%
    lm(formula = regime_soc ~ welfare + welfare_kids + welfare_aged + unemployment + emergency + housing) %>%
    broom::glance(),
  dat %>%
    lm(formula = regime_dev ~ agr + com_manuf + roads_bridges + construction) %>%
    broom::glance(),
  dat %>%
    lm(formula = regime_mid ~ education + edu_social + health + waste + planning) %>%
    broom::glance(), 
  .id = "type"
) %>% 
  mutate(type = type %>% dplyr::recode_factor("1" = "soc", "2" = "dev",  "3" = "mid")) %>%
  select(type, r.squared, sigma) %>%
  mutate(label_sigma = paste0("Avg. Error: ", format(round(sigma, 3), scientific = FALSE) ),
         label_rsq = paste0("R2: ", format(round(r.squared, 3), scientific = FALSE)))
  

# # Calculate error stats using mean lines
# e = bind_rows(
#   d %>%
#     tidyr::pivot_wider(id_cols = c(year), names_from = var, values_from = mu) %>%
#     lm(formula = regime_dev ~ agr + com_manuf + roads_bridges + construction) %>%
#     broom::glance() %>% select(sigma) %>% mutate(type = "dev"),
#   
#   d %>%
#     tidyr::pivot_wider(id_cols = c(year), names_from = var, values_from = mu) %>%
#     lm(formula = regime_soc ~ welfare + welfare_kids + welfare_aged + unemployment + emergency + housing) %>%
#     broom::glance() %>% select(sigma) %>% mutate(type = "soc"),
#   
#   d %>%
#     tidyr::pivot_wider(id_cols = c(year), names_from = var, values_from = mu) %>%
#     lm(formula = regime_mid ~ education + edu_social + health + waste + planning) %>%
#     broom::glance() %>% select(sigma) %>% mutate(type = "mid")
# ) %>%
#   mutate(label = paste0("Avg. Error: ", format(round(sigma, 4), scientific = FALSE) ))
#dat %>% nrow()

v = list(
  soc = list(
    regime_soc = "Social Welfare\nRegime Index\n",
    welfare = "Assistance for\nLow Income\nResidents\n",
    welfare_kids = "Assistance for\nChildren\n",
    welfare_aged = "Assistance for\nElders\n",
    unemployment = "Unemployment\nRelief",
    emergency = "Emergency\nServices",
    housing = "Public\nHousing"
  ),
  mid = list(
    regime_mid = "Middle Class\nRegime Index\n",
    education = "Education\n",
    edu_social = "Social\nEducation\n",
    health = "Health Care\n",
    waste = "Waste & \nRecycling",
    planning = "City\nPlanning"
  ),
  dev = list(
    regime_dev = "Developmental\nRegime Index\n",
    agr = "Agriculture,\nForestry,\n& Fisheries\n",
    com_manuf = "Commerce &\n Manufacturing",
    roads_bridges = "Roads &\n Bridges",
    construction = "Construction"
  )
)
get_viz = function(d, v, e, .type = "soc"){
  .regime = paste0("regime_", .type)
  
  .color = switch(
    EXPR = .type,
    "soc" = "#ffb000",
    "mid" = "#648fff",
    "dev" = "#696880")
  .num = switch(
    EXPR = .type,
    "soc" = 1,
    "mid" = 2,
    "dev" = 3
  )
  
  .title = if(.num == 1){ "Average Index and Indicator Scores over Time" }else{ NULL }
  .caption = if(.num == 3){ "Chart displays sample means; Text shows statistics for entire sample (n = 40062)."}else{NULL}
  .yname = if(.num == 2){ "Normalized Spending Rates\n(Z-score)"}else{ " \n " }
  .xname = if(.num == 3){"Year (2000-2018)"}else{ NULL}
  .striptext = if(.num == 1){ "black" }else{ "white" }
  # Get labels for just that type  
  .v = v[[.type]]

  breaks = names(.v)
  labels = unlist(.v)
  n = length(breaks)  
  .othercolor = .color # #FFFFFF
  .range = seq(from = -0.8, to = -0.1, length.out = (n - 1)) %>% purrr::map_chr(~colorspace::darken(.othercolor, amount = .))
  manycolors = c(.color, .range)
  

  # Show us the average for each
  dviz = d %>% filter(type == .type)
  
  manylines = dviz %>% filter(var != .regime)
  oneline = dviz %>% filter(var == .regime)
  
  .e = dviz %>% select(mu) %>% 
    summarize(ymax = max(mu), ymin = min(mu),
              y1 = ymax, 
              y2 = ymax - (ymax - ymin)/8,
              y3 = ymax - 2*(ymax - ymin)/8,
              x = 2001, type = .type) %>% 
    left_join(y = e, by = "type")
  
  gg = ggplot() +
    geom_line(data = manylines, 
              mapping = aes(x = year, y = mu, color = var, group = var),
              linewidth = .75, alpha = 0.8)  +
    geom_line(data = oneline, 
              mapping = aes(x = year, y = mu, color = var, group = var),
              linewidth = 4, color = "black") +
    geom_line(data = oneline, 
              mapping = aes(x = year, y = mu, color = var, group = var),
              linewidth = 3)  +
    shadowtext::geom_shadowtext(
      data = oneline %>% filter(year == 2014),
      mapping = aes(x = year, y = mu, label = "Index Mean"),
      bg.r = 0.1, bg.color = "black", color = "white", hjust = 0, size = 5) +
    
    shadowtext::geom_shadowtext(
      data = .e,
      mapping = aes(x = x, y = y1, label = "Goodness of Fit (index ~ indicators)"),
      bg.r = 0.3, fontface = "bold", bg.color = "white", color = "#373737", hjust = 0) +
    shadowtext::geom_shadowtext(
      data = .e,
              mapping = aes(x = x, y = y2, label = label_rsq),
              bg.r = 0.3, bg.color = "white", color = "#373737", hjust = 0) +
    shadowtext::geom_shadowtext(
      data = .e,
              mapping = aes(x = x, y = y3, label = label_sigma),
              bg.r = 0.3, bg.color = "white", color = "#373737", hjust = 0) +
    scale_color_manual(
      values = manycolors,
      breaks = breaks,
      labels = labels,
      name = NULL) +
    scale_y_continuous(breaks = seq(from = -3, to = 3, by = 0.25),
                       name = .yname) +
    scale_x_continuous(expand = expansion(c(0,0)),
                       name = .xname) +
    facet_wrap(~label) +
    theme_classic(base_size = 14) +
    theme(panel.border = element_rect(fill = NA, color = "#373737"),
          strip.background = element_rect(fill = .color, color = NA),
          strip.text = element_text(color = .striptext,face = "bold"),
          plot.margin = margin(0,0,0,0,"cm"),
          legend.box.margin = margin(0,0,0,0,"cm"),
          legend.margin = margin(0,r = 0.5,0,0, "cm"),
          plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5)) +
    labs(title = .title, caption = .caption)
  
  return(gg)
}

g1 = get_viz(d = d, v = v, e = e, .type = "soc")
g2 = get_viz(d = d, v = v, e = e, .type = "mid")
g3 = get_viz(d = d, v = v, e = e, .type = "dev")

g = ggpubr::ggarrange(
  plotlist = list(g1, NULL, g2, NULL, g3), 
  nrow = 5, 
  heights = c(1, -0.1, 1, -0.1, 1), align = "hv", legend = "right")

ggsave(g, filename = "viz/mean_trends.png", dpi = 100, width = 8, height = 11)
browseURL("viz/mean_trends.png")

