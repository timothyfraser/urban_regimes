#' @name 06_figure_2.R
#' @author Tim Fraser

# 4. Bands #########################

# How much does spending per capita change over time?
dat <- read_rds("raw_data/indices.rds")  %>%
  select(year, muni_code, contains("regime")) %>%
  pivot_longer(cols = contains("regime"), 
               names_to = "type", values_to = "regime") %>%
  mutate(type = type  %>% recode_factor(
    "regime_soc" = "Social Welfare",
    "regime_mid" = "Middle Class",
    "regime_dev" = "Developmental"))

mystats <- bind_rows(
  dat %>%
    group_by(year, type) %>%
    summarize(
      bands = "5-95%",
      median = median(regime, na.rm = TRUE),
      low = quantile(regime, probs = 0.95, na.rm = TRUE),
      high = quantile(regime, probs = 0.05, na.rm = TRUE)),
  dat %>%
    group_by(year, type) %>%
    summarize(
      bands = "25-75%",
      low = quantile(regime, probs = 0.25, na.rm = TRUE),
      high = quantile(regime, probs = 0.75, na.rm = TRUE))) 

library(ggtext)
library(shadowtext)
library(ggnewscale)

mylabels <- mystats %>%
  filter(year %in% c(2000, 2018) ) %>%
  pivot_longer(cols = c(median, low, high), names_to = "stat", values_to = "value") %>%
  mutate(label = round(value, 2)) %>%
  mutate(color = paste(stat, bands),
         color = case_when(
           str_detect(color, "median") ~ "median",
           str_detect(color, "25-75%") ~ "inner",
           str_detect(color, "5-95%") ~ "outer"))

g1 <- ggplot() +
  geom_ribbon(data = mystats %>% filter(bands == "5-95%"),
              mapping = aes(x = year, y = median, ymin = low, ymax = high, fill = bands)) +
  geom_ribbon(data = mystats %>% filter(bands == "25-75%"), 
              mapping = aes(x = year, y = median, ymin = low, ymax = high, fill = bands),
              color = "white", size = 1.25) +
  geom_line(data = mystats, mapping = aes(x = year, y = median, group = bands, color = "Median (50%)"), size = 0.75) +
  facet_wrap(~type, ncol = 3) +
  theme_classic(base_size = 14) +
  theme(panel.border = element_rect(fill = NA, color = "black"),
        panel.spacing = unit(0.5, "cm")) +
  scale_fill_manual(
    values = c("#DC267F", "#ADADC9"),
    breaks = c("25-75%", "5-95%"),
    labels = c("Most Common (25-75%", "General Span (5-95%)")
  ) +
  scale_color_manual(values = "black", guide = guide_legend(override.aes = list(size = 1.5))) +
  scale_y_continuous(limits = c(-2, 2)) +
  labs(x = "Year (2000-2018)", y = "Urban Regime Index Score",
       fill = NULL, color = NULL) +
  theme(legend.position = c(0.85, 0.1), 
        strip.text.x = ggtext::element_markdown(size = 14, hjust = 0.5),
        legend.background = element_rect(fill = NA), legend.margin = margin(0,0,0,0, "cm"), 
        legend.spacing = unit(0, "cm"),
        axis.ticks = element_blank(), axis.line = element_blank(),
        panel.border = element_rect(color = "#373737", fill = NA),
        strip.background = element_blank()) +
  ggnewscale::new_scale("color") +
  geom_shadowtext(data = mylabels %>% filter(year == 2000), 
                  mapping = aes(x = year, y = value, label = label, group = bands, color = color),
                  hjust = 0, vjust = 0, bg.r = 0.3, bg.color = "white") +
  geom_shadowtext(data = mylabels %>% filter(year == 2018), 
                  mapping = aes(x = year, y = value, label = label, group = bands, color = color),
                  hjust = 1, vjust = 0, bg.r = 0.3, bg.color = "white") +
  scale_color_manual(
    values = c("black", "#B31C66", "#373737"), # 
    breaks = c("median", "inner", "outer"), guide = "none")  

ggsave(g1, filename = "viz/bands.png", width = 8, height = 6, dpi = 500)

rm(list= ls())