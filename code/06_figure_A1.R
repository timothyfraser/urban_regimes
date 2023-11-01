#' @name 06_figure_A1.R
#' @author Tim Fraser

# 3. Distributions #########################

# Some cities spend *much, much more*, but few cities spend much less
g1 <- read_rds("raw_data/indices.rds")  %>%
  select(year, muni_code, contains("regime")) %>%
  pivot_longer(cols = contains("regime"), 
               names_to = "type", values_to = "regime") %>%
  mutate(type = type %>% recode_factor(
    "regime_dev" = "Developmental",
    "regime_mid" = "Middle Class",
    "regime_soc" = "Social Welfare")) %>%
  ggplot(mapping = aes(x = regime, y = type, color = type)) +
  geom_jitter(alpha = 0.25) +
  geom_violin(alpha = 0.75, color = "black") +
  labs(x = "Index Score\n(Standard Deviations from the Mean)",
       y = "", subtitle = "Distribution of Urban Regime Index Scores\namong Japanese Cities (2000-2018)") +
  theme_classic(base_size = 14) +
  theme(panel.border = element_rect(fill = NA, color = "black"),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("#648FFF",  "#5C5A8D", "#FFB000")) +
  guides(color = "none") 

ggsave(g1, filename = "viz/distribution.png", width = 6, height = 3, dpi = 500)
