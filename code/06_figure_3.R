#' @name 06_figure_3.R
#' @author Tim Fraser
#' 
#' 
library(dplyr)
library(readr)
library(ggplot2)
library(shadowtext)
library(ggtext)

## 5.2 Areas #######################

tally <- read_rds("raw_data/datcat.rds") %>%
  group_by(type, year) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(percent = count / sum(count))

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

ggsave(g4, filename = "viz/percent_area.png", width = 7, height = 4, dpi = 500)

rm(list = ls())