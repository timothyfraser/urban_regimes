#' @name 06_figure_4.R
#' @author Tim Fraser

# 6. Maps ##############################

## Build Mapping Files

# Let's map these indices! To do so, we use a shapefile of 
# municipal political boundaries from the
# Ministry of Land, Infrastructure, and Transportation. 
# Then, using an East Asian Albers Equal Area Conic projection, we map them. 

#Let's create a shapefile of municipalities.

## Packages #####################
library(tidyverse)
library(sf)
# library(rgdal)


# This data uses the North East Asia Albers Equal Distance Conic Projection
eqdc = "+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=15 +lat_2=65 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# and also the North East Asia Albers Equal Area Conic Project
eqac = "+proj=aea +lat_1=15 +lat_2=65 +lat_0=30 +lon_0=95 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# rotate = 180
rotate = 90
az = st_crs(paste0("+proj=aeqd +ellps=sphere +lat_0=90 +lon_0=", rotate))


# First, let's read in our shapefile of municipal political boundaries
read_sf("raw_data/shapes/polbnda_jpn.shp") %>%
  # Transform to Asia North Albers Equal Area Conic projection
  # Obtained here: https://spatialreference.org/ref/esri/102025/
  st_transform(crs = st_crs(eqac)) %>%
  # Let's make one row/shape per municipality code
  group_by(muni_code = adm_code) %>%
  summarize(geometry = st_union(geometry)) %>%
  ungroup() %>%
  # Transform to an Azimuthal Equidistant Projection, 
  # rotated XX degrees to allow easy visualization (to make Japan sideways)
  st_transform(crs = az)  %>%
  saveRDS("raw_data/muni_shapes.rds")


# Now generate prefectural boundaries
read_rds("raw_data/muni_shapes.rds") %>%
  group_by(pref_code = str_sub(muni_code, 1,2)) %>%
  summarize(geometry = st_union(geometry)) %>%
  ungroup() %>%
  saveRDS("raw_data/pref_shapes.rds")

read_rds("raw_data/pref_shapes.rds") %>% 
  summarize(geometry = st_union(geometry)) %>%
  saveRDS("raw_data/country_shapes.rds")

## Maps over Time ################
# 
# box <- read_rds("raw_data/muni_shapes.rds") %>% 
#   st_bbox() %>% c() %>%
#   data.frame(area = .) %>%
#   tibble::rownames_to_column(var = "type") %>%
#   mutate(area = if_else(type == "ymin", area - area / 5.4, area),
#          area = if_else(type == "ymax", area + area / 7.3, area),
#          area = if_else(type == "xmin", area - area / 3.5, area),
#          area = if_else(type == "xmax", area + area / 80, area)) %>%
#   pivot_wider( names_from = type, values_from = area) %>%
#   unlist() %>%
#   st_bbox()

# Approximate bbox
# box = c(xmin = 129, xmax = 146, ymin = 30, ymax = 46) %>%
#   st_bbox(crs = 4326) %>%
#   st_as_sfc(crs = 4326) %>%
#   st_as_sf()
# Reset bbox crs
# box = st_bbox(st_transform(st_as_sfc(bbox), crs = az))

# rotate = 180
rotate = 90
az = st_crs(paste0("+proj=aeqd +ellps=sphere +lat_0=90 +lon_0=", rotate))

rawbox = c(xmin = 3852894, xmax = 4723286, ymin = -5002514, ymax = -2839368)
boxaz = rawbox %>%
  st_bbox(crs = az) %>%
  st_as_sfc(crs = az) %>%
  st_as_sf(crs = az) 

country <- read_rds("raw_data/country_shapes.rds") %>%
  st_crop(boxaz) 
prefs <- read_rds("raw_data/pref_shapes.rds") %>%
  st_crop(y = boxaz)
shapes <- read_rds("raw_data/muni_shapes.rds") %>%
  # Join in urban regime categories
  left_join(by = "muni_code", 
            y = read_rds("raw_data/datcat.rds"),
            multiple = "all") %>%
  st_crop(boxaz)

# # boxaz = box %>% st_transform(crs = az)
# ggplot() +
#   geom_sf(data = country) +
#   geom_sf(data = boxaz, fill = NA, color = "red")
# 
# shapes %>% head()


# boundaries = "#9090C0"

get_gg = function(.year = 2018, shapes, prefs, country, box){
  mylevels = shapes %>%  with(levels(type))
  mylevels = mylevels %>% 
    stringr::str_replace(pattern = "[(]", replacement = "  \n(") %>%
    stringr::str_replace(pattern = "[)]", replacement = ")\n")
  ggplot() +
    geom_sf(data = country, fill = NA, color = "lightblue", linewidth = 3) +
    # geom_sf(data = prefs, color = boundaries, size = 2, fill = "grey") +
    geom_sf(data = shapes %>%
              filter(year == .year), 
            mapping = aes(fill = type), color = "darkgrey", linewidth = 0.01) +
    # Overlay prefectural boundaires
    geom_sf(data = country, color = "black", linewidth = 0.2, fill = NA) +
    geom_sf(data = prefs, color = "black", linewidth = 0.1, fill = NA) +
    geom_sf(data = shapes %>%
              filter(year == .year) %>%
              filter(str_detect(type, "Social Welfare")), 
            mapping = aes(fill = type), color = "white", linewidth = 0.07) +
    scale_fill_manual(
      values = c(
        "#696880", "#ADADC9", "#648FFF", "#001FA2", 
        "#5542AD", "#DC267F", "#ff683b", "#ffb000"
      ),
      labels = mylevels
      
    ) +
    labs(y = NULL, x = NULL, fill = "Urban Regimes", title = .year) +
    theme_void(base_size = 13) +
    theme(plot.margin = margin(0,0,0,0, "cm"), 
          legend.margin = margin(0,0,0,0,"cm"),
          legend.box.margin = margin(0,0,0,0,"cm"),
          #panel.border = element_rect(color = "#373737", fill = NA),
          plot.title = element_text(hjust = 0.1,vjust = -5.5),
          legend.position = "right") +
    coord_sf(ylim = c(box["ymin"]+(1e4*8), box["ymax"]-(1e4*8)),
             xlim = c(box["xmin"]+(1e4*2), box["xmax"]-(1e4*2)))
  
}

g1 = get_gg(.year = 2000, shapes = shapes, prefs = prefs, country = country, box = rawbox)
g2 = get_gg(.year = 2010, shapes = shapes, prefs = prefs, country = country, box = rawbox)
g3 = get_gg(.year = 2018, shapes = shapes, prefs = prefs, country = country, box = rawbox)

combo <- ggpubr::ggarrange(g1, NULL, g2, NULL, g3, 
                           widths = c(1, -0.5, 1, -0.5, 1),
                           nrow = 1, common.legend = TRUE, legend = "right") +
  theme(
    legend.box.margin = margin(0,r = 0,0,l = 0.25,"cm"),
    legend.margin = margin(0,0,0,l = 0.25,"cm"),
    plot.subtitle = element_blank(),
    panel.spacing = unit(0, "cm"),
    panel.border = element_blank(),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"))

ggsave(combo, filename = "viz/map_categories_3b.png", width = 7.5, height = 5, dpi = 500)

#rstudioapi::viewer("viz/map_categories_3b.png")
# Now calculate prefectural boundaries

# g1 <- ggplot() +
#   geom_sf(data = prefs, color = boundaries, size = 2, fill = "grey") +
#   geom_sf(data = shapes %>%
#             filter(year == 2017), 
#           mapping = aes(fill = type), color = "darkgrey", size = 0.01) +
#   # Overlay prefectural boundaires
#   geom_sf(data = country, color = "black", size = 0.2, fill = NA) +
#   geom_sf(data = prefs, color = "black", size = 0.1, fill = NA) +
#   geom_sf(data = shapes %>%
#             filter(year == 2017) %>%
#             filter(str_detect(type, "Social Welfare")), 
#           mapping = aes(fill = type), color = "white", size = 0.07) +
#   scale_fill_manual(values = c(
#     "#696880", "#ADADC9", "#648FFF", "#001FA2", 
#     "#5542AD", "#DC267F", "#ff683b", "#ffb000"
#   )) +
#   labs(y = NULL, x = NULL, fill = "Urban Regimes", title = "2010") +
#   theme_void(base_size = 13) +
#   theme(plot.margin = margin(0,0,0,0, "cm"), 
#         legend.margin = margin(0,0,0,0,"cm"),
#         legend.box.margin = margin(0,0,0,0,"cm"),
#         #panel.border = element_rect(color = "#373737", fill = NA),
#         plot.title = element_text(hjust = 0.5,vjust = -15.5),
#         legend.position = "right") +
#   coord_sf(xlim = c(box["xmin"]+(1e4*8), box["xmax"]-(1e4*8)),
#            ylim = c(box["ymin"]+(1e4*2), box["ymax"]-(1e4*2)))
# ggsave(g1, filename = "viz/map_2017b.png", dpi = 500, width = 8, height = 2.5)
#
# g2 <- ggplot() +
#   geom_sf(data = prefs, color = boundaries, size = 2, fill = "grey") +
#   geom_sf(data = shapes %>%
#             filter(year == 2010), 
#           mapping = aes(fill = type), color = "darkgrey", size = 0.01) +
#   # Overlay prefectural boundaires
#   geom_sf(data = country, color = "black", size = 0.2, fill = NA) +
#   geom_sf(data = prefs, color = "black", size = 0.1, fill = NA) +
#   geom_sf(data = shapes %>%
#             filter(year == 2010) %>%
#             filter(str_detect(type, "Social Welfare")), 
#           mapping = aes(fill = type), color = "white", size = 0.07) +
#   scale_fill_manual(values = c(
#     "#696880", "#ADADC9", "#648FFF", "#001FA2", 
#     "#5542AD", "#DC267F", "#ff683b", "#ffb000"
#   )) +
#   labs(y = NULL, x = NULL, fill = "Urban Regimes", title = "2010") +
#   theme_void(base_size = 13) +
#   theme(plot.margin = margin(0,0,0,0, "cm"), 
#         legend.margin = margin(0,0,0,0,"cm"),
#         legend.box.margin = margin(0,0,0,0,"cm"),
#         #panel.border = element_rect(color = "#373737", fill = NA),
#         plot.title = element_text(hjust = 0.5,vjust = -15.5),
#         legend.position = "right") +
#   coord_sf(xlim = c(box["xmin"]+(1e4*8), box["xmax"]-(1e4*8)),
#            ylim = c(box["ymin"]+(1e4*2), box["ymax"]-(1e4*2)))
# 
# # Now calculate prefectural boundaries
# g3 <- ggplot() +
#   geom_sf(data = prefs, color = "#9090C0", size = 2, fill = "grey") +
#   geom_sf(data = shapes %>%
#             filter(year == 2000), 
#           mapping = aes(fill = type), color = "darkgrey", size = 0.01) +
#   # Overlay prefectural boundaires
#   geom_sf(data = country, color = "black", size = 0.2, fill = NA) +
#   geom_sf(data = prefs, color = "black", size = 0.1, fill = NA) +
#   geom_sf(data = shapes %>%
#             filter(year == 2000) %>%
#             filter(str_detect(type, "Social Welfare")), 
#           mapping = aes(fill = type), color = "white", size = 0.07) +
#   scale_fill_manual(values = c(
#     "#696880", "#ADADC9", "#648FFF", "#001FA2", 
#     "#5542AD", "#DC267F", "#ff683b", "#ffb000"
#   )) +
#   labs(y = NULL, x = NULL, fill = "Urban Regimes", title = "2000") +
#   theme_void(base_size = 13) +
#   theme(plot.margin = margin(0,0,0,0, "cm"), 
#         legend.margin = margin(0,0,0,0,"cm"),
#         legend.box.margin = margin(0,0,0,0,"cm"),
#         #panel.border = element_rect(color = "#373737", fill = NA),
#         plot.title = element_text(hjust = 0.5,vjust = -15.5),
#         legend.position = "right") +
#   coord_sf(xlim = c(box["xmin"]+(1e4*8), box["xmax"]-(1e4*8)),
#            ylim = c(box["ymin"]+(1e4*2), box["ymax"]-(1e4*2)))

# combo <- ggpubr::ggarrange(g1, NULL, g2, NULL, g3, 
#                            heights = c(1, -0.35, 1, -0.35, 1),
#                            ncol = 1, common.legend = TRUE, legend = "right") +
#   theme(
#     legend.box.margin = margin(0,0,0,l = 0.5,"cm"),
#     legend.margin = margin(0,0,0,l = 0.5,"cm"),
#     plot.subtitle = element_blank(),
#     panel.spacing = unit(0, "cm"),
#     panel.border = element_blank(),
#     plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"))
# 
# ggsave(combo, filename = "viz/map_categories_3.png", width = 7.5, height = 5, dpi = 500)
# and also the North East Asia Albers Equal Area Conic Project

# -6117697 ymin: -6567243 xmax: -2555485 ymax: -3842894
#Azimuthal Equidistant Projection
rm(list= ls())
