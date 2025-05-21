#### SELECT SITES

### Author: Ulisse Gomarasca (ugomar@bgc-jena.mpg.de)
### Script settings ------------------------------------------------------------
# Clear environment
rm(list = ls(all = TRUE))

# Data settings
vers_in <- as.character(read.table("data/efp_version.txt"))
vers_out <- paste0(vers_in)
savedata <- as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved



### Utilities ------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggthemes)
library(patchwork)    # combine plots
library(readr)
library(readxl)
library(tibble)
library(tidyr)
library(sf)
library(sp)

## Other (for plotting)
source("scripts/themes/MyCols.R")
source("scripts/themes/MyPlotSpecs.R")
source("scripts/themes/MyThemes.R")



### Data -----------------------------------------------------------------------
## Site list
site_csv <- read_csv("data/inter/site_list_all.csv", show_col_types = F)

## site coordinates
coords <- read_csv("data/input/coords.csv", show_col_types = F)
# coords_siteyears <- read_csv("data/input/coords_ByYears_v03.csv", show_col_types = F)

# coords <- bind_rows(coords, coords_siteyears) %>% unique()



### Attach coordinates and IGBP class ------------------------------------------
site_all <- site_csv %>% 
  left_join(read_csv("data/input/igbp.csv", show_col_types = F), by = "SITE_ID") %>% 
  left_join(coords, by = "SITE_ID") %>% 
  rename(longitude = LONGITUDE, latitude = LATITUDE) %>% 
  unique()



### Convert to shapefile -------------------------------------------------------
site_shp <- st_as_sf(site_all, coords = c("longitude", "latitude"), crs = st_crs(4326))

# world <- map_data('world')
# worldBound <- st_as_sf(world, coords = c("long", "lat"))
# 
# 
# loc_spdf <- SpatialPointsDataFrame(coords = coords, data = site_all,
#                                    proj4string = crs(worldBound))

# ggplot() +
#   geom_map(
#     data = world, map = worldBound_robin,
#     aes(map_id = region),
#     color = "white", fill = "lightgray", linewidth = 0.1
#   ) +
#   geom_sf(data = site_shp) +
#   coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), default_crs = "+proj=robin")


### Plot -----------------------------------------------------------------------
# ## Old map ----
# world <- map_data('world')
# p_map <- ggplot() +
#   geom_map(
#     data = world, map = world,
#     aes(long, lat, map_id = region),
#     color = "white", fill = "lightgray", linewidth = 0.1
#   ) +
#   geom_point(
#     data = site_all,
#     aes(longitude, latitude, color = IGBP),
#     alpha = alpha_medium, shape = 18, size = point_size_medium_small, stroke = line_width_thick
#   ) +
#   scale_color_manual(values = CatCol_igbp) +
#   guides(color = guide_legend(title = "IGBP class: ")) +
#   # geom_polygon(
#   #   data = site_shp,
#   #   aes(geometry),
#   #   alpha = 0.7
#   # ) +
#   coord_equal() +
#   # coord_map("stereographic") +
#   theme_transparent +
#   theme_bw() +
#   theme(
#     axis.title = element_blank(),
#     # legend.position = "none"
#     legend.title = element_text(size = text_size_medium, lineheight = text_size_medium),
#     legend.text = element_text(size = text_size_medium - 2, lineheight = text_size_medium - 2)
#     ) +
#   NULL

## Equal-area map ----
# based on: https://www.riinu.me/2022/02/world-map-ggplot2/
world_map <- map_data("world") %>% 
  filter(! long > 180)

countries <- world_map %>% 
  distinct(region) %>% 
  rowid_to_column() %>% 
  dplyr::filter(region != "Antarctica") %>% 
  mutate(rowid = factor(rowid))

# Palette
mypalette <- colorRampPalette(Five_vintage_geo3_light1)(length(unique(countries$rowid))) # generate color palette
set.seed(009); mypalette <- sample(mypalette) # randomize order

# Map
p_map <- countries %>% 
  ggplot() +
  geom_map(aes(fill = rowid, map_id = region), map = world_map,
           color = NA, fill = Five_vintage_geo3_light1[3],  # no country for a-political map
           # color = "white", # map with borders instead of above line
           linewidth = 0.2, show.legend = F
           ) +
  geom_point(
    data = site_all,
    aes(longitude, latitude, color = IGBP),
    alpha = 0.8, shape = 18, size = point_size_medium_small, stroke = line_width_thick,
    inherit.aes = F
  ) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  coord_map("mollweide") +
  scale_fill_manual(values = mypalette) +
  scale_color_manual(values = CatCol_igbp) +
  guides(color = guide_legend(title = "IGBP class: ", nrow = 1,
                              override.aes = list(alpha = 1, size = point_size_medium + 1)
                              )
         ) +
  theme_map() +
  # theme_transparent +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
    legend.title = element_text(size = text_size_big + 3, lineheight = text_size_big + 3),
    legend.text = element_text(size = text_size_big, lineheight = text_size_big),
    panel.border = element_rect(fill = NA, color = line_color_plot),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
  ) +
  NULL
p_map


## Europe map ----
lat_min <- 36; lat_max <- 70;      lon_min <- -9; lon_max <- 30 # Europe bounding box

eu_countries <- countries %>% 
  dplyr::filter(region %in% c(
    "Algeria", "Morocco", "Libia", "Egypt", "Tunisia",
    "Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czech Republic",
    "Denmark", "Estonia", "Finland", "France", "Georgia", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Kazakhstan", "Kosovo",
    "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands", "North Macedonia", "Norway",
    "Poland", "Portugal", "Romania", "Russia", "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine", "UK", "Vatican"
  ))

# Palette
mypalette <- colorRampPalette(Five_vintage_geo3_light1)(length(unique(eu_countries$rowid))) # generate color palette
set.seed(009); mypalette <- sample(mypalette) # randomize order

# Map
p_eu <- eu_countries %>% 
  ggplot() +
  geom_map(aes(fill = rowid, map_id = region), map = world_map,
           color = NA, fill = Five_vintage_geo3_light1[3],  # no country for a-political map
           # color = "white", # map with borders instead of above line
           linewidth = 0.2, show.legend = F
  ) +
  geom_point(
    data = site_all %>%
      dplyr::filter(latitude > lat_min & latitude < lat_max & longitude > lon_min & longitude < lon_max | SITE_ID %in% c("RU-Fyo", "RU-Fy2")),
    aes(longitude, latitude, color = IGBP),
    alpha = 0.8, shape = 18, size = point_size_medium_small, stroke = line_width_thick,
    inherit.aes = F
  ) +
  # xlim(c(lon_min, lon_max)) + ylim(c(lat_min, lat_max)) +
  coord_map("mollweide") +
  scale_fill_manual(values = mypalette) +
  scale_color_manual(values = CatCol_igbp) +
  guides(color = guide_legend(title = "IGBP class: ", nrow = 1,
                              override.aes = list(alpha = 1, size = point_size_medium + 1)
                              )
  ) +
  theme_map() +
  # theme_transparent +
  theme(
    legend.position = "none",
    panel.border = element_rect(fill = NA, color = line_color_plot),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
  ) +
  NULL
p_eu


## Combine maps ----
p_out <- p_map +
  inset_element(
    ggplot() +
      geom_rect(
        aes(xmin = lon_min, xmax = lon_max, ymin = lat_min, ymax = lat_max),
        color = line_color_plot, fill = NA, linewidth = 0.2,
        show.legend = F, inherit.aes = F
        ) +
      theme_void(),
    left = 0.47, bottom = 0.725, right = 0.58, top = 0.945, ignore_tag = T
  ) +
  inset_element(p_eu, left = -0.2, bottom = 0.05, right = 0.5, top = 0.55, ignore_tag = T) +
  # theme(
  #   rect = element_rect(color = line_color_plot, linewidth = 0.25)
  # ) +
  NULL
p_out

## Transparent
p_transparent <- p_out  & theme_transparent & theme(rect = element_rect(color = NA, fill = NA))



### Save -----------------------------------------------------------------------
if (savedata) {
  ## data
  write_csv(site_all, glue::glue("data/inter/site_list_all_coords_{vers_out}.csv"))
  sf::st_write(site_shp, glue::glue("data/inter/site_list_all_{vers_out}.shp"), append = F)
  
  ## map
  ggplot2::ggsave(filename = glue::glue("processed_sites_{vers_out}_no_borders.jpg"), plot = p_map, device = "jpeg",
                  path = "results/maps", width = 508, height = 254, units = "mm", dpi = 300) # 1920 x  1080 px resolution (16:9)
  ggplot2::ggsave(filename = glue::glue("site_map_w_inset_{vers_out}_no_borders.jpg"), plot = p_out, device = "jpeg",
                  path = "results/maps", width = 508, height = 254, units = "mm", dpi = 600) # 1920 x  1080 px resolution (16:9)
  # transparent png
  ggplot2::ggsave(filename = glue::glue("site_map_w_inset_{vers_out}_no_borders.png"), plot = p_transparent, device = "png",
                  path = "results/maps", bg = "transparent", width = 508, height = 254, units = "mm", dpi = 300)
  
  
  # ggplot2::ggsave(filename = "processed_sites.png", plot = p_map, device = "png",
  #                 bg = "transparent",
  #                 path = "results/maps", width = 508, height = 254, units = "mm", dpi = 150) # 1920 x  1080 px resolution (16:9)
}