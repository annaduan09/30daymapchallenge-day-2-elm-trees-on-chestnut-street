#### Setup ####
# Packages
library(tidyverse)
library(tigris)
library(yelpr)
library(sf)
library(mapview)
library(httr)
library(purrr)
library(waffle)
library(stringr)

#### Philadelphia area boundaries ####
options(tigris_use_cache = TRUE) # set cache = TRUE to save time for future calls

# NJ and PA ZIPs
panj_zcta <- zctas(year = 2021) %>%
  st_as_sf() %>%
  st_transform("ESRI:102729") %>%
  st_crop(xmin= 2658842, ymin= 222140, xmax= 2710107, ymax= 250527.3) %>%
  dplyr::select(geometry, ZCTA5CE20) %>%
  st_make_valid() %>%
  erase_water()

phl_zcta <- panj_zcta %>%
  filter(substr(ZCTA5CE20, start = 1, stop = 3) == 191)

tree_bbox <- c(xmin= 2668840, ymin= 227174.5, xmax= 2700284, ymax= 241808.4)

# NJ and PA streets
pa_st <- primary_secondary_roads(year = "2020", state = "PA") %>%
  dplyr::select(geometry)%>%
  st_transform("ESRI:102729") %>%
  st_crop(xmin= 2658842, ymin= 222140, xmax= 2710107, ymax= 250527.3) 

nj_st <- primary_secondary_roads(year = "2020", state = "NJ") %>%
  dplyr::select(geometry)%>%
  st_transform("ESRI:102729") %>%
  st_crop(xmin= 2658842, ymin= 222140, xmax= 2710107, ymax= 250527.3)

bg_st <- rbind(nj_st, pa_st) %>% # final street layer
  st_as_sf() %>%
  st_union()

#### Prep tree and streets data ####
sf_use_s2(FALSE)

# select out streets in center city region, these are the tree named ones
phl_st <- st_read("Data/Street_Centerline.geojson")%>%
  filter(word(ST_NAME, 1) %in% c("RACE", "CHERRY", "ARCH", "CHESTNUT", "LOCUST", "SPRUCE", "PINE", "SOUTH")) %>%
  mutate(ST_NAME = ifelse(ST_NAME == "RACE", "SASSAFRAS (RACE)", 
                         ifelse(ST_NAME == "ARCH", "MULBERRY (ARCH)",
                                ifelse(ST_NAME == "SOUTH", "CEDAR (SOUTH)",
                                       ST_NAME)))) %>%
  st_transform("ESRI:102729") %>%
  group_by(ST_NAME) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_crop(xmin= 2668840, ymin= 227174.5, xmax= 2700284, ymax= 241808.4) 

# buffer to capture trees on either side of street
phl_st_buff <- phl_st %>%
  st_buffer(100) 

# select out trees in center city region to match phl_st
tree <- st_read("Data/PPR_Tree_Inventory_2022.geojson")%>%
  st_transform("ESRI:102729") %>%
  mutate(TREE_NAME = sub(".*- ", "", TREE_NAME),
         TREE_NAME = str_to_title(TREE_NAME),
         TREE_NAME = ifelse(grepl("Cherry", TREE_NAME) == TRUE, "Cherry",
                            ifelse(grepl("Maple", TREE_NAME) == TRUE, "Maple",
                                   ifelse(grepl("Oak", TREE_NAME) == TRUE, "Oak",
                                          ifelse(grepl("Spruce", TREE_NAME) == TRUE, "Spruce", 
                                                 ifelse(grepl("Elm", TREE_NAME) == TRUE, "Elm",
                                                        ifelse(grepl("Crabapple", TREE_NAME) == TRUE, "Crabapple", 
                                                               ifelse(grepl("Linden", TREE_NAME) == TRUE, "Linden",
                                                                      ifelse(grepl("Hawthorn", TREE_NAME) == TRUE, "Hawthorn", TREE_NAME))))))))) %>%
  filter(is.na(TREE_NAME) == FALSE & TREE_NAME != "Unknown" & TREE_NAME != "" & TREE_NAME != " ") %>%
  st_crop(xmin= 2668840, ymin= 227174.5, xmax= 2700284, ymax= 241808.4) 

tree_st <- tree %>%
  st_intersection(phl_st_buff)

species_st <- tree_st %>%
  st_drop_geometry() %>%
  mutate(count = 1) %>%
  dplyr::group_by(TREE_NAME, ST_NAME) %>%
  dplyr::summarize(count = sum(count)) %>%
  arrange(desc(count)) %>%
  ungroup() %>%
  group_by(ST_NAME) %>%
  slice_max(order_by = count, n = 1) %>%
  ungroup() %>%
  left_join(phl_st) %>%
  st_as_sf()

species_all <- tree_st %>%
  mutate(count = 1) %>%
  dplyr::group_by(TREE_NAME) %>%
  dplyr::summarize(count = sum(count)) %>%
  arrange(desc(count)) %>%
  st_drop_geometry()

#### map ####
pal <- c("#da68f8", "#bdf767", "#70f7ee", "#d34ea5", "#0df38f", "#f85b57", "#21a645", "#fa1bfc", "#34f50e", "#c78bb9", "#809b31", "#7a2edc", "#f79302", "#4d57a8", "#dad892", "#53a4e5", "#fbd127", "#256676", "#fcd6ff", "#8b500e", "#1eafae", "#7d525f", "#fcb790")

ggplot() +
  geom_sf(data = panj_zcta, fill = "gray20", color = "gray30", size = 0.01) +
  geom_sf(data = bg_st %>% st_buffer(150), color = "gray25", fill = "gray25",) +
  geom_sf(data = species_st %>% st_buffer(150), aes(color = TREE_NAME, fill = TREE_NAME)) +
  scale_color_manual(values=pal) +
  scale_fill_manual(values=pal) +
  labs(color = "Species", fill = "Species") +
  theme_void() + theme(legend.position = c(0.18, 0.3),
                          legend.text = element_text(color = "white", face = "italic"),
                          legend.title = element_text(color = "white", face = "bold"))
