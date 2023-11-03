#### Setup ####
# Packages
library(tidyverse)
library(tigris)
library(sf)
library(mapview)
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

# all of PHL for inset map
phl_bound <- zctas(year = 2021) %>%
  st_as_sf() %>%
  st_transform("ESRI:102729") %>%
  st_crop(xmin= 2657656, ymin= 197945.6, xmax= 2750109, ymax= 304965.3) %>%
  dplyr::select(geometry, ZCTA5CE20) %>%
  st_make_valid() %>%
  erase_water()

# phl_bound <- zctas(year = 2021) %>%
#   filter(substr(ZCTA5CE20, start = 1, stop = 3) == 191) %>%
#   st_as_sf() %>%
#   st_transform("ESRI:102729") %>%
#   st_make_valid() %>%
#   erase_water()

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

# phl_st <- roads(year = "2020", state = "PA", county = 101) %>%
#   st_transform("ESRI:102729") %>%
#   st_crop(xmin= 2658842, ymin= 222140, xmax= 2710107, ymax= 250527.3) %>%
#   group_by(FULLNAME) %>%
#   summarize(geometry = st_union(geometry)) %>%
#   filter(word(FULLNAME, 1) %in% c("Race", "Cherry", "Arch", "Chestnut", "Walnut",
#                                   "Locust", "Spruce", "Pine", "South") |
#            word(FULLNAME, 2) %in% c("Race", "Cherry", "Arch", "Chestnut", "Walnut",
#                                     "Locust", "Spruce", "Pine", "South")) %>%
#   rename(street =)

bg_st <- rbind(nj_st, pa_st) %>% # final street background layer
  st_as_sf() %>%
  st_union()

#### Prep tree and streets data ####
sf_use_s2(FALSE)

# select out streets in center city region, these are the tree named ones
phl_st <- st_read("Data/Street_Centerline.geojson")%>%
 filter(word(ST_NAME, 1) %in% c("RACE", "CHERRY", "ARCH", "CHESTNUT", "WALNUT", "LOCUST", "SPRUCE", "PINE", "SOUTH")) %>%
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

species_street_df <- tree_st %>%
  mutate(count = 1) %>%
  dplyr::group_by(TREE_NAME, ST_NAME) %>%
  dplyr::summarize(count = sum(count)) %>%
  arrange(desc(count)) %>%
  ungroup() %>%
  st_drop_geometry()

#### map ####

# blue background rectangle for water features
water_rect <- st_as_sfc(st_bbox(panj_zcta), crs = "ESRI:102729")
# palette
pal <- c("Callery Pear" = "chartreuse2", "Honeylocust" = "orchid1", 
         "London Planetree" = "cyan4", "Maple" = "orange1", 
         "Oak" = "turquoise", "Elm" = "yellow", "Cherry" = "navy", 
          "Linden" = "pink", "Ginkgo" = "salmon", "Japanese Zelkova" = "thistle1")
# map
ggplot() +
  geom_sf(data = water_rect, fill = "lightblue2") +
  geom_sf(data = panj_zcta, fill = "snow2", color = NA, size = 0.5) +
  geom_sf(data = phl_zcta, fill = "snow1", color = NA, size = 0.8) +
  geom_sf(data = bg_st, color = "snow2") +
  geom_sf(data = species_st %>% st_buffer(50), aes(color = TREE_NAME, fill = TREE_NAME), size = 10) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  labs(color = "", fill = "") +
  theme_void() + theme(legend.position = c(0.15, 0.25),
                          legend.text = element_text(color = "snow1"))

# inset map
ggplot() +
  geom_sf(data = phl_bound, color = "snow2", fill = "snow1") +
  theme_void() + theme(panel.background = element_rect(fill = "lightblue2"))

#### waffle plots ####
# Split the data frame by ST_NAME to create a list of data frames
list_of_dfs <- split(species_street_df, species_street_df$ST_NAME)
# Function to process each data frame and get the top 6 species
process_df <- function(df) {
  # Order by descending count
  df <- df[order(-df$count), ]
  # Keep only the top 6 species
  top6 <- head(df, 6)
  # Create a named vector for these top 6 species
  top6_vector <- setNames(top6$count, top6$TREE_NAME)
  return(top6_vector)
}
# Apply the function to each data frame in the list to create the vectors
street_vectors <- lapply(list_of_dfs, process_df)
# Name the vectors with 'species_' prefix and the street names
list2env(setNames(street_vectors, paste0("species_", names(street_vectors))), envir = .GlobalEnv)

unique_species <- unique(species_street_df$TREE_NAME)

# palette 
top_species_per_street <- lapply(list_of_dfs, function(df) {
head(df$TREE_NAME, 6)
})

# Unlist to create a single vector with all species
all_top_species <- unlist(top_species_per_street)

# Get unique species from this combined vector
unique_top_species <- unique(all_top_species)

# plots
waffle(`species_CEDAR (SOUTH)`/3,
       colors = pal, title = "south (cedar)")

waffle(`species_CHERRY`/1.5,
       colors = pal, title = "cherry")

waffle(`species_CHESTNUT`/6,
       colors = pal, title = "chestnut")

waffle(`species_WALNUT`/7,
       colors = pal, title = "walnut")

waffle(`species_LOCUST`/4,
       colors = pal, title = "locust")


waffle(`species_SPRUCE`/5.5,
       colors = pal, title = "spruce")

waffle(`species_MULBERRY (ARCH)`/3.5,
       colors = pal, title = "arch(mulberry)")

waffle(`species_PINE`/6,
       colors = pal, title = "pine")

waffle(`species_SASSAFRAS (RACE)`/3.5,
       colors = pal, title = "race (sassafras)")

waffle(`species_SASSAFRAS (RACE)`/3.5,
       colors = pal, title = "race (sassafras)")