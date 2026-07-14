library(tidyverse)
library(tigris)
options(tigris_use_cache = TRUE)
library(sf)
library(mapview)

ne_legis <- state_legislative_districts(state = "Nebraska") |> 
  mutate(District = str_remove(NAMELSAD, "^State Senate District ") |> 
           str_extract("\\d+") |> 
           str_trim() |> 
           as.integer()) |> 
  st_transform(crs = 4326) |> 
  select(District, geometry)

ne_county <- counties(state = "Nebraska") |> 
  rename(County = NAME) |> 
  st_transform(crs = 4326) |> 
  select(County, geometry)

# Lincoln Lancaster Geography --------------------------------------------------
lincoln_small <- c(2, 21, 32, 30, 25, 27, 46, 26, 28, 29)

lancaster_geom <- filter(ne_county, County == "Lancaster")

legis_lancaster <- ne_legis |> 
  filter(District %in% lincoln_small) |> 
  st_intersection(lancaster_geom) |> 
  st_make_valid()

mapview(lancaster_geom) +
  mapview(legis_lancaster)

# Identify cropped area of Lancaster geom
st_bbox(lancaster_geom)
lancaster_bb <- st_bbox(c(xmin = -96.91393, ymin = 40.63869,
                          xmax = -96.46362, ymax = 40.96996),
                        crs = st_crs(lancaster_geom))

sf_lancaster_box <- st_as_sfc(lancaster_bb) |> 
  st_sf(geometry = _)

# Crop Lancaster legislative districts with clipped Lancaster geom
legis_lincoln <- st_intersection(legis_lancaster, sf_lancaster_box)

mapview(legis_lincoln)

# Omaha and Surrounding Counties Geography -------------------------------------
omaha_small <- c(23, 12, 15, 39, 36, 18, 10, 4, 31, 20, 6, 49, 13, 8, 9, 5, 14, 3, 11, 7, 45)

saunders_geom <- filter(ne_county, County == "Saunders")
douglas_geom <- filter(ne_county, County == "Douglas")
sarpy_geom <- filter(ne_county, County == "Sarpy")

# Create Douglas and Sarpy as 'sfc' class geoms (geometry without data columns)
douglas_sfc <- st_geometry(douglas_geom)
sarpy_sfc   <- st_geometry(sarpy_geom)

# Merge Douglas and Sarpy geoms
douglassarpy_sfc <- st_union(douglas_sfc, sarpy_sfc) |> 
  st_make_valid()

# Identify cropped area of Douglas/Sarpy geom
bbox_crop <- st_bbox(c(xmin = -96.47072, ymin = 41.05226, 
                       xmax = -95.84161, ymax = 41.39331), 
                     crs = st_crs(douglassarpy_sfc))

# Crop Douglas/Sarpy geom as class 'sfc'
douglassarpy_clip_sfc <- st_crop(douglassarpy_sfc, bbox_crop)

# Identify cropped area of Saunders geom
saunders_bb <- st_bbox(c(xmin = -96.47076, ymin = 41.05226,
                         xmax = -96.31098, ymax = 41.39317),
                       crs = st_crs(saunders_geom))

# Crop Saunders geom as class 'sfc'
saunders_clip_sfc <- st_crop(st_geometry(saunders_geom), saunders_bb)


# Merge Douglas/Sarpy with Saunders
omaha_raw_sfc <- st_union(douglassarpy_clip_sfc, saunders_clip_sfc)

# Identify cropped area of Omaha legislative districts
omaha_keep_bb <- st_bbox(c(xmin = -96.47076, ymin = 41.05226,
                           xmax = -95.84161, ymax = 41.39331),
                         crs = st_crs(omaha_raw_sfc))

# Merge Douglas/Sapry and Saunders geoms into one geom
omaha_combined_sfc <- st_crop(omaha_raw_sfc, omaha_keep_bb) |> 
  st_collection_extract("POLYGON") |> 
  st_union() |> 
  st_make_valid()

# Transform raw sfc object into sf object
omaha_geom <- st_sf(geometry = omaha_combined_sfc)

mapview(omaha_geom)

# Separate Omaha geom into legislative districts
legis_omaha <- ne_legis |> 
  filter(District %in% omaha_small) |> 
  st_intersection(omaha_geom) |> 
  st_make_valid()

mapview(legis_omaha)

# Base Maps of Three Legislative Areas -----------------------------------------
base_ne <- ggplot() +
  geom_sf(data = ne_county,
          fill = NA,
          color = "gray") +
  geom_sf(data = ne_legis,
          fill = NA) +
  theme_void()

base_lincoln <- ggplot() +
  geom_sf(data = legis_lincoln) +
  theme_void()

base_omaha <- ggplot() +
  geom_sf(data = legis_omaha) +
  theme_void()

# Coordinate Locations for LD Labels -------------------------------------------
## ----- Map Centroid Coordinates ----------------------------------------------
legis_centroids <- st_centroid(ne_legis) |> 
  arrange(District)

base_ne +
  geom_sf_text(data = legis_centroids,
               aes(label = District))

centroid_coords <- st_coordinates(legis_centroids) |> 
  as_tibble() |> 
  mutate(District = factor(1:49), .before = X) |> 
  rename(long = X, lat = Y) 

## ----- Map Greater Nebraska Label Coordinates --------------------------------
### All Greater NE LDs Except LD35
update_greaterne_coords <- tribble(
  ~District, ~new_lat,         ~new_long,
  factor(37), 40.691245908447,  -98.94431182252877,
  factor(32), 40.34908654714287, -97.37321150216337,
  factor(41), 41.39190713918393, -98.75752543133243
)

lincoln_small_labels <- lincoln_small[!lincoln_small %in% c(2, 21, 30, 32)]
omaha_small_labels <- omaha_small[!omaha_small %in% c(15, 23, 36)]

revised_greaterne_label_geom <- centroid_coords |> 
  left_join(update_greaterne_coords, by = "District") |>
  mutate(
    lat = coalesce(new_lat, lat),
    long = coalesce(new_long, long)
  ) |>
  select(-new_lat, -new_long) |> 
  st_as_sf(coords = c("long", "lat"),
           crs = st_crs(ne_legis)) |> 
  filter(!District %in% c(35, lincoln_small_labels, omaha_small_labels)) |> 
  mutate(District = as.integer(District))

greaterne_label_geom <- revised_greaterne_label_geom

### LD35 

# Geom segment from 41.025592946670585, -98.40731905940301
# to 35 centroid 40.93844, -98.33107	

connector_ld35 <- tibble(
  District = factor(35),
  y_start = 41.025592946670585,
  x_start = -98.40731905940301,
  y_end = 40.93844,
  x_end = -98.33107
)

label_ld35_geom <- tibble(
  District = 35,
  lat = 41.11721805442036, 
  long = -98.53846834495387 ) |> 
  st_as_sf(coords = c("long", "lat"),
           crs = st_crs(ne_legis)) |> 
  mutate(District = as.integer(District))

base_ne +
  geom_sf_text(data = revised_greaterne_label_geom,
               aes(label = District)) +
  geom_segment(data = connector_ld35,
               aes(x= x_start, y = y_start,
                   xend = x_end, yend = y_end),
               linewidth = 0.75) +
  geom_sf_text(data = label_ld35_geom,
               aes(label = District))

## ----- Map Lincoln Label Coordinates -----------------------------------------
update_lincoln_coords <- tribble(
  ~District, ~new_lat,            ~new_long,
  factor(21), 40.9095226827732,   -96.75818149912358,
  factor(2),  40.832294899253846, -96.53897023779865,
  factor(25), 40.74141665555281,  -96.5457117182025,
  factor(30), 40.659807358358584, -96.60633311482843,
  factor(32), 40.669392405794554, -96.83311954339106,
  factor(28), 40.80796456819948, -96.64067698886433
)

revised_lincoln_label_geom <- centroid_coords |> 
  left_join(update_lincoln_coords, by = "District") |>
  mutate(
    lat = coalesce(new_lat, lat),
    long = coalesce(new_long, long)
  ) |>
  select(-new_lat, -new_long) |> 
  st_as_sf(coords = c("long", "lat"),
           crs = st_crs(ne_legis)) |> 
  filter(District %in% c(2, 21, 30, 32, lincoln_small_labels)) |> 
  mutate(District = as.integer(District))

lincoln_label_geom <- revised_lincoln_label_geom

base_lincoln +
  geom_sf_text(data = lincoln_label_geom,
               aes(label = District))

mapview(lincoln_label_geom)

## ----- Map Omaha Label Coordinates -------------------------------------------
update_omaha_coords <- tribble(
  ~District, ~new_lat,   ~new_long,
  factor(15), 41.34658,   -96.33435,
  factor(23), 41.18960,   -96.41671
)

revised_omaha_label_geom <- centroid_coords |> 
  left_join(update_omaha_coords, by = "District") |>
  mutate(
    lat = coalesce(new_lat, lat),
    long = coalesce(new_long, long)
  ) |>
  select(-new_lat, -new_long) |>
  st_as_sf(coords = c("long", "lat"),
           crs = st_crs(ne_legis)) |> 
  filter(District %in% c(15, 23, 36, omaha_small_labels)) |> 
  mutate(District = as.integer(District))

omaha_label_geom <- revised_omaha_label_geom

base_omaha +
  geom_sf_text(data = omaha_label_geom,
               aes(label = District))

mapview(omaha_label_geom) + mapview(legis_omaha)

# Save Shape Files -------------------------------------------------------------

save(legis_lincoln, legis_omaha, ne_legis, ne_county,
     greaterne_label_geom, lincoln_label_geom,
     omaha_label_geom, connector_ld35, label_ld35_geom,
     file = "./data/legis_objects.rda")
