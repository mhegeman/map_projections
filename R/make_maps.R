# Install and load required packages if not already installed
if (!require(sf)) install.packages("sf")
if (!require(rnaturalearth)) install.packages("rnaturalearth")
if (!require(rnaturalearthdata)) install.packages("rnaturalearthdata")
if (!require(ggplot2)) install.packages("ggplot2")

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)

# Get world map data at medium resolution
world <- ne_countries(scale = "medium", returnclass = "sf")

# Calculate North America bbox for centering
north_america <- world[world$continent == "North America", ]
na_bbox <- st_bbox(north_america)

# Create ocean background with global extent
ocean_polygon <- st_polygon(list(rbind(
  c(-180, -90),
  c(180, -90),
  c(180, 90),
  c(-180, 90),
  c(-180, -90)
))) %>%
  st_sfc(crs = 4326) %>%
  st_sf()

# Function to create map with different projections and improved aesthetics
create_projection_map <- function(projection, title) {
  # Transform both the world and ocean data to the target projection
  world_transformed <- st_transform(world, crs = projection)
  ocean_transformed <- st_transform(ocean_polygon, crs = projection)
  na_transformed <- st_transform(north_america, crs = projection)

  # Get the bounding box of transformed North America and expand it slightly
  bbox_transformed <- st_bbox(na_transformed)
  bbox_width <- bbox_transformed[3] - bbox_transformed[1]
  bbox_height <- bbox_transformed[4] - bbox_transformed[2]

  # Expand bbox by 20%
  plot_bbox <- c(
    bbox_transformed[1] - 0.2 * bbox_width,
    bbox_transformed[2] - 0.2 * bbox_height,
    bbox_transformed[3] + 0.2 * bbox_width,
    bbox_transformed[4] + 0.2 * bbox_height
  )

  # For conical projections, we'll use a different approach
  is_conic <- grepl("aea|lcc", projection)

  if (is_conic) {
    ggplot() +
      geom_sf(data = world_transformed, fill = "#90CA91", color = "#666666", linewidth = 0.2) +
      coord_sf(crs = projection,
               xlim = plot_bbox[c(1,3)],
               ylim = plot_bbox[c(2,4)]) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "#B3E0F2", color = NA),
        panel.background = element_rect(fill = "#B3E0F2", color = NA),
        panel.grid.major = element_line(color = "#cccccc", linewidth = 0.2),
        panel.grid.minor = element_line(color = "#cccccc", linewidth = 0.1),
        axis.text = element_text(size = 8),
        plot.margin = margin(10, 10, 10, 10)
      ) +
      ggtitle(title)
  } else {
    ggplot() +
      geom_sf(data = ocean_transformed, fill = "#B3E0F2", color = NA) +
      geom_sf(data = world_transformed, fill = "#90CA91", color = "#666666", linewidth = 0.2) +
      coord_sf(crs = projection,
               xlim = plot_bbox[c(1,3)],
               ylim = plot_bbox[c(2,4)]) +
      ggtitle(title) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "#cccccc", linewidth = 0.2),
        panel.grid.minor = element_line(color = "#cccccc", linewidth = 0.1),
        axis.text = element_text(size = 8),
        plot.margin = margin(10, 10, 10, 10)
      )
  }
}

# List of projections with their descriptions:

# 1. Albers Equal-Area Conic Projection (Optimized for North America)
albers <- create_projection_map(
  "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
  "Albers Equal-Area Conic Projection\nStandard projection for North America")
ggsave("albers_na.png", albers, width = 12, height = 8, dpi = 300)

# 2. Lambert Conformal Conic (Common for North America)
lambert_cc <- create_projection_map(
  "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
  "Lambert Conformal Conic\nPreserves shape, good for mid-latitudes")
ggsave("lambert_cc_na.png", lambert_cc, width = 12, height = 8, dpi = 300)

# 3. Mercator Projection (EPSG:3857)
mercator <- create_projection_map(
  3857,
  "Mercator Projection\nCommon web mapping projection")
ggsave("mercator_na.png", mercator, width = 12, height = 8, dpi = 300)

# 4. UTM Zone for Central North America (EPSG:32614)
utm <- create_projection_map(
  "+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs",
  "UTM Zone 14N\nConformal projection for local mapping")
ggsave("utm_na.png", utm, width = 12, height = 8, dpi = 300)

# 5. Plate Carrée (Equirectangular) Projection
plate_carree <- create_projection_map(
  "+proj=longlat +datum=WGS84 +lon_0=-96 +no_defs",  # Centered on North America
  "Plate Carrée Projection\nSimple latitude-longitude grid")
ggsave("plate_carree_na.png", plate_carree, width = 12, height = 8, dpi = 300)

# 6. Lambert Azimuthal Equal-Area (Centered on North America)
lambert_ae <- create_projection_map(
  "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
  "Lambert Azimuthal Equal-Area\nEqual-area projection centered on North America")
ggsave("lambert_ae_na.png", lambert_ae, width = 12, height = 8, dpi = 300)
