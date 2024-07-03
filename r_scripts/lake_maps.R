library(sf)
library(leaflet)
library(leaflet.providers)
library(ggmap)
library(ggspatial)
library(magick)
library(raster)
library(png)


#google maps API key
#AIzaSyDcbYDYdqnznWxAD_Du2nD7n5MMatd-Hfk


#data paths
rec_data_path = "./data/lake_coords/reciever_and_habitat_locations/"
image_path = "./images/drone_photos/"
polygon_path = "./data/lake_coords/"
save_plot_path = "./images/traces/"

#API key for google maps
google_maps_api_key <- "AIzaSyDcbYDYdqnznWxAD_Du2nD7n5MMatd-Hfk"
register_google(key = google_maps_api_key)

#receiver and habitat locations
BT_rec_locs_kml <- paste0(rec_data_path, "BT_rec_hab_locations.kml")
BT_rec_locs <- st_read(BT_rec_locs_kml)[1:4,]
BT_hab_locs <- st_read(BT_rec_locs_kml)[5:8,]

#BT polygon
BT_poly_epsg32634 <- sf::st_read(paste0(polygon_path, "BT_poly_epsg32634.gpkg"))
#check crs
st_crs(BT_poly_epsg32634)

#load BT drone image
BT_drone_img <- readPNG(paste0(image_path, "Lake BT_drone_1_ang.png"))

# Get the bounding box of the polygon to set the axis limits
bbox <- st_bbox(BT_poly_epsg32634)

# Create the base ggplot
lake_BT_trace_full <- 
  ggplot() +
  geom_sf(data = BT_poly_epsg32634, fill = NA, color = "black", lwd = 1) +
  geom_sf(data = BT_rec_locs, aes(color = "red", size = 3)) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                          pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                          style = north_arrow_fancy_orienteering) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'none'
  )

print(lake_BT_trace_full)

#save image
ggsave(filename = paste0(save_plot_path, "lake_BT_trace_full.pdf"), plot = lake_BT_trace_full, height = 20, width = 20, unit = 'cm', dpi = 300)

# Create the lake outline and receiver locations plot separately
lake_BT_trace <- lake_BT_trace_full +
  annotation_scale(location = "none") +
  annotation_north_arrow(location = "none")

print(lake_BT_trace)

# Create the plot for the north compass separately
p_compass <- ggplot() +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme_void() +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))

print(p_compass)

ggsave(filename = paste0(save_plot_path, "compass.png"), plot = p_compass, height = 20, width = 20, unit = 'cm', dpi = 300)

# # Get the map within the bounding box
# map <- get_map(location = c(lon = mean(c(bbox["xmin"], bbox["xmax"])), 
#                             lat = mean(c(bbox["ymin"], bbox["ymax"]))), 
#                zoom = 22, 
#                maptype = "satellite", 
#                source = "google")
# 
# ggmap(map) +
#   geom_sf(data = locs, inherit.aes = FALSE, color = "red", size = 2) +
#   theme_minimal()
# # print(locs)


# leaflet(data = locs) %>%
#   # Add default tiles to avoid gray background initially
#   addProviderTiles(providers$CartoDB.Positron) %>%  
#   addMarkers(~st_coordinates(geometry)[,1], ~st_coordinates(geometry)[,2], 
#              popup = ~Name) %>%  
#   addProviderTiles("https://mt1.google.com/vt/lyrs=s&x={x}&y={y}&z={z}&key=AIzaSyDcbYDYdqnznWxAD_Du2nD7n5MMatd-Hfk", 
#                    options = providerTileOptions(api_key = google_maps_api_key)) %>%
#   setView(lng = mean(st_coordinates(locs)[,1]), lat = mean(st_coordinates(locs)[,2]), zoom = 12)


BT_lake_image 







