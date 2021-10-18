# functions.R

# results in world_plot
world_plot_f <- function(world) {
  ggplot(world)  +
    geom_sf(
      fill = "white",
      col = 'grey',
      na.rm = TRUE,
      inherit.aes = FALSE
    )  +
    coord_sf(expand = FALSE)
}

# results in ISO
ISO_get_f <- function(cc_data) {
  paste(cc_data[2, 2])
  
}

# results in crop
crop_get_f <- function(cc_data) {
  paste(cc_data[2, 3])
  
}

# results in v_crop
v_crop_get_f <- function(cc_data) {
  st_read(paste0("data/", cc_data[2, 4]))
}

# results in v_crop_extent
v_crop_extent_f <- function(v_crop) {
  extent(v_crop)
}

# results in v_crop_plot
v_crop_plot_f <- function(v_crop, v_crop_extent, ISO, crop, world) {
  ggplot() +
    geom_sf(
      data = world,
      fill = "white",
      col = 'grey',
      na.rm = TRUE,
      inherit.aes = FALSE
    )  +
    coord_sf(expand = FALSE) +
    geom_sf(
      data = v_crop,
      aes(fill = crop_value),
      alpha = 0.5,
      inherit.aes = FALSE
    ) +
    xlim ((v_crop_extent@xmin - 3), (v_crop_extent@xmax) + 3) +
    ylim ((v_crop_extent@ymin - 3), (v_crop_extent@ymax) + 3) +
    labs(fill = paste0("--------------------\n", ISO," ", crop, "\nZones\n--------------------"))
}


# results in v_ISO
v_ISO_get_f <- function(ISO) {
  getData('GADM', country = paste(ISO), level = 0) %>% st_as_sf()
}

# results in v_ISO_extent
v_ISO_extent_f <- function(v_ISO) {
  extent(v_ISO)
}

# results in v_ISO_plot
v_ISO_plot_f <- function(v_ISO, ISO) {
  ggplot() +
    geom_sf(
      data = v_ISO,
      aes(fill = NAME_0),
      col = 'grey',
      na.rm = TRUE,
      inherit.aes = FALSE
    )  +
    labs(
      fill = paste0(
        "--------------------\n",
        ISO,
        "\nExtent\n\n\n\n\n--------------------"
      )
    )
}

# results in r_ind1
r_ind1_get_f <- function(cc_data, ISO, crop) {
  paste0("data/", ISO,"/",crop,"/", cc_data[2, 6]) %>% raster()

}

# results in r_ind1_plot
r_ind1_plot_f <- function(r_ind1, v_ISO, v_ISO_extent, world, ISO, crop) {

gplot(r_ind1, maxpixels=50000) + #this uses gplot from the rastervis package
  geom_tile(aes(fill = value ), alpha = 1) +  
  geom_sf(
    data = world,
    fill = NA,
    col = 'dark grey',
    na.rm = TRUE,
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = v_ISO,
    fill = NA,
    col = 'dark grey',
    na.rm = TRUE,
    inherit.aes = FALSE
  )  +
  scale_fill_gradient(low = "green",
                      high = 'red',
                      na.value = NA) +
  xlim ((v_ISO_extent@xmin - 3), (v_ISO_extent@xmax) + 3) +
  ylim ((v_ISO_extent@ymin - 3), (v_ISO_extent@ymax) + 3) +
  labs(fill = paste0("--------------------\n",
        ISO," ",crop,"\nInd 1\n\n\n\n\n--------------------")) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  coord_sf(expand = FALSE)

}


# results in r_lc_file
r_lc_get_write_f <- function(r_ind1,ISO) {
  raster("D:/repos/climate-smart-agri-sourcing/spatial_data/output/r_crop_mask_COP_rcl.tif") %>% crop(r_ind1) %>% writeRaster(paste0("data/",ISO,"/r_lc.tif"), overwrite = TRUE)
  }

# results in r_lc
r_lc_get_f <- function(ISO, r_lc_file){
  raster(paste0("data/",ISO,"/r_lc.tif"))
}


# results in r_ISO_file
r_ISO_make_write_f  <- function(v_ISO, r_lc, ISO){

  rasterize(v_ISO, r_lc) %>% writeRaster(paste0("data/",ISO,"/r_ISO.tif"), overwrite = TRUE)
}

# results in r_ISO
r_ISO_get_f <- function(ISO, r_ISO_file){
  raster(paste0("data/",ISO,"/r_ISO.tif"))
}


# results in r_lc_ISO_file
r_lc_ISO_make_write_f  <- function(r_ISO, r_lc, ISO){
  
(r_lc * r_ISO) %>% writeRaster(paste0("data/",ISO,"/r_lc_ISO.tif"), overwrite = TRUE)
}

# results in r_lc_ISO
r_lc_ISO_get_f <- function(ISO, r_lc_ISO_file){
  raster(paste0("data/",ISO,"/r_lc_ISO.tif"))
}


# results in r_lc_ISO_plot
r_lc_ISO_plot_f <- function(r_lc_ISO, v_ISO, ISO){

gplot(r_lc_ISO, maxpixels=50000) + #this uses gplot from the rastervis package
  geom_tile(aes(fill = value), alpha = 1) +  
  geom_sf(
    data = v_ISO,
    fill = NA,
    col = 'dark grey',
    na.rm = TRUE,
    inherit.aes = FALSE
  )  +
  scale_fill_gradient(high = "green",
                      low = 'red',
                      na.value = NA) +
  labs(fill = paste0("--------------------\n",
        ISO,"\nLand Cover\n\n\n\n\n--------------------")) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  coord_sf(expand = FALSE)

}

# results in r_crop_file
r_crop_make_write_f  <- function(v_crop, r_lc_ISO, ISO, crop){

  raster::rasterize(v_crop, r_lc_ISO, field=1, background = 0) %>% writeRaster(paste0("data/",ISO,"/", crop,"/r_crop.tif"), overwrite = TRUE)
}

# results in r_crop
r_crop_get_f <- function(r_crop_file, ISO, crop){

  raster(paste0("data/",ISO,"/", crop,"/r_crop.tif"))
}


# results in r_crop_plot
r_crop_plot_f <- function(r_crop, v_ISO, ISO, crop){

  gplot(r_crop, maxpixels=50000) + #this uses gplot from the rastervis package
  geom_tile(aes(fill = value), alpha = 1) +  
  geom_sf(
    data = v_ISO,
    fill = NA,
    col = 'dark grey',
    na.rm = TRUE,
    inherit.aes = FALSE
  )  +
  scale_fill_gradient(high = "green",
                      low = 'red',
                      na.value = NA) +
  labs(fill = paste0("--------------------\n",
        ISO," ", crop, "\n\n\n\n\n\n--------------------")) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  coord_sf(expand = FALSE)

}


# results in r_crop_ISO_file
r_crop_ISO_make_write_f <- function(r_crop, r_ISO, ISO, crop){

  (r_crop * r_ISO) %>% writeRaster(paste0("data/",ISO,"/", crop,"/r_crop_ISO.tif"), overwrite = TRUE)
}


# results in r_crop_ISO
r_crop_ISO_get_f <- function(r_crop_ISO_file, ISO, crop){
  raster(paste0("data/",ISO,"/", crop,"/r_crop_ISO.tif"))
}


# results in r_crop_ISO_plot
r_crop_ISO_plot_f <- function(r_crop_ISO, v_ISO, ISO, crop){

  gplot(r_crop_ISO, maxpixels=50000) + #this uses gplot from the rastervis package
  geom_tile(aes(fill = value), alpha = 1) +  
  geom_sf(
    data = v_ISO,
    fill = NA,
    col = 'dark grey',
    na.rm = TRUE,
    inherit.aes = FALSE
  )  +
  scale_fill_gradient(high = "green",
                      low = 'red',
                      na.value = NA) +
  labs(fill = paste0("--------------------\n",
        ISO," ", crop, "\n\n\n\n\n\n--------------------")) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  coord_sf(expand = FALSE)

}


# results in r_crop_ISO_lc_file
r_crop_ISO_lc_make_write_f <- function(r_lc_ISO, r_crop_ISO, ISO, crop){

 ( r_lc_ISO + (r_crop_ISO  * 10)) %>% writeRaster(paste0("data/",ISO,"/", crop,"/r_crop_ISO_lc.tif"), overwrite = TRUE)
}

# results in r_crop_ISO_lc
r_crop_ISO_lc_get_f <- function(r_crop_ISO_lc_file, ISO, crop){
  raster(paste0("data/",ISO,"/", crop,"/r_crop_ISO_lc.tif"))
}

# results in r_crop_ISO_lc_plot
r_crop_ISO_lc_plot_f <- function(r_crop_ISO_lc, v_ISO, ISO, crop){

  gplot(r_crop_ISO_lc, maxpixels=50000) + #this uses gplot from the rastervis package
  geom_tile(aes(fill = value), alpha = 1) +  
  geom_sf(
    data = v_ISO,
    fill = NA,
    col = 'dark grey',
    na.rm = TRUE,
    inherit.aes = FALSE
  )  +
  scale_fill_gradient(high = "green",
                      low = 'red',
                      na.value = NA) +
  labs(fill = paste0("--------------------\n",
        ISO," ", crop, "\n\n\n\n\n\n--------------------")) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  coord_sf(expand = FALSE)

}


# results in r_crop_ISO_lc_rcl_file
r_crop_ISO_lc_rcl_make_write_f <- function(r_crop_ISO_lc, ISO, crop){

reclassify(r_crop_ISO_lc, matrix(c(-0.5, 0.5, 0,
           0.5, 1.5, 1,
           1.5, 2.5, 2,
           2.5, 3.5, 3,
           3.5, 10.5, 0,
           10.5, 11.5, 1,
           11.5, 12.5, 2,
           12.5, 13.5, 4), ncol=3, byrow=TRUE), filename = paste0("data/",ISO,"/", crop,"/r_crop_ISO_lc_rcl.tif"), overwrite = TRUE)
}

# results in r_crop_ISO_lc_rcl
r_crop_ISO_lc_rcl_get_f <- function(r_crop_ISO_lc_rcl_file, ISO, crop){
  raster(paste0("data/",ISO,"/", crop,"/r_crop_ISO_lc_rcl.tif"))
}

# results in r_crop_ISO_lc_rcl_plot
r_crop_ISO_lc_rcl_plot_f <- function(r_crop_ISO_lc_rcl, v_ISO, ISO, crop){

  gplot(r_crop_ISO_lc_rcl, maxpixels=50000) + #this uses gplot from the rastervis package
  geom_tile(aes(fill = value), alpha = 1) +  
  geom_sf(
    data = v_ISO,
    fill = NA,
    col = 'dark grey',
    na.rm = TRUE,
    inherit.aes = FALSE
  )  +
  scale_fill_gradient(high = "green",
                      low = 'red',
                      na.value = NA) +
  labs(fill = paste0("--------------------\n",
        ISO," ", crop, "\n\n\n\n\n\n--------------------")) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  coord_sf(expand = FALSE)

}

# results in r_crop_ISO_lc_rcl_agg_file
r_crop_ISO_lc_rcl_agg_make_write_f <- function(r_crop_ISO_lc_rcl, ISO, crop){

aggregate(r_crop_ISO_lc_rcl, 10, fun = "modal") %>% writeRaster(paste0("data/",ISO,"/", crop,"/r_crop_ISO_lc_rcl_agg.tif"), overwrite = TRUE)

}

# results in r_crop_ISO_lc_rcl_agg
r_crop_ISO_lc_rcl_agg_get_f <- function(r_crop_ISO_lc_rcl_agg_file, ISO, crop){
  raster(paste0("data/",ISO,"/", crop,"/r_crop_ISO_lc_rcl_agg.tif"))
}

# results in v_crop_ISO_lc_rcl_agg_file
v_crop_ISO_lc_rcl_agg_make_write_f <- function(r_crop_ISO_lc_rcl_agg, ISO, crop){

rasterToPolygons(r_crop_ISO_lc_rcl_agg, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=TRUE) %>% st_as_sf %>% write_sf(paste0("data/",ISO,"/", crop,"/v_crop_ISO_lc_rcl_agg.shp"), overwrite = TRUE)

}

# results in v_crop_ISO_lc_rcl_agg
v_crop_ISO_lc_rcl_agg_get_f <- function(v_crop_ISO_lc_rcl_agg_file, ISO, crop){
  st_read(paste0("data/",ISO,"/", crop,"/v_crop_ISO_lc_rcl_agg.shp"))
}

# results in dB_ind1_summary
dB_ind1_summary_make <- function(r_ind1, v_crop_ISO_lc_rcl_agg){

exact_extract(r_ind1, v_crop_ISO_lc_rcl_agg, fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'), quantiles = c(0.25, 0.75))
}

# results in dB_sourcing_stats
dB_sourcing_stats_make <- function(v_crop_ISO_lc_rcl_agg, dB_ind1_summary){

tibble(v_crop_ISO_lc_rcl_agg$r__ISO_) %>% `names<-`(c("Landuse")) %>% 
mutate(Landuse_names = factor(Landuse, labels = c("No Vegetation", "Pastures/Shrubland", "Woods and Forests", "Cropland", "Cotton" ))) %>% 
cbind(dB_ind1_summary) %>% 
mutate(IQR = (q75 - q25))  %>% 
mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
mutate(max_min = pmax(min, l_whisker)) %>%
mutate(min_max = pmin(max, u_whisker))
}

# results in dB_sourcing_stats_plot
dB_sourcing_stats_plot_f <- function(dB_sourcing_stats, ISO, crop){

ggplot(dB_sourcing_stats,
       aes(Landuse, group = Landuse, fill = Landuse_names,)) +
  geom_boxplot(
    aes(
      ymin = max_min,
      lower = q25,
      middle = median,
      upper = q75,
      ymax = min_max,
      width = count
    ),
    stat = "identity"
  ) +
  scale_fill_manual(values = c("white", "orange", "yellow", "green", "dark green")) +
  scale_y_continuous(labels = comma) +
  labs(y = "# of seasons") +
  labs(x = "Sourcing Area")  +
  labs(fill = paste0("--------------------\nCurrent\nLong Rains\n",
        ISO," ", crop, "\nFreq. of 7\nConsecutive\nDry Days\n--------------------")) +
    ylim(0, 30) +
  geom_hline(
    yintercept = 10,
    linetype = 'dashed',
    colour = 'red',
    size = 1
  ) +
  geom_text(
    x = 0,
    y = 11,
    label = "Risk Threshold",
    colour = 'red'
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
}
