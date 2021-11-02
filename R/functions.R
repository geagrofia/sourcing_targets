# functions.R

### ----- Global spatial data ----- ###

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

# results in r_lc_plot
r_lc_plot_f <- function(r_lc, world) {
    gplot(r_lc, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = value), alpha = 1) +
      geom_sf(
        data = world,
        fill = NA,
        col = 'dark grey',
        na.rm = TRUE,
        inherit.aes = FALSE
      ) +
      scale_fill_gradient(low = "white",
                          high = 'dark green',
                          na.value = NA) +
      xlim(-20, 60) +
      ylim(-40, 40) +
      labs(fill = "--------------------\nLand Cover\nAfrica\n
        \n\n\n--------------------") +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )
}

### ----- Static raw data ----- ###

# results in cc_data_kable
cc_data_kable_make_f <- function(cc_data) {
  cc_data %>% kable() %>%
kable_styling(latex_options = c("striped", "scale_down"))
}

 # results in ISO
ISO_get_f <- function(cc_data, cc_row) {
   paste(cc_data[cc_row, 2])
}

# results in crop
crop_get_f <- function(cc_data, cc_row) {
  paste(cc_data[cc_row, 3])
}

# results in drought
drought_get_f <- function(cc_data, cc_row) {
  paste(cc_data[cc_row, 5])
}

# results in heat
heat_get_f <- function(cc_data, cc_row) {
  paste(cc_data[cc_row, 8])
}

# results in flood
flood_get_f <- function(cc_data, cc_row) {
  paste(cc_data[cc_row, 11])
}

### ----- Country spatial data ----- ###

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
    labs(fill = paste0(
      "--------------------\n",
      ISO,
      " ",
      crop,
      "\nZones\n--------------------"
    ))
}


# results in v_ISO
v_ISO_get_f <- function(ISO) {
  getData('GADM', country = paste(ISO), level = 0) %>% st_as_sf()
}

# results in v_ISO1
v_ISO1_get_f <- function(ISO) {
  getData('GADM', country = paste(ISO), level = 1) %>% st_as_sf()
}

# results in v_ISO_extent
v_ISO_extent_f <- function(v_ISO) {
  extent(v_ISO)
}

# results in v_ISO_plot
v_ISO_plot_f <- function(v_ISO1, ISO) {
  ggplot() +
    geom_sf(
      data = v_ISO1,
      aes(fill = NAME_0),
      col = 'black',
      na.rm = TRUE,
      inherit.aes = FALSE
    )  +
    labs(fill = paste0(
      "--------------------\n",
      ISO,
      "\nExtent\n\n\n\n\n--------------------"
    ))
}

# results in r_clim_mask
r_clim_mask_get_f <- function(ISO) {
  paste0("data/", ISO, "/clim_mask.tif") %>% raster()
}


### ----- Spatial data preparation ----- ###

# results in r_ISO_file
r_ISO_make_write_f  <- function(v_ISO, r_lc, v_ISO_extent, ISO) {
  rasterize(v_ISO, r_lc) %>% crop(v_ISO_extent) %>% writeRaster(paste0("data/", ISO, "/r_ISO.tif"), overwrite = TRUE)
}

# results in r_ISO
r_ISO_get_f <- function(ISO, r_ISO_file) {
  raster(paste0("data/", ISO, "/r_ISO.tif"))
}

# results in r_ISO1_file
r_ISO1_make_write_f  <- function(v_ISO1, r_ISO, ISO) {
  rasterize(v_ISO1, r_ISO) %>% writeRaster(paste0("data/", ISO, "/r_ISO1.tif"), overwrite = TRUE)
}

# results in r_ISO1
r_ISO1_get_f <- function(ISO, r_ISO1_file) {
  raster(paste0("data/", ISO, "/r_ISO1.tif"))
}

# results in r_lc_ISO_file
r_lc_ISO_make_write_f  <- function(r_ISO, r_lc, ISO) {
  (r_lc * r_ISO) %>% writeRaster(paste0("data/", ISO, "/r_lc_ISO.tif"), overwrite = TRUE)
}

# results in r_lc_ISO
r_lc_ISO_get_f <- function(ISO, r_lc_ISO_file) {
  raster(paste0("data/", ISO, "/r_lc_ISO.tif"))
}

# results in r_lc_ISO_plot
r_lc_ISO_plot_f <- function(r_lc_ISO, world, v_ISO_extent, ISO) {
    gplot(r_lc_ISO, maxpixels = 500000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = value), alpha = 1) +
      geom_sf(
        data = world,
        fill = NA,
        col = 'dark grey',
        na.rm = TRUE,
        inherit.aes = FALSE
      ) +
      scale_fill_gradient(low = "white",
                          high = 'dark green',
                          na.value = NA) +
      xlim((v_ISO_extent@xmin -1), (v_ISO_extent@xmax +1)) +
      ylim((v_ISO_extent@ymin -1), (v_ISO_extent@ymax +1)) +
      labs(fill = paste0("-------------------\nLand Cover\n",ISO ,"\n
        \n\n\n--------------------")) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )
}  

  # results in r_crop_file
r_crop_make_write_f  <- function(v_crop, r_lc_ISO, ISO, crop) {
  raster::rasterize(v_crop, r_lc_ISO, field = 1, background = 0) %>% writeRaster(paste0("data/", ISO, "/", crop, "/r_crop.tif"), overwrite = TRUE)
}

# results in r_crop
r_crop_get_f <- function(r_crop_file, ISO, crop) {
  raster(paste0("data/", ISO, "/", crop, "/r_crop.tif"))
}

  # results in r_crop_ISO_file
r_crop_ISO_make_write_f <- function(r_crop, r_ISO, ISO, crop) {
  (r_crop * r_ISO) %>% writeRaster(paste0("data/", ISO, "/", crop, "/r_crop_ISO.tif"),
                                   overwrite = TRUE)
}

# results in r_crop_ISO
r_crop_ISO_get_f <- function(r_crop_ISO_file, ISO, crop) {
  raster(paste0("data/", ISO, "/", crop, "/r_crop_ISO.tif"))
}

# results in r_crop_ISO_plot
r_crop_ISO_plot_f <- function(r_crop_ISO, v_ISO1, ISO, crop) {
  gplot(r_crop_ISO, maxpixels = 50000) + #this uses gplot from the rastervis package
    geom_tile(aes(fill = value), alpha = 1) +
    geom_sf(
      data = v_ISO1,
      fill = NA,
      col = 'black',
      na.rm = TRUE,
      inherit.aes = FALSE
    )  +
    scale_fill_gradient(high = "green",
                        low = 'red',
                        na.value = NA) +
    labs(
      fill = paste0(
        "--------------------\n",
        ISO,
        " ",
        crop,
        "\n\n\n\n\n\n--------------------"
      )
    ) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) +
    coord_sf(expand = FALSE)
}

# results in r_crop_ISO_lc_file
r_crop_ISO_lc_file <-
  function(r_lc_ISO, r_crop_ISO, ISO, crop) {
    (r_lc_ISO + (r_crop_ISO  * 10)) %>% writeRaster(paste0("data/", ISO, "/", crop, "/r_crop_ISO_lc.tif"),
                                                    overwrite = TRUE)
  }

# results in r_crop_ISO_lc
r_crop_ISO_lc_get_f <- function(r_crop_ISO_lc_file, ISO, crop) {
  raster(paste0("data/", ISO, "/", crop, "/r_crop_ISO_lc.tif"))
}

# results in r_crop_ISO_lc_plot
r_crop_ISO_lc_plot_f <- function(r_crop_ISO_lc, v_ISO1, ISO, crop) {
  gplot(r_crop_ISO_lc, maxpixels = 50000) + #this uses gplot from the rastervis package
    geom_tile(aes(fill = value), alpha = 1) +
    geom_sf(
      data = v_ISO1,
      fill = NA,
      col = 'black',
      na.rm = TRUE,
      inherit.aes = FALSE
    )  +
    scale_fill_gradient(high = "green",
                        low = 'red',
                        na.value = NA) +
    labs(
      fill = paste0(
        "--------------------\n",
        ISO,
        " ",
        crop,
        "\n\n\n\n\n\n--------------------"
      )
    ) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) +
    coord_sf(expand = FALSE)
}


# results in r_crop_ISO_lc_rcl_file
r_crop_ISO_lc_rcl_make_write_f <-
  function(r_crop_ISO_lc, ISO, crop) {
    reclassify(
      r_crop_ISO_lc,
      matrix(
        c(
          -0.5, 0.5, 0,
          0.5, 1.5, 0,
          1.5, 2.5, 0,
          2.5, 3.5, 1,
          3.5, 10.5, 0,
          10.5, 11.5, 0,
          11.5, 12.5, 0,
          12.5, 13.5, 2
        ),
        ncol = 3,
        byrow = TRUE
      ),
      filename = paste0("data/", ISO, "/", crop, "/r_crop_ISO_lc_rcl.tif"),
      overwrite = TRUE
    )
  }

# results in r_crop_ISO_lc_rcl
r_crop_ISO_lc_rcl_get_f <-
  function(r_crop_ISO_lc_rcl_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/r_crop_ISO_lc_rcl.tif"))
  }

# results in r_crop_ISO_lc_rcl_plot
r_crop_ISO_lc_rcl_plot_f <-
  function(r_crop_ISO_lc_rcl, v_ISO1, ISO, crop) {
    gplot(r_crop_ISO_lc_rcl, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = value), alpha = 1) +
      geom_sf(
        data = v_ISO1,
        fill = NA,
        col = 'black',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_gradient(high = "green",
                          low = 'red',
                          na.value = NA) +
      labs(
        fill = paste0(
          "--------------------\n",
          ISO,
          " ",
          crop,
          "\n\n\n\n\n\n--------------------"
        )
      ) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) +
      coord_sf(expand = FALSE)
  }


### ----- Aggregate the crop/landuse raster and convert to vector----- ###


  # results in r_crop_ISO_lc_rcl_agg_file
r_crop_ISO_lc_rcl_agg_make_write_f <-
  function(r_crop_ISO_lc_rcl, ISO, crop) {
    aggregate(r_crop_ISO_lc_rcl, 10, fun = "modal") %>% writeRaster(paste0("data/", ISO, "/", crop, "/r_crop_ISO_lc_rcl_agg.tif"),
                                                                    overwrite = TRUE)
  }

# results in r_crop_ISO_lc_rcl_agg
r_crop_ISO_lc_rcl_agg_get_f <-
  function(r_crop_ISO_lc_rcl_agg_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/r_crop_ISO_lc_rcl_agg.tif"))
  }

# results in v_crop_ISO_lc_rcl_agg_file
v_crop_ISO_lc_rcl_agg_make_write_f <-
  function(r_crop_ISO_lc_rcl_agg,  ISO, crop) {
    rasterToPolygons(
      r_crop_ISO_lc_rcl_agg,
      fun = NULL,
      n = 4,
      na.rm = TRUE,
      digits = 12,
      dissolve = TRUE
    ) %>% 
     st_as_sf %>% 
     write_sf(paste0("data/", ISO, "/", crop, "/v_crop_ISO_lc_rcl_agg.shp"), overwrite = TRUE)
  }

# results in v_crop_ISO_lc_rcl_agg
v_crop_ISO_lc_rcl_agg_get_f <-
   function(v_crop_ISO_lc_rcl_agg_file, ISO, crop, v_ISO1) {   
st_read(paste0("data/", ISO, "/", crop, "/v_crop_ISO_lc_rcl_agg.shp")) %>%
mutate(LC = factor(r__ISO_,
labels = c("Not Cropland","Cropland","Cotton")
)) %>%
filter(r__ISO_ > 0) %>%
st_intersection(v_ISO1) %>%
mutate(crop_ISO1 = paste(NAME_1, LC, sep = '_'))
}


### ----- Trimmed Climate Mask ----- ###

# results in r_clim_mask_trim_file
r_clim_mask_trim_file_make_write_f <- function(r_lc_ISO, r_clim_mask, ISO) {
aggregate((r_lc_ISO / r_lc_ISO), (res(r_clim_mask)/res(r_lc_ISO)), fun=modal) %>%
resample(r_clim_mask, method="ngb",
  filename = paste0("data/", ISO, "/r_clim_mask_trim.tif"), overwrite = TRUE)
}

# results in r_clim_mask_trim
r_clim_mask_trim_get_f <- function(ISO, r_clim_mask_trim_file) {
  raster(paste0("data/", ISO, "/r_clim_mask_trim.tif"))
}


### ----- Climate Base Data ----- ###

### --- Rainfall --- 

### Load original data trim and save and get

# results in r_rainfallc_file
r_rainfallc_make_write_f <- function(ISO, crop, cc_data, r_clim_mask_trim) {
  (raster(paste0("data/", ISO, "/", crop, "/", cc_data[2, 14])) * r_clim_mask_trim) %>% 
  writeRaster(paste0("data/", ISO, "/", crop, "/r_rainfallc.tif"), overwrite = TRUE)
}

# results in r_rainfallc
r_rainfallc_get_f <- function(ISO, crop, r_rainfallc_file) {
  raster(paste0("data/", ISO, "/", crop, "/r_rainfallc.tif"))
}

# results in r_rainfallf_file
r_rainfallf_make_write_f <- function(ISO, crop, cc_data, r_clim_mask_trim) {
  (raster(paste0("data/", ISO, "/", crop, "/", cc_data[2, 15])) * r_clim_mask_trim) %>% 
  writeRaster(paste0("data/", ISO, "/", crop, "/r_rainfallf.tif"), overwrite = TRUE)
}

# results in r_rainfallf
r_rainfallf_get_f <- function(ISO, crop, r_rainfallf_file) {
  raster(paste0("data/", ISO, "/", crop, "/r_rainfallf.tif"))
}

### --- Temperature --- 

### Load original data trim and save and get

# results in r_tempc_file
r_tempc_make_write_f <- function(ISO, crop, cc_data, r_clim_mask_trim) {
  (raster(paste0("data/", ISO, "/", crop, "/", cc_data[2, 16])) * r_clim_mask_trim) %>% 
  writeRaster(paste0("data/", ISO, "/", crop, "/r_tempc.tif"), overwrite = TRUE)
}

# results in r_tempc
r_tempc_get_f <- function(ISO, crop, r_tempc_file) {
  raster(paste0("data/", ISO, "/", crop, "/r_tempc.tif"))
} 

# results in r_tempf_file
r_tempf_make_write_f <- function(ISO, crop, cc_data, r_clim_mask_trim) {
  (raster(paste0("data/", ISO, "/", crop, "/", cc_data[2, 17])) * r_clim_mask_trim) %>% 
  writeRaster(paste0("data/", ISO, "/", crop, "/r_tempf.tif"), overwrite = TRUE)
}

# results in r_tempf
r_tempf_get_f <- function(ISO, crop, r_tempf_file) {
  raster(paste0("data/", ISO, "/", crop, "/r_tempf.tif"))
} 

### --- Season Onset --- 

# results in r_onsetc_file
r_onsetc_make_write_f <- function(ISO, crop, cc_data, r_clim_mask_trim) {
  (raster(paste0("data/", ISO, "/", crop, "/", cc_data[2, 18])) * r_clim_mask_trim) %>% 
  writeRaster(paste0("data/", ISO, "/", crop, "/r_onsetc.tif"), overwrite = TRUE)
}

# results in r_onsetc
r_onsetc_get_f <- function(ISO, crop, r_onsetc_file) {
  raster(paste0("data/", ISO, "/", crop, "/r_onsetc.tif"))
}

# results in r_onsetf_file
r_onsetf_make_write_f <- function(ISO, crop, cc_data, r_clim_mask_trim) {
  (raster(paste0("data/", ISO, "/", crop, "/", cc_data[2, 19])) * r_clim_mask_trim) %>% 
  writeRaster(paste0("data/", ISO, "/", crop, "/r_onsetf.tif"), overwrite = TRUE)
}

# results in r_onsetf
r_onsetf_get_f <- function(ISO, crop, r_onsetf_file) {
  raster(paste0("data/", ISO, "/", crop, "/r_onsetf.tif"))
}

### --- Season Duration --- 

# results in r_durationc_file
r_durationc_make_write_f <- function(ISO, crop, cc_data, r_clim_mask_trim) {
  (raster(paste0("data/", ISO, "/", crop, "/", cc_data[2, 20])) * r_clim_mask_trim) %>% 
  writeRaster(paste0("data/", ISO, "/", crop, "/r_durationc.tif"), overwrite = TRUE)
}

# results in r_durationc
r_durationc_get_f <- function(ISO, crop, r_durationc_file) {
  raster(paste0("data/", ISO, "/", crop, "/r_durationc.tif"))
}

# results in r_durationf_file
r_durationf_make_write_f <- function(ISO, crop, cc_data, r_clim_mask_trim) {
  (raster(paste0("data/", ISO, "/", crop, "/", cc_data[2, 21])) * r_clim_mask_trim) %>% 
  writeRaster(paste0("data/", ISO, "/", crop, "/r_durationf.tif"), overwrite = TRUE)
}

# results in r_durationf
r_durationf_get_f <- function(ISO, crop, r_durationf_file) {
  raster(paste0("data/", ISO, "/", crop, "/r_durationf.tif"))
}

### Calculate changes and save

# results in r_rainfall_change_file
r_rainfall_change_make_write_f <-
  function(r_rainfallc, r_rainfallf, ISO, crop) {
    (r_rainfallf - r_rainfallc) %>% writeRaster(paste0("data/", ISO, "/", crop, "/r_rainfall_change.tif"),
                                              overwrite = TRUE)
  }

# results in r_rainfall_change
r_rainfall_change_get_f <-
  function(r_rainfall_change_file, ISO, crop) {
    paste0("data/", ISO, "/", crop, "/r_rainfall_change.tif") %>% raster()
  }


# results in r_temp_change_file
r_temp_change_make_write_f <-
  function(r_tempc, r_tempf, ISO, crop) {
    (r_tempf - r_tempc) %>% writeRaster(paste0("data/", ISO, "/", crop, "/r_temp_change.tif"),
                                              overwrite = TRUE)
  }

# results in r_temp_change
r_temp_change_get_f <-
  function(r_temp_change_file, ISO, crop) {
    paste0("data/", ISO, "/", crop, "/r_temp_change.tif") %>% raster()
  }

# results in r_onset_change_file
r_onset_change_make_write_f <-
  function(r_onsetc, r_onsetf, ISO, crop) {
    (r_onsetf - r_onsetc) %>% writeRaster(paste0("data/", ISO, "/", crop, "/r_onset_change.tif"),
                                              overwrite = TRUE)
  }

# results in r_onset_change
r_onset_change_get_f <-
  function(r_onset_change_file, ISO, crop) {
    paste0("data/", ISO, "/", crop, "/r_onset_change.tif") %>% raster()
  }


# results in r_duration_change_file
r_duration_change_make_write_f <-
  function(r_durationc, r_durationf, ISO, crop) {
    (r_durationf - r_durationc) %>% writeRaster(paste0("data/", ISO, "/", crop, "/r_duration_change.tif"),
                                              overwrite = TRUE)
  }

# results in r_duration_change
r_duration_change_get_f <-
  function(r_duration_change_file, ISO, crop) {
    paste0("data/", ISO, "/", crop, "/r_duration_change.tif") %>% raster()
  }


### plot climate base data

# results in r_rainfallc_plot
r_rainfallc_plot_f <-
  function(r_rainfallc,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop) {
    gplot(r_rainfallc, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = value), alpha = 1) +
      geom_sf(
        data = world,
        fill = NA,
        col = 'dark grey',
        na.rm = TRUE,
        inherit.aes = FALSE
      ) +
      geom_sf(
        data = v_ISO1,
        fill = NA,
        col = 'black',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_gradient(low = "light blue",
                          high = "blue",
                          na.value = NA) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
    labs(
      fill = paste0(
        "--------------------\nPast\nRainfall\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) +
      coord_sf(expand = FALSE)
  }

# results in r_rainfallf_plot
r_rainfallf_plot_f <-
  function(r_rainfallf,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop) {
    gplot(r_rainfallf, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = value), alpha = 1) +
      geom_sf(
        data = world,
        fill = NA,
        col = 'dark grey',
        na.rm = TRUE,
        inherit.aes = FALSE
      ) +
      geom_sf(
        data = v_ISO1,
        fill = NA,
        col = 'black',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_gradient(low = "light blue",
                          high = "blue",
                          na.value = NA) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
    labs(
      fill = paste0(
        "--------------------\nFuture\nRainfall\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) +
      coord_sf(expand = FALSE)
  }

# results in r_tempc_plot
r_tempc_plot_f <-
  function(r_tempc,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop) {
    gplot(r_tempc, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = value), alpha = 1) +
      geom_sf(
        data = world,
        fill = NA,
        col = 'dark grey',
        na.rm = TRUE,
        inherit.aes = FALSE
      ) +
      geom_sf(
        data = v_ISO1,
        fill = NA,
        col = 'black',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_gradient(low = "yellow",
                          high = "brown",
                          na.value = NA) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
    labs(
      fill = paste0(
        "--------------------\nPast\nMean\nTemperature\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) +
      coord_sf(expand = FALSE)
  }

# results in r_tempf_plot
r_tempf_plot_f <-
  function(r_tempf,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop) {
    gplot(r_tempf, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = value), alpha = 1) +
      geom_sf(
        data = world,
        fill = NA,
        col = 'dark grey',
        na.rm = TRUE,
        inherit.aes = FALSE
      ) +
      geom_sf(
        data = v_ISO1,
        fill = NA,
        col = 'black',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_gradient(low = "yellow",
                          high = "brown",
                          na.value = NA) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
    labs(
      fill = paste0(
        "--------------------\nFuture\nMean\nTemperature\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) +
      coord_sf(expand = FALSE)
  }

# results in r_onsetc_plot
r_onsetc_plot_f <-
  function(r_onsetc,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop) {
    gplot(r_onsetc, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = value), alpha = 1) +
      geom_sf(
        data = world,
        fill = NA,
        col = 'dark grey',
        na.rm = TRUE,
        inherit.aes = FALSE
      ) +
      geom_sf(
        data = v_ISO1,
        fill = NA,
        col = 'black',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_gradient(low = "purple",
                          high = "yellow",
                          na.value = NA) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
    labs(
      fill = paste0(
        "--------------------\nPast\nSeason\nOnset\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) +
      coord_sf(expand = FALSE)
  }

# results in r_onsetf_plot
r_onsetf_plot_f <-
  function(r_onsetf,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop) {
    gplot(r_onsetf, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = value), alpha = 1) +
      geom_sf(
        data = world,
        fill = NA,
        col = 'dark grey',
        na.rm = TRUE,
        inherit.aes = FALSE
      ) +
      geom_sf(
        data = v_ISO1,
        fill = NA,
        col = 'black',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_gradient(low = "purple",
                          high = "yellow",
                          na.value = NA) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
    labs(
      fill = paste0(
        "--------------------\nFuture\nSeason\nOnset\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) +
      coord_sf(expand = FALSE)
  }

# results in r_durationc_plot
r_durationc_plot_f <-
  function(r_durationc,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop) {
    gplot(r_durationc, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = value), alpha = 1) +
      geom_sf(
        data = world,
        fill = NA,
        col = 'dark grey',
        na.rm = TRUE,
        inherit.aes = FALSE
      ) +
      geom_sf(
        data = v_ISO1,
        fill = NA,
        col = 'black',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_gradient(low = "orange",
                          high = "red",
                          na.value = NA) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
    labs(
      fill = paste0(
        "--------------------\nPast\nSeason\nDuration\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) +
      coord_sf(expand = FALSE)
  }

# results in r_durationf_plot
r_durationf_plot_f <-
  function(r_durationf,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop) {
    gplot(r_durationf, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = value), alpha = 1) +
      geom_sf(
        data = world,
        fill = NA,
        col = 'dark grey',
        na.rm = TRUE,
        inherit.aes = FALSE
      ) +
      geom_sf(
        data = v_ISO1,
        fill = NA,
        col = 'black',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_gradient(low = "orange",
                          high = "red",
                          na.value = NA) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
    labs(
      fill = paste0(
        "--------------------\nFuture\nSeason\nDuration\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) +
      coord_sf(expand = FALSE)
  }

## climate base data change plots

# results in r_rainfall_change_plot
r_rainfall_change_plot_f <-
  function(r_rainfall_change,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop) {
    gplot(r_rainfall_change, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = value), alpha = 1) +
      geom_sf(
        data = world,
        fill = NA,
        col = 'dark grey',
        na.rm = TRUE,
        inherit.aes = FALSE
      ) +
      geom_sf(
        data = v_ISO1,
        fill = NA,
        col = 'black',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_gradient(low = "yellow",
                          high = "blue",
                          na.value = NA) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
    labs(
      fill = paste0(
        "--------------------\nChange\nRainfall\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) +
      coord_sf(expand = FALSE)
  }

# results in r_temp_change_plot
r_temp_change_plot_f <-
  function(r_temp_change,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop) {
    gplot(r_temp_change, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = value), alpha = 1) +
      geom_sf(
        data = world,
        fill = NA,
        col = 'dark grey',
        na.rm = TRUE,
        inherit.aes = FALSE
      ) +
      geom_sf(
        data = v_ISO1,
        fill = NA,
        col = 'black',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_gradient(low = "blue",
                          high = "red",
                          na.value = NA) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
    labs(
      fill = paste0(
        "--------------------\nChange\nMean\nTemperature\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) +
      coord_sf(expand = FALSE)
  }

# results in r_onset_change_plot
r_onset_change_plot_f <-
  function(r_onset_change,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop) {
    gplot(r_onset_change, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = value), alpha = 1) +
      geom_sf(
        data = world,
        fill = NA,
        col = 'dark grey',
        na.rm = TRUE,
        inherit.aes = FALSE
      ) +
      geom_sf(
        data = v_ISO1,
        fill = NA,
        col = 'black',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_gradient(low = "red",
                          high = "blue",
                          na.value = NA) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
    labs(
      fill = paste0(
        "--------------------\nChange\nSeason\nOnset\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) +
      coord_sf(expand = FALSE)
  }

# results in r_duration_change_plot
r_duration_change_plot_f <-
  function(r_duration_change,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop) {
    gplot(r_duration_change, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = value), alpha = 1) +
      geom_sf(
        data = world,
        fill = NA,
        col = 'dark grey',
        na.rm = TRUE,
        inherit.aes = FALSE
      ) +
      geom_sf(
        data = v_ISO1,
        fill = NA,
        col = 'black',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_gradient(low = "purple",
                          high = "orange",
                          na.value = NA) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
    labs(
      fill = paste0(
        "--------------------\nChange\nSeason\nDuration\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) +
      coord_sf(expand = FALSE)
  }

### ----- Climate Hazard Indicator Data ----- ###
### Load original data trim and save

# results in r_droughtc_file
r_droughtc_make_write_f <- function(ISO, crop, cc_data, r_clim_mask_trim) {
  (raster(paste0("data/", ISO, "/", crop, "/", cc_data[2, 6])) * r_clim_mask_trim) %>% 
  writeRaster(paste0("data/", ISO, "/", crop, "/r_droughtc.tif"), overwrite = TRUE)
}

# results in r_droughtc
r_droughtc_get_f <- function(ISO, crop, r_droughtc_file) {
  paste0("data/", ISO, "/", crop, "/r_droughtc.tif") %>% raster()
}

# results in r_droughtf_file
r_droughtf_make_write_f <- function(ISO, crop, cc_data, r_clim_mask_trim) {
  (raster(paste0("data/", ISO, "/", crop, "/", cc_data[2, 7])) * r_clim_mask_trim) %>% 
  writeRaster(paste0("data/", ISO, "/", crop, "/r_droughtf.tif"), overwrite = TRUE)
}

# results in r_droughtf
r_droughtf_get_f <- function(ISO, crop, r_droughtf_file) {
  paste0("data/", ISO, "/", crop, "/r_droughtf.tif") %>% raster()
}

# results in r_heatc_file
r_heatc_make_write_f <- function(ISO, crop, cc_data, r_clim_mask_trim) {
  (raster(paste0("data/", ISO, "/", crop, "/", cc_data[2, 9])) * r_clim_mask_trim) %>% 
  writeRaster(paste0("data/", ISO, "/", crop, "/r_heatc.tif"), overwrite = TRUE)
}

# results in r_heatc
r_heatc_get_f <- function(ISO, crop, r_heatc_file) {
  paste0("data/", ISO, "/", crop, "/r_heatc.tif") %>% raster()
}

# results in r_heatf_file
r_heatf_make_write_f <- function(ISO, crop, cc_data, r_clim_mask_trim) {
  (raster(paste0("data/", ISO, "/", crop, "/", cc_data[2, 10])) * r_clim_mask_trim) %>% 
  writeRaster(paste0("data/", ISO, "/", crop, "/r_heatf.tif"), overwrite = TRUE)
}

# results in r_heatf
r_heatf_get_f <- function(ISO, crop, r_heatf_file) {
  paste0("data/", ISO, "/", crop, "/r_heatf.tif") %>% raster()
}

# results in r_floodc_file
r_floodc_make_write_f <- function(ISO, crop, cc_data, r_clim_mask_trim) {
  (raster(paste0("data/", ISO, "/", crop, "/", cc_data[2, 12])) * r_clim_mask_trim) %>% 
  writeRaster(paste0("data/", ISO, "/", crop, "/r_floodc.tif"), overwrite = TRUE)
}

# results in r_floodc
r_floodc_get_f <- function(ISO, crop, r_floodc_file) {
  paste0("data/", ISO, "/", crop, "/r_floodc.tif") %>% raster()
}

# results in r_floodf_file
r_floodf_make_write_f <- function(ISO, crop, cc_data, r_clim_mask_trim) {
  (raster(paste0("data/", ISO, "/", crop, "/", cc_data[2, 13])) * r_clim_mask_trim) %>% 
  writeRaster(paste0("data/", ISO, "/", crop, "/r_floodf.tif"), overwrite = TRUE)
}

# results in r_floodf
r_floodf_get_f <- function(ISO, crop, r_floodf_file) {
  paste0("data/", ISO, "/", crop, "/r_floodf.tif") %>% raster()
}

### Calculate changes and save

# results in r_drought_change_file
r_drought_change_make_write_f <-
  function(r_droughtc, r_droughtf, ISO, crop) {
    (r_droughtf - r_droughtc) %>% writeRaster(paste0("data/", ISO, "/", crop, "/r_drought_change.tif"),
                                              overwrite = TRUE)
  }

# results in r_drought_change
r_drought_change_get_f <-
  function(r_drought_change_file, ISO, crop) {
    paste0("data/", ISO, "/", crop, "/r_drought_change.tif") %>% raster()
  }

# results in r_heat_change_file
r_heat_change_make_write_f <-
  function(r_heatc, r_heatf, ISO, crop) {
    (r_heatf - r_heatc) %>% writeRaster(paste0("data/", ISO, "/", crop, "/r_heat_change.tif"),
                                              overwrite = TRUE)
  }

# results in r_heat_change
r_heat_change_get_f <-
  function(r_heat_change_file, ISO, crop) {
    paste0("data/", ISO, "/", crop, "/r_heat_change.tif") %>% raster()    
  }  

# results in r_flood_change_file
r_flood_change_make_write_f <-
  function(r_floodc, r_floodf, ISO, crop) {
    (r_floodf - r_floodc) %>% writeRaster(paste0("data/", ISO, "/", crop, "/r_flood_change.tif"),
                                              overwrite = TRUE)
  }

# results in r_flood_change
r_flood_change_get_f <-
  function(r_flood_change_file, ISO, crop) {
    paste0("data/", ISO, "/", crop, "/r_flood_change.tif") %>% raster()
  }

### plot climate hazard data

# results in r_droughtc_plot
r_droughtc_plot_f <-
  function(r_droughtc,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           drought) {
    gplot(r_droughtc, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = value), alpha = 1) +
      geom_sf(
        data = world,
        fill = NA,
        col = 'dark grey',
        na.rm = TRUE,
        inherit.aes = FALSE
      ) +
      geom_sf(
        data = v_ISO1,
        fill = NA,
        col = 'black',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_gradient(low = "light blue",
                          high = "yellow",
                          na.value = NA) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
    labs(
      fill = paste0(
        "--------------------\nPast\n", 
        drought, 
        "\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) +
      coord_sf(expand = FALSE)
  }

# results in r_droughtf_plot
r_droughtf_plot_f <-
  function(r_droughtf,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           drought) {
    gplot(r_droughtf, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = value), alpha = 1) +
      geom_sf(
        data = world,
        fill = NA,
        col = 'dark grey',
        na.rm = TRUE,
        inherit.aes = FALSE
      ) +
      geom_sf(
        data = v_ISO1,
        fill = NA,
        col = 'black',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_gradient(low = "light blue",
                          high = "yellow",
                          na.value = NA) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
    labs(
      fill = paste0(
        "--------------------\nFuture\n", 
        drought, 
        "\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) +
      coord_sf(expand = FALSE)
  }

# results in r_drought_change_plot
r_drought_change_plot_f <-
  function(r_drought_change,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           drought) {
    gplot(r_drought_change, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = value), alpha = 1) +
      geom_sf(
        data = world,
        fill = NA,
        col = 'dark grey',
        na.rm = TRUE,
        inherit.aes = FALSE
      ) +
      geom_sf(
        data = v_ISO1,
        fill = NA,
        col = 'black',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_gradient(low = "light blue",
                          high = "yellow",
                          na.value = NA) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
    labs(
      fill = paste0(
        "--------------------\nChange\n", 
        drought, 
        "\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) +
      coord_sf(expand = FALSE)
  }

# results in r_heatc_plot
r_heatc_plot_f <-
  function(r_heatc,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           heat) {
    gplot(r_heatc, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = value), alpha = 1) +
      geom_sf(
        data = world,
        fill = NA,
        col = 'dark grey',
        na.rm = TRUE,
        inherit.aes = FALSE
      ) +
      geom_sf(
        data = v_ISO1,
        fill = NA,
        col = 'black',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_gradient(low = "yellow",
                          high = "brown",
                          na.value = NA) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
    labs(
      fill = paste0(
        "--------------------\nPast\n", 
        heat, 
        "\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) +
      coord_sf(expand = FALSE)
  }

# results in r_heatf_plot
r_heatf_plot_f <-
  function(r_heatf,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           heat) {
    gplot(r_heatf, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = value), alpha = 1) +
      geom_sf(
        data = world,
        fill = NA,
        col = 'dark grey',
        na.rm = TRUE,
        inherit.aes = FALSE
      ) +
      geom_sf(
        data = v_ISO1,
        fill = NA,
        col = 'black',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_gradient(low = "yellow",
                          high = "brown",
                          na.value = NA) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
    labs(
      fill = paste0(
        "--------------------\nFuture\n", 
        heat, 
        "\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) +
      coord_sf(expand = FALSE)
  }

# results in r_heat_change_plot
r_heat_change_plot_f <-
  function(r_heat_change,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           heat) {
    gplot(r_heat_change, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = value), alpha = 1) +
      geom_sf(
        data = world,
        fill = NA,
        col = 'dark grey',
        na.rm = TRUE,
        inherit.aes = FALSE
      ) +
      geom_sf(
        data = v_ISO1,
        fill = NA,
        col = 'black',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_gradient(low = "light blue",
                          high = "red",
                          na.value = NA) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
    labs(
      fill = paste0(
        "--------------------\nChange\n", 
        heat, 
        "\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) +
      coord_sf(expand = FALSE)
  }


# results in r_floodc_plot
r_floodc_plot_f <-
  function(r_floodc,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    gplot(r_floodc, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = value), alpha = 1) +
      geom_sf(
        data = world,
        fill = NA,
        col = 'dark grey',
        na.rm = TRUE,
        inherit.aes = FALSE
      ) +
      geom_sf(
        data = v_ISO1,
        fill = NA,
        col = 'black',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_gradient(low = "light blue",
                          high = "blue",
                          na.value = NA) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
    labs(
      fill = paste0(
        "--------------------\nPast\n", 
        flood, 
        "\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) +
      coord_sf(expand = FALSE)
  }

# results in r_floodf_plot
r_floodf_plot_f <-
  function(r_floodf,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    gplot(r_floodf, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = value), alpha = 1) +
      geom_sf(
        data = world,
        fill = NA,
        col = 'dark grey',
        na.rm = TRUE,
        inherit.aes = FALSE
      ) +
      geom_sf(
        data = v_ISO1,
        fill = NA,
        col = 'black',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_gradient(low = "light blue",
                          high = "blue",
                          na.value = NA) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
    labs(
      fill = paste0(
        "--------------------\nFuture\n", 
        flood, 
        "\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) +
      coord_sf(expand = FALSE)
  }

# results in r_flood_change_plot
r_flood_change_plot_f <-
  function(r_flood_change,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    gplot(r_flood_change, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = value), alpha = 1) +
      geom_sf(
        data = world,
        fill = NA,
        col = 'dark grey',
        na.rm = TRUE,
        inherit.aes = FALSE
      ) +
      geom_sf(
        data = v_ISO1,
        fill = NA,
        col = 'black',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = filter(v_crop_ISO_lc_rcl_agg, r__ISO_ == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_gradient(low = "light blue",
                          high = "blue",
                          na.value = NA) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
    labs(
      fill = paste0(
        "--------------------\nChange\n", 
        flood, 
        "\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) +
      coord_sf(expand = FALSE)
  }

### ----- Zonal Statistics ----- ###

### Rainfall

# results in dB_rainfallc_summary
dB_rainfallc_summary_make_f <-
  function(r_rainfallc, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      r_rainfallc,
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75), append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

  # results in dB_rainfallf_summary
dB_rainfallf_summary_make_f <-
  function(r_rainfallf, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      r_rainfallf,
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75), append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

# results in dB_rainfall_change_summary
dB_rainfall_change_summary_make_f <-
  function(r_rainfall_change, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      r_rainfall_change,
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75), append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

### Temperature

# results in dB_tempc_summary
dB_tempc_summary_make_f <-
  function(r_tempc, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      r_tempc,
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75), append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

  # results in dB_tempf_summary
dB_tempf_summary_make_f <-
  function(r_tempf, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      r_tempf,
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75), append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

# results in dB_temp_change_summary
dB_temp_change_summary_make_f <-
  function(r_temp_change, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      r_temp_change,
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75), append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

### Season Onset

# results in dB_onsetc_summary
dB_onsetc_summary_make_f <-
  function(r_onsetc, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      r_onsetc,
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75), append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

  # results in dB_onsetf_summary
dB_onsetf_summary_make_f <-
  function(r_onsetf, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      r_onsetf,
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75), append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

# results in dB_onset_change_summary
dB_onset_change_summary_make_f <-
  function(r_onset_change, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      r_onset_change,
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75), append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

### Season Duration

# results in dB_durationc_summary
dB_durationc_summary_make_f <-
  function(r_durationc, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      r_durationc,
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75), append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

  # results in dB_durationf_summary
dB_durationf_summary_make_f <-
  function(r_durationf, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      r_durationf,
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75), append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

# results in dB_duration_change_summary
dB_duration_change_summary_make_f <-
  function(r_duration_change, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      r_duration_change,
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75), append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

### Drought

# results in dB_droughtc_summary
dB_droughtc_summary_make_f <-
  function(r_droughtc, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      r_droughtc,
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75), append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

  # results in dB_droughtf_summary
dB_droughtf_summary_make_f <-
  function(r_droughtf, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      r_droughtf,
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75), append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

# results in dB_drought_change_summary
dB_drought_change_summary_make_f <-
  function(r_drought_change, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      r_drought_change,
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75), append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

  ### Heat

# results in dB_heatc_summary
dB_heatc_summary_make_f <-
  function(r_heatc, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      r_heatc,
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75), append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

  # results in dB_heatf_summary
dB_heatf_summary_make_f <-
  function(r_heatf, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      r_heatf,
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75), append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

# results in dB_heat_change_summary
dB_heat_change_summary_make_f <-
  function(r_heat_change, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      r_heat_change,
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75), append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

  ### Waterlogging

# results in dB_floodc_summary
dB_floodc_summary_make_f <-
  function(r_floodc, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      r_floodc,
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75), append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

  # results in dB_floodf_summary
dB_floodf_summary_make_f <-
  function(r_floodf, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      r_floodf,
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75), append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

# results in dB_flood_change_summary
dB_flood_change_summary_make_f <-
  function(r_flood_change, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      r_flood_change,
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75), append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

### ----- Zonal Statistics Plots----- ###

### Rainfall

# results in dB_rainfallc_summary_plot
dB_rainfallc_summary_plot_f <- function(dB_rainfallc_summary, ISO, crop) {
  ggplot(dB_rainfallc_summary, aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
    geom_boxplot(aes(
      ymin = max_min,
      lower = q25,
      middle = median,
      upper = q75,
      ymax = min_max
    ),
    stat = "identity") +
    scale_y_continuous(labels = comma) +
    labs(y = "Annual Rainfall") +
    labs(x = "Sourcing Area")  +
    labs(
      fill = paste0(
        "--------------------\nPast\nRainfall\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
    ylim(0, 2000) +    
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}

# results in dB_rainfallf_summary_plot
dB_rainfallf_summary_plot_f <- function(dB_rainfallf_summary, ISO, crop) {
  ggplot(dB_rainfallf_summary, aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
    geom_boxplot(aes(
      ymin = max_min,
      lower = q25,
      middle = median,
      upper = q75,
      ymax = min_max
    ),
    stat = "identity") +
    scale_y_continuous(labels = comma) +
    labs(y = "Annual Rainfall") +
    labs(x = "Sourcing Area")  +
    labs(
      fill = paste0(
        "--------------------\nFuture\nRainfall\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
    ylim(0, 2000) +    
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}

# results in dB_rainfall_change_summary_plot
dB_rainfall_change_summary_plot_f <- function(dB_rainfall_change_summary, ISO, crop) {
  ggplot(dB_rainfall_change_summary, aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
    geom_boxplot(aes(
      ymin = max_min,
      lower = q25,
      middle = median,
      upper = q75,
      ymax = min_max
    ),
    stat = "identity") +
    scale_y_continuous(labels = comma) +
    labs(y = "Annual Rainfall") +
    labs(x = "Sourcing Area")  +
    labs(
      fill = paste0(
        "--------------------\nChange\nRainfall\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
    ylim(-500, 500) +
    geom_hline(
      yintercept = 0,
      linetype = 'dashed',
      colour = 'red',
      size = 1
    ) +        
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}

### Temperature

# results in dB_tempc_summary_plot
dB_tempc_summary_plot_f <- function(dB_tempc_summary, ISO, crop) {
  ggplot(dB_tempc_summary, aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
    geom_boxplot(aes(
      ymin = max_min,
      lower = q25,
      middle = median,
      upper = q75,
      ymax = min_max
    ),
    stat = "identity") +
    scale_y_continuous(labels = comma) +
    labs(y = "Annual Temperature") +
    labs(x = "Sourcing Area")  +
    labs(
      fill = paste0(
        "--------------------\nPast\nTemperature\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
    ylim(10, 35) +    
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}

# results in dB_tempf_summary_plot
dB_tempf_summary_plot_f <- function(dB_tempf_summary, ISO, crop) {
  ggplot(dB_tempf_summary, aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
    geom_boxplot(aes(
      ymin = max_min,
      lower = q25,
      middle = median,
      upper = q75,
      ymax = min_max
    ),
    stat = "identity") +
    scale_y_continuous(labels = comma) +
    labs(y = "Annual Temperature") +
    labs(x = "Sourcing Area")  +
    labs(
      fill = paste0(
        "--------------------\nFuture\nTemperature\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
    ylim(10, 35) +    
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}

# results in dB_temp_change_summary_plot
dB_temp_change_summary_plot_f <- function(dB_temp_change_summary, ISO, crop) {
  ggplot(dB_temp_change_summary, aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
    geom_boxplot(aes(
      ymin = max_min,
      lower = q25,
      middle = median,
      upper = q75,
      ymax = min_max
    ),
    stat = "identity") +
    scale_y_continuous(labels = comma) +
    labs(y = "Annual Temperature") +
    labs(x = "Sourcing Area")  +
    labs(
      fill = paste0(
        "--------------------\nChange\nTemperature\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
    ylim(-5, 5) +
    geom_hline(
      yintercept = 0,
      linetype = 'dashed',
      colour = 'red',
      size = 1
    ) +        
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}

### Season Onset

# results in dB_onsetc_summary_plot
dB_onsetc_summary_plot_f <- function(dB_onsetc_summary, ISO, crop) {
  ggplot(dB_onsetc_summary, aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
    geom_boxplot(aes(
      ymin = max_min,
      lower = q25,
      middle = median,
      upper = q75,
      ymax = min_max
    ),
    stat = "identity") +
    scale_y_continuous(labels = comma) +
    labs(y = "Day number") +
    labs(x = "Sourcing Area")  +
    labs(
      fill = paste0(
        "--------------------\nPast\nSeason Onset\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
    ylim(0, 365) +    
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}

# results in dB_onsetf_summary_plot
dB_onsetf_summary_plot_f <- function(dB_onsetf_summary, ISO, crop) {
  ggplot(dB_onsetf_summary, aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
    geom_boxplot(aes(
      ymin = max_min,
      lower = q25,
      middle = median,
      upper = q75,
      ymax = min_max
    ),
    stat = "identity") +
    scale_y_continuous(labels = comma) +
    labs(y = "Day number") +
    labs(x = "Sourcing Area")  +
    labs(
      fill = paste0(
        "--------------------\nFuture\nSeason Onset\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
    ylim(0, 365) +    
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}

# results in dB_onset_change_summary_plot
dB_onset_change_summary_plot_f <- function(dB_onset_change_summary, ISO, crop) {
  ggplot(dB_onset_change_summary, aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
    geom_boxplot(aes(
      ymin = max_min,
      lower = q25,
      middle = median,
      upper = q75,
      ymax = min_max
    ),
    stat = "identity") +
    scale_y_continuous(labels = comma) +
    labs(y = "Day number") +
    labs(x = "Sourcing Area")  +
    labs(
      fill = paste0(
        "--------------------\nChange\nSeason Onset\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
    ylim(-50, 50) +
    geom_hline(
      yintercept = 0,
      linetype = 'dashed',
      colour = 'red',
      size = 1
    ) +        
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}

### Season Duration

# results in dB_durationc_summary_plot
dB_durationc_summary_plot_f <- function(dB_durationc_summary, ISO, crop) {
  ggplot(dB_durationc_summary, aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
    geom_boxplot(aes(
      ymin = max_min,
      lower = q25,
      middle = median,
      upper = q75,
      ymax = min_max
    ),
    stat = "identity") +
    scale_y_continuous(labels = comma) +
    labs(y = "Days") +
    labs(x = "Sourcing Area")  +
    labs(
      fill = paste0(
        "--------------------\nPast\nSeason Duration\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
    ylim(0, 365) +    
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}

# results in dB_durationf_summary_plot
dB_durationf_summary_plot_f <- function(dB_durationf_summary, ISO, crop) {
  ggplot(dB_durationf_summary, aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
    geom_boxplot(aes(
      ymin = max_min,
      lower = q25,
      middle = median,
      upper = q75,
      ymax = min_max
    ),
    stat = "identity") +
    scale_y_continuous(labels = comma) +
    labs(y = "Days") +
    labs(x = "Sourcing Area")  +
    labs(
      fill = paste0(
        "--------------------\nFuture\nSeason Duration\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
    ylim(0, 365) +    
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}

# results in dB_duration_change_summary_plot
dB_duration_change_summary_plot_f <- function(dB_duration_change_summary, ISO, crop) {
  ggplot(dB_duration_change_summary, aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
    geom_boxplot(aes(
      ymin = max_min,
      lower = q25,
      middle = median,
      upper = q75,
      ymax = min_max
    ),
    stat = "identity") +
    scale_y_continuous(labels = comma) +
    labs(y = "Days") +
    labs(x = "Sourcing Area")  +
    labs(
      fill = paste0(
        "--------------------\nChange\nSeason Duration\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
    ylim(-150, 150) +
    geom_hline(
      yintercept = 0,
      linetype = 'dashed',
      colour = 'red',
      size = 1
    ) +        
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}

### Drought

# results in dB_droughtc_summary_plot
dB_droughtc_summary_plot_f <- function(dB_droughtc_summary, drought_threshold_l, drought_threshold_u, drought, ISO, crop) {
  ggplot(dB_droughtc_summary, aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
    #filter(dB_sourcing_stats, Landuse_names == c("Cotton", "Cropland")),
    geom_boxplot(aes(
      ymin = max_min,
      lower = q25,
      middle = median,
      upper = q75,
      ymax = min_max
    ),
    stat = "identity") +
    #scale_fill_manual(values = c("white", "orange", "yellow", "green", "dark green")) +
    #scale_fill_manual(values = c("green", "dark green")) +
    scale_y_continuous(labels = comma) +
    labs(y = "# of seasons with hazard") +
    labs(x = "Sourcing Area")  +
    labs(
      fill = paste0(
        "--------------------\nPast\n", 
        drought, 
        "\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
    ylim(0, 20) +
    geom_hline(
      yintercept = drought_threshold_l,
      linetype = 'dashed',
      colour = 'red',
      size = 1
    ) +
    geom_text(
      x = 0,
      y = (drought_threshold_l + 0.5),
      label = "Lower Risk Threshold",
      colour = 'red',
      hjust = 0
    ) +
    geom_hline(
      yintercept = drought_threshold_u,
      linetype = 'dashed',
      colour = 'dark red',
      size = 1
    ) +
    geom_text(
      x = 0,
      y = (drought_threshold_u + 0.5),
      label = "Upper Risk Threshold",
      colour = 'dark red',
      hjust = 0
    ) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}

# results in dB_droughtf_summary_plot
dB_droughtf_summary_plot_f <- function(dB_droughtf_summary, drought_threshold_l, drought_threshold_u, drought, ISO, crop) {
  ggplot(dB_droughtf_summary, aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
    #filter(dB_sourcing_stats, Landuse_names == c("Cotton", "Cropland")),
    geom_boxplot(aes(
      ymin = max_min,
      lower = q25,
      middle = median,
      upper = q75,
      ymax = min_max
    ),
    stat = "identity") +
    #scale_fill_manual(values = c("white", "orange", "yellow", "green", "dark green")) +
    #scale_fill_manual(values = c("green", "dark green")) +
    scale_y_continuous(labels = comma) +
    labs(y = "# of seasons with hazard") +
    labs(x = "Sourcing Area")  +
    labs(
      fill = paste0(
        "--------------------\nFuture\n", 
        drought, 
        "\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
    ylim(0, 20) +
    geom_hline(
      yintercept = drought_threshold_l,
      linetype = 'dashed',
      colour = 'red',
      size = 1
    ) +
    geom_text(
      x = 0,
      y = (drought_threshold_l + 0.5),
      label = "Lower Risk Threshold",
      colour = 'red',
      hjust = 0
    ) +
    geom_hline(
      yintercept = drought_threshold_u,
      linetype = 'dashed',
      colour = 'dark red',
      size = 1
    ) +
    geom_text(
      x = 0,
      y = (drought_threshold_u + 0.5),
      label = "Upper Risk Threshold",
      colour = 'dark red',
      hjust = 0
    ) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}

  # results in dB_drought_change_summary_plot
dB_drought_change_summary_plot_f <- function(dB_drought_change_summary, drought_threshold_l, drought_threshold_u, drought, ISO, crop) {
  ggplot(dB_drought_change_summary, aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
    #filter(dB_sourcing_stats, Landuse_names == c("Cotton", "Cropland")),
    geom_boxplot(aes(
      ymin = max_min,
      lower = q25,
      middle = median,
      upper = q75,
      ymax = min_max
    ),
    stat = "identity") +
    #scale_fill_manual(values = c("white", "orange", "yellow", "green", "dark green")) +
    #scale_fill_manual(values = c("green", "dark green")) +
    scale_y_continuous(labels = comma) +
    labs(y = "Change in frequency") +
    labs(x = "Sourcing Area")  +
    labs(
      fill = paste0(
        "--------------------\nChange\n", 
        drought, 
        "\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
    ylim(-20, 20) +
    geom_hline(
      yintercept = 0,
      linetype = 'dashed',
      colour = 'red',
      size = 1
    ) +    
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}


### Heat Stress

# results in dB_heatc_summary_plot
dB_heatc_summary_plot_f <- function(dB_heatc_summary, heat_threshold_l, heat_threshold_u, heat, ISO, crop) {
  ggplot(dB_heatc_summary, aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
    #filter(dB_sourcing_stats, Landuse_names == c("Cotton", "Cropland")),
    geom_boxplot(aes(
      ymin = max_min,
      lower = q25,
      middle = median,
      upper = q75,
      ymax = min_max
    ),
    stat = "identity") +
    #scale_fill_manual(values = c("white", "orange", "yellow", "green", "dark green")) +
    #scale_fill_manual(values = c("green", "dark green")) +
    scale_y_continuous(labels = comma) +
    labs(y = "# of seasons with hazard") +
    labs(x = "Sourcing Area")  +
    labs(
      fill = paste0(
        "--------------------\nPast\n", 
        heat, 
        "\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
    ylim(0, 20) +
    geom_hline(
      yintercept = heat_threshold_l,
      linetype = 'dashed',
      colour = 'red',
      size = 1
    ) +
    geom_text(
      x = 0,
      y = (heat_threshold_l + 0.5),
      label = "Lower Risk Threshold",
      colour = 'red',
      hjust = 0
    ) +
    geom_hline(
      yintercept = heat_threshold_u,
      linetype = 'dashed',
      colour = 'dark red',
      size = 1
    ) +
    geom_text(
      x = 0,
      y = (heat_threshold_u + 0.5),
      label = "Upper Risk Threshold",
      colour = 'dark red',
      hjust = 0
    ) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}

# results in dB_heatf_summary_plot
dB_heatf_summary_plot_f <- function(dB_heatf_summary, heat_threshold_l, heat_threshold_u, heat, ISO, crop) {
  ggplot(dB_heatf_summary, aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
    #filter(dB_sourcing_stats, Landuse_names == c("Cotton", "Cropland")),
    geom_boxplot(aes(
      ymin = max_min,
      lower = q25,
      middle = median,
      upper = q75,
      ymax = min_max
    ),
    stat = "identity") +
    #scale_fill_manual(values = c("white", "orange", "yellow", "green", "dark green")) +
    #scale_fill_manual(values = c("green", "dark green")) +
    scale_y_continuous(labels = comma) +
    labs(y = "# of seasons with hazard") +
    labs(x = "Sourcing Area")  +
    labs(
      fill = paste0(
        "--------------------\nFuture\n", 
        heat, 
        "\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
    ylim(0, 20) +
    geom_hline(
      yintercept = heat_threshold_l,
      linetype = 'dashed',
      colour = 'red',
      size = 1
    ) +
    geom_text(
      x = 0,
      y = (heat_threshold_l + 0.5),
      label = "Lower Risk Threshold",
      colour = 'red',
      hjust = 0
    ) +
    geom_hline(
      yintercept = heat_threshold_u,
      linetype = 'dashed',
      colour = 'dark red',
      size = 1
    ) +
    geom_text(
      x = 0,
      y = (heat_threshold_u + 0.5),
      label = "Upper Risk Threshold",
      colour = 'dark red',
      hjust = 0
    ) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}

  # results in dB_heat_change_summary_plot
dB_heat_change_summary_plot_f <- function(dB_heat_change_summary, heat_threshold_l, heat_threshold_u, heat, ISO, crop) {
  ggplot(dB_heat_change_summary, aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
    #filter(dB_sourcing_stats, Landuse_names == c("Cotton", "Cropland")),
    geom_boxplot(aes(
      ymin = max_min,
      lower = q25,
      middle = median,
      upper = q75,
      ymax = min_max
    ),
    stat = "identity") +
    #scale_fill_manual(values = c("white", "orange", "yellow", "green", "dark green")) +
    #scale_fill_manual(values = c("green", "dark green")) +
    scale_y_continuous(labels = comma) +
    labs(y = "Change in frequency") +
    labs(x = "Sourcing Area")  +
    labs(
      fill = paste0(
        "--------------------\nChange\n", 
        heat, 
        "\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
    ylim(-20, 20) +
    geom_hline(
      yintercept = 0,
      linetype = 'dashed',
      colour = 'red',
      size = 1
    ) +    
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}


### Waterlogging

 # results in dB_floodc_summary_plot
dB_floodc_summary_plot_f <- function(dB_floodc_summary, flood_threshold_l, flood_threshold_u, flood, ISO, crop) {
  ggplot(dB_floodc_summary, aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
    #filter(dB_sourcing_stats, Landuse_names == c("Cotton", "Cropland")),
    geom_boxplot(aes(
      ymin = max_min,
      lower = q25,
      middle = median,
      upper = q75,
      ymax = min_max
    ),
    stat = "identity") +
    #scale_fill_manual(values = c("white", "orange", "yellow", "green", "dark green")) +
    #scale_fill_manual(values = c("green", "dark green")) +
    scale_y_continuous(labels = comma) +
    labs(y = "Average # per period") +
    labs(x = "Sourcing Area")  +
    labs(
      fill = paste0(
        "--------------------\nPast\n", 
        flood, 
        "\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
    ylim(0, 20) +
    geom_hline(
      yintercept = flood_threshold_l,
      linetype = 'dashed',
      colour = 'red',
      size = 1
    ) +
    geom_text(
      x = 0,
      y = (flood_threshold_l + 0.5),
      label = "Lower Risk Threshold",
      colour = 'red',
      hjust = 0
    ) +
    geom_hline(
      yintercept = flood_threshold_u,
      linetype = 'dashed',
      colour = 'dark red',
      size = 1
    ) +
    geom_text(
      x = 0,
      y = (flood_threshold_u + 0.5),
      label = "Upper Risk Threshold",
      colour = 'dark red',
      hjust = 0
    ) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}

# results in dB_floodf_summary_plot
dB_floodf_summary_plot_f <- function(dB_floodf_summary, flood_threshold_l, flood_threshold_u, flood, ISO, crop) {
  ggplot(dB_floodf_summary, aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
    #filter(dB_sourcing_stats, Landuse_names == c("Cotton", "Cropland")),
    geom_boxplot(aes(
      ymin = max_min,
      lower = q25,
      middle = median,
      upper = q75,
      ymax = min_max
    ),
    stat = "identity") +
    #scale_fill_manual(values = c("white", "orange", "yellow", "green", "dark green")) +
    #scale_fill_manual(values = c("green", "dark green")) +
    scale_y_continuous(labels = comma) +
    labs(y = "Average # per period") +
    labs(x = "Sourcing Area")  +
    labs(
      fill = paste0(
        "--------------------\nFuture\n", 
        flood, 
        "\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
    ylim(0, 20) +
    geom_hline(
      yintercept = flood_threshold_l,
      linetype = 'dashed',
      colour = 'red',
      size = 1
    ) +
    geom_text(
      x = 0,
      y = (flood_threshold_l + 0.5),
      label = "Lower Risk Threshold",
      colour = 'red',
      hjust = 0
    ) +
    geom_hline(
      yintercept = flood_threshold_u,
      linetype = 'dashed',
      colour = 'dark red',
      size = 1
    ) +
    geom_text(
      x = 0,
      y = (flood_threshold_u + 0.5),
      label = "Upper Risk Threshold",
      colour = 'dark red',
      hjust = 0
    ) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}

  # results in dB_flood_change_summary_plot
dB_flood_change_summary_plot_f <- function(dB_flood_change_summary, flood_threshold_l, flood_threshold_u, flood, ISO, crop) {
  ggplot(dB_flood_change_summary, aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
    #filter(dB_sourcing_stats, Landuse_names == c("Cotton", "Cropland")),
    geom_boxplot(aes(
      ymin = max_min,
      lower = q25,
      middle = median,
      upper = q75,
      ymax = min_max
    ),
    stat = "identity") +
    #scale_fill_manual(values = c("white", "orange", "yellow", "green", "dark green")) +
    #scale_fill_manual(values = c("green", "dark green")) +
    scale_y_continuous(labels = comma) +
    labs(y = "Change in average # per period") +
    labs(x = "Sourcing Area")  +
    labs(
      fill = paste0(
        "--------------------\nChange\n", 
        flood, 
        "\n",
        ISO, 
        "\n",
        crop,
        "\n--------------------"
      )
    ) +
    ylim(-10, 10) +
    geom_hline(
      yintercept = 0,
      linetype = 'dashed',
      colour = 'red',
      size = 1
    ) +    
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}


### ----- Impact maps and charts ----- ###

### Prepare Impact Data

# results in dB_impact_prelim
dB_impact_prelim_make_f <- function(r_droughtc) {
  coordinates(rasterToPoints(r_droughtc)) %>% as_tibble() %>% dplyr::select(x,y) 
}

# results in xy_impact
xy_impact_make_f <- function(dB_impact_prelim) {
  cbind(pull(dB_impact_prelim, x), pull(dB_impact_prelim, y))
}

# results in newcol_droughtc
newcol_droughtc_make_f <- function(xy_impact, r_droughtc) {
  extract(r_droughtc, xy_impact)
}

# results in newcol_droughtf
newcol_droughtf_make_f <- function(xy_impact, r_droughtf) {
  extract(r_droughtf, xy_impact)
}

# results in newcol_heatc
newcol_heatc_make_f <- function(xy_impact, r_heatc) {
  extract(r_heatc, xy_impact)
}

# results in newcol_heatf
newcol_heatf_make_f <- function(xy_impact, r_heatf) {
  extract(r_heatf, xy_impact)
}

# results in newcol_floodc
newcol_floodc_make_f <- function(xy_impact, r_floodc) {
  extract(r_floodc, xy_impact)
}

# results in newcol_floodf
newcol_floodf_make_f <- function(xy_impact, r_floodf) {
  extract(r_floodf, xy_impact)
}

# results in dB_impact
dB_impact_make_f <- function(dB_impact_prelim, newcol_droughtc, newcol_droughtf, newcol_heatc, newcol_heatf, newcol_floodc, newcol_floodf) {
  dB_impact_prelim %>% mutate(droughtc = newcol_droughtc) %>% mutate(droughtf = newcol_droughtf) %>% mutate(heatc = newcol_heatc) %>% mutate(heatf = newcol_heatf) %>% mutate(floodc = newcol_floodc) %>% mutate(floodf = newcol_floodf)
}

### Fuzzy Partitions

#### construct Fuzzy Partitions

# results in fp_drought_l
fp_drought_l_make_f <- function(drought_threshold_l, drought_threshold_l_width){
      LinearFuzzyPartition(
      level = c("low", "high"),
      crossoverPoint = drought_threshold_l,
      transitionWidth = drought_threshold_l_width
    )
}

# results in fp_drought_u
fp_drought_u_make_f <- function(drought_threshold_u, drought_threshold_u_width){
      LinearFuzzyPartition(
      level = c("low", "high"),
      crossoverPoint = drought_threshold_u,
      transitionWidth = drought_threshold_u_width
    )
}

# results in fp_heat_l
fp_heat_l_make_f <- function(heat_threshold_l, heat_threshold_l_width){
      LinearFuzzyPartition(
      level = c("low", "high"),
      crossoverPoint = heat_threshold_l,
      transitionWidth = heat_threshold_l_width
    )
}

# results in fp_heat_u
fp_heat_u_make_f <- function(heat_threshold_u, heat_threshold_u_width){
      LinearFuzzyPartition(
      level = c("low", "high"),
      crossoverPoint = heat_threshold_u,
      transitionWidth = heat_threshold_u_width
    )
}

# results in fp_flood_l
fp_flood_l_make_f <- function(flood_threshold_l, flood_threshold_l_width){
      LinearFuzzyPartition(
      level = c("low", "high"),
      crossoverPoint = flood_threshold_l,
      transitionWidth = flood_threshold_l_width
    )
}

# results in fp_flood_u
fp_flood_u_make_f <- function(flood_threshold_u, flood_threshold_u_width){
      LinearFuzzyPartition(
      level = c("low", "high"),
      crossoverPoint = flood_threshold_u,
      transitionWidth = flood_threshold_u_width
    )
}

#### plot Fuzzy Partitions

# results in fp_drought_l_plot
fp_drought_l_plot_f <- function(fp_drought_l, drought){
plot(
  fp_drought_l,
  n = 10,
  xlim = c(0, 20),
  xlab = paste(drought),
  title = "")
}

# results in fp_drought_u_plot
fp_drought_u_plot_f <- function(fp_drought_u, drought){
plot(
  fp_drought_u,
  n = 10,
  xlim = c(0, 20),
  xlab = paste(drought),
  title = "")
}

# results in fp_heat_l_plot
fp_heat_l_plot_f <- function(fp_heat_l, heat){
plot(
  fp_heat_l,
  n = 10,
  xlim = c(0, 20),
  xlab = paste(heat),
  title = "")
}

# results in fp_heat_u_plot
fp_heat_u_plot_f <- function(fp_heat_u, heat){
plot(
  fp_heat_u,
  n = 10,
  xlim = c(0, 20),
  xlab = paste(heat),
  title = "")
}

# results in fp_flood_l_plot
fp_flood_l_plot_f <- function(fp_flood_l, flood){
plot(
  fp_flood_l,
  n = 10,
  xlim = c(0, 20),
  xlab = paste(flood),
  title = "")
}

# results in fp_flood_u_plot
fp_flood_u_plot_f <- function(fp_flood_u, flood){
plot(
  fp_flood_u,
  n = 10,
  xlim = c(0, 20),
  xlab = paste(flood),
  title = "")
}

### Rule Base Construction

# results in rb_droughtc
rb_droughtc_make_f <- function(drought){
    
  RuleBase(new("Proposition", table = tibble(!!paste0(as.character("droughtc")) := c("low", "high"))), 
    new("Conclusion", table = tibble(!!paste0(as.character("droughtc"),
           "_o") := c("optimal", "suboptimal"))))
}


# results in rb_heatc
rb_heatc_make_f <- function(heat){
    
  RuleBase(new("Proposition", table = tibble(!!paste0(as.character("heatc")) := c("low", "high"))), 
    new("Conclusion", table = tibble(!!paste0(as.character("heatc"),
           "_o") := c("optimal", "suboptimal"))))
}

# results in rb_floodc
rb_floodc_make_f <- function(flood){
    
  RuleBase(new("Proposition", table = tibble(!!paste0(as.character("floodc")) := c("low", "high"))), 
    new("Conclusion", table = tibble(!!paste0(as.character("floodc"),
           "_o") := c("optimal", "suboptimal"))))
}

# results in rb_droughtf
rb_droughtf_make_f <- function(drought){
    
  RuleBase(new("Proposition", table = tibble(!!paste0(as.character("droughtf")) := c("low", "high"))), 
    new("Conclusion", table = tibble(!!paste0(as.character("droughtf"),
           "_o") := c("optimal", "suboptimal"))))
}

# results in rb_heatf
rb_heatf_make_f <- function(heat){
    
  RuleBase(new("Proposition", table = tibble(!!paste0(as.character("heatf")) := c("low", "high"))), 
    new("Conclusion", table = tibble(!!paste0(as.character("heatf"),
           "_o") := c("optimal", "suboptimal"))))
}

# results in rb_floodf
rb_floodf_make_f <- function(flood){
    
  RuleBase(new("Proposition", table = tibble(!!paste0(as.character("floodf")) := c("low", "high"))), 
    new("Conclusion", table = tibble(!!paste0(as.character("floodf"),
           "_o") := c("optimal", "suboptimal"))))
}

### Fuzzy Partition Matrix Prediction lower


# results in fpm_droughtc_l
fpm_droughtc_l_make_f <- function(rb_droughtc, dB_impact, fp_drought_l){

  predict(rb_droughtc,
          newdata = dB_impact,
          droughtc = fp_drought_l)

}

# results in fpm_heatc_l
fpm_heatc_l_make_f <- function(rb_heatc, dB_impact, fp_heat_l){

  predict(rb_heatc,
          newdata = dB_impact,
          heatc = fp_heat_l)

}

# results in fpm_floodc_l
fpm_floodc_l_make_f <- function(rb_floodc, dB_impact, fp_flood_l){

  predict(rb_floodc,
          newdata = dB_impact,
          floodc = fp_flood_l)

}


# results in fpm_droughtf_l
fpm_droughtf_l_make_f <- function(rb_droughtf, dB_impact, fp_drought_l){

  predict(rb_droughtf,
          newdata = dB_impact,
          droughtf = fp_drought_l)

}

# results in fpm_heatf_l
fpm_heatf_l_make_f <- function(rb_heatf, dB_impact, fp_heat_l){

  predict(rb_heatf,
          newdata = dB_impact,
          heatf = fp_heat_l)

}

# results in fpm_floodf_l
fpm_floodf_l_make_f <- function(rb_floodf, dB_impact, fp_flood_l){

  predict(rb_floodf,
          newdata = dB_impact,
          floodf = fp_flood_l)

}

### Fuzzy Partition Matrix Prediction upper


# results in fpm_droughtc_u
fpm_droughtc_u_make_f <- function(rb_droughtc, dB_impact, fp_drought_u){

  predict(rb_droughtc,
          newdata = dB_impact,
          droughtc = fp_drought_u)

}

# results in fpm_heatc_u
fpm_heatc_u_make_f <- function(rb_heatc, dB_impact, fp_heat_u){

  predict(rb_heatc,
          newdata = dB_impact,
          heatc = fp_heat_u)

}

# results in fpm_floodc_u
fpm_floodc_u_make_f <- function(rb_floodc, dB_impact, fp_flood_u){

  predict(rb_floodc,
          newdata = dB_impact,
          floodc = fp_flood_u)

}


# results in fpm_droughtf_u
fpm_droughtf_u_make_f <- function(rb_droughtf, dB_impact, fp_drought_u){

  predict(rb_droughtf,
          newdata = dB_impact,
          droughtf = fp_drought_u)

}

# results in fpm_heatf_u
fpm_heatf_u_make_f <- function(rb_heatf, dB_impact, fp_heat_u){

  predict(rb_heatf,
          newdata = dB_impact,
          heatf = fp_heat_u)

}

# results in fpm_floodf_u
fpm_floodf_u_make_f <- function(rb_floodf, dB_impact, fp_flood_u){

  predict(rb_floodf,
          newdata = dB_impact,
          floodf = fp_flood_u)

}
