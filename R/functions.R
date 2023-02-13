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

# results in drought_threshold_l
drought_threshold_l_get_f <- function(cc_data, cc_row) {
  as.numeric(paste(cc_data[cc_row, 25]))
}

# results in drought_threshold_l_width
drought_threshold_l_width_get_f <- function(cc_data, cc_row) {
  as.numeric(paste(cc_data[cc_row, 26]))
}

# results in drought_threshold_u
drought_threshold_u_get_f <- function(cc_data, cc_row) {
  as.numeric(paste(cc_data[cc_row, 27]))
}

# results in drought_threshold_u_width
drought_threshold_u_width_get_f <- function(cc_data, cc_row) {
  as.numeric(paste(cc_data[cc_row, 28]))
}


# results in heat_threshold_l
heat_threshold_l_get_f <- function(cc_data, cc_row) {
  as.numeric(paste(cc_data[cc_row, 29]))
}

# results in heat_threshold_l_width
heat_threshold_l_width_get_f <- function(cc_data, cc_row) {
  as.numeric(paste(cc_data[cc_row, 30]))
}

# results in heat_threshold_u
heat_threshold_u_get_f <- function(cc_data, cc_row) {
  as.numeric(paste(cc_data[cc_row, 31]))
}

# results in heat_threshold_u_width
heat_threshold_u_width_get_f <- function(cc_data, cc_row) {
  as.numeric(paste(cc_data[cc_row, 32]))
}


# results in flood_threshold_l
flood_threshold_l_get_f <- function(cc_data, cc_row) {
  as.numeric(paste(cc_data[cc_row, 33]))
}

# results in flood_threshold_l_width
flood_threshold_l_width_get_f <- function(cc_data, cc_row) {
  as.numeric(paste(cc_data[cc_row, 34]))
}

# results in flood_threshold_u
flood_threshold_u_get_f <- function(cc_data, cc_row) {
  as.numeric(paste(cc_data[cc_row, 35]))
}

# results in flood_threshold_u_width
flood_threshold_u_width_get_f <- function(cc_data, cc_row) {
  as.numeric(paste(cc_data[cc_row, 36]))
}

# results in aggregation_value
aggregation_value_get_f <- function(cc_data, cc_row) {
  as.numeric(paste(cc_data[cc_row, 37]))
}

### ----- Country spatial data ----- ###

# results in v_crop
v_crop_get_f <- function(cc_data, cc_row) {
  st_read(paste0("data/", paste(cc_data[cc_row, 4])))
  
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
    labs(
      fill = paste0(
        "--------------------\nv_crop\n",
        ISO,
        " ",
        crop,
        "\n\n--------------------"
      )
    )
}


# results in v_ISO
v_ISO_get_f <- function(ISO) {
  getData('GADM', country = paste(ISO), level = 0) %>% st_as_sf() %>% mutate(New_ID = 1)
}


# results in v_ISO1
#v_ISO1_get_f <- function(cc_data, cc_row, ISO) {

#  if (cc_data[cc_row, 23] == 1)  {
#    getData('GADM', country = paste(ISO), level = 1) %>% st_as_sf() %>% mutate(New_ID = 1)
#  } else {
#    st_read(paste0("data/", paste(cc_data[cc_row, 24]))) %>% mutate(New_ID = 1)
#  }
#  
#}


# alternative because getData is being deprecated

# results in v_ISO1
v_ISO1_get_f <- function(cc_data, cc_row, ISO) {
  
  if (cc_data[cc_row, 23] == 1)  {
    gadm( country = paste(ISO), level = 1 , version="latest", path = "D:/repos/sourcing_targets" ) %>% st_as_sf() %>% mutate(New_ID = 1)
  } else {
    st_read(paste0("data/", paste(cc_data[cc_row, 24]))) %>% mutate(New_ID = 1)
  }
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
      fill = 'grey', # previously aes(fill = NAME_1),
      col = 'black',
      na.rm = TRUE,
      inherit.aes = FALSE
    )  +
    labs(
      fill = paste0(
        "--------------------\nv_ISO1\n",
        ISO,
        "\nExtent\n\n\n\n--------------------"
      )
    )
}

# results in r_clim_mask
r_clim_mask_get_f <- function(ISO) {
  paste0("data/", ISO, "/clim_mask.tif") %>% raster()
}

# results in r_lc_global
# this is the original function - now (19/12/2022) replaced with local potential crop areas
#r_lc_global_get_f <- function(r_clim_mask) {
#  raster(
#    "D:/repos/climate-smart-agri-sourcing/spatial_data/output/r_crop_mask_COP_rcl.tif"
#  )
#}

# results in r_lc_global
# this is the new function - now (19/12/2022) to replace the global potential crop areas
r_lc_global_get_f <- function(r_clim_mask, cc_data, cc_row) {
  raster(paste0("data/", paste(cc_data[cc_row, 22])))
}

# results in r_lc_file
#r_lc_make_write_f <- function(r_lc_global, r_clim_mask, ISO) {
#  r_lc_global %>% crop(r_clim_mask) %>%  writeRaster(paste0("data/", ISO, "/r_lc.tif"), overwrite = TRUE)
#}

# results in r_lc_file - alternative
r_lc_make_write_f <- function(r_lc_global, aggregation_value, r_clim_mask, ISO) {
  r_lc_global %>% aggregate(aggregation_value, fun = "modal") %>% crop(r_clim_mask, filename = paste0("data/", ISO, "/r_lc.tif"), overwrite = TRUE)
}


# results in r_lc
r_lc_get_f <- function(r_lc_file, ISO) {
  raster(paste0("data/", ISO, "/r_lc.tif"))
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

### ----- Spatial data preparation ----- ###


# # results in r_ISO_file
# r_ISO_make_write_f  <- function(v_ISO, r_lc, v_ISO_extent, ISO) {
#  rasterize(vect(v_ISO), rast(r_lc), field = "New_ID")  %>% crop(v_ISO_extent) %>% raster() %>%
#  writeRaster(paste0("data/", ISO, "/r_ISO.tif"), overwrite = TRUE)
# }

# results in r_ISO_a_file
r_ISO_a_make_write_f  <- function(v_ISO, r_lc, ISO) {
  rasterize(
    v_ISO,
    r_lc,
    field = "New_ID",
    filename = paste0("data/", ISO, "/r_ISO_a.tif"),
    overwrite = TRUE
  )
}

# results in r_ISO_a
r_ISO_a_get_f <- function(ISO, r_ISO_a_file) {
  raster(paste0("data/", ISO, "/r_ISO_a.tif"))
}

# results in r_ISO_file
r_ISO_make_write_f  <- function(r_ISO_a, v_ISO_extent, ISO) {
  raster::crop(
    r_ISO_a,
    v_ISO_extent,
    filename = paste0("data/", ISO, "/r_ISO.tif"),
    overwrite = TRUE
  )
}

# # results in r_ISO_b
# r_ISO_b_get_f <- function(ISO, r_ISO_b_file) {
#   raster(paste0("data/", ISO, "/r_ISO_b.tif"))
# }

# # results in r_ISO_file
# r_ISO_make_write_f  <- function(r_ISO_b, ISO) {
# r_ISO_b %>% raster() %>%
#  writeRaster(paste0("data/", ISO, "/r_ISO.tif"), overwrite = TRUE)
# }

# results in r_ISO
r_ISO_get_f <- function(ISO, r_ISO_file) {
  raster(paste0("data/", ISO, "/r_ISO.tif"))
}


# results in r_lc_ISO_file
r_lc_ISO_make_write_f  <- function(r_lc, r_ISO,  ISO) {
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
    xlim((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax + 1)) +
    ylim((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax + 1)) +
    labs(fill = paste0(
      "-------------------\nr_lc_ISO\n",
      ISO ,
      "\n
        \n\n\n--------------------"
    )) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
}

# results in r_crop_file
r_crop_make_write_f  <- function(v_crop, r_lc_ISO, ISO, crop) {
  rasterize(vect(v_crop),
            rast(r_lc_ISO),
            field = 1,
            background = 0) %>% raster() %>% writeRaster(paste0("data/", ISO, "/", crop, "/r_crop.tif"), overwrite = TRUE)
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
        "--------------------\nr_crop_ISO\n",
        ISO,
        " ",
        crop,
        "\n\n\n\n\n--------------------"
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
r_crop_ISO_lc_file_make_write_f <-
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
        "--------------------\nr_crop_ISO_lc\n",
        ISO,
        " ",
        crop,
        "\n\n\n\n\n--------------------"
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
          -0.5,
          0.5,
          0,
          0.5,
          1.5,
          0,
          1.5,
          2.5,
          0,
          2.5,
          3.5,
          1,
          3.5,
          10.5,
          0,
          10.5,
          11.5,
          0,
          11.5,
          12.5,
          0,
          12.5,
          13.5,
          2
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
          "--------------------\nr_crop_ISO_lc_rcl\n",
          ISO,
          " ",
          crop,
          "\n\n\n\n\n--------------------"
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

### old version before aggregating earlier
# results in r_crop_ISO_lc_rcl_agg_file
#r_crop_ISO_lc_rcl_agg_make_write_f <-
#  function(r_crop_ISO_lc_rcl, aggregation_value, ISO, crop) {
#    aggregate(r_crop_ISO_lc_rcl, aggregation_value, fun = "modal") %>% writeRaster(paste0("data/", ISO, "/", crop, "/r_crop_ISO_lc_rcl_agg.tif"),
#                                                                    overwrite = TRUE)
#  }

# results in r_crop_ISO_lc_rcl_agg_file
r_crop_ISO_lc_rcl_agg_make_write_f <-
  function(r_crop_ISO_lc_rcl, ISO, crop) {
    r_crop_ISO_lc_rcl %>% writeRaster(paste0("data/", ISO, "/", crop, "/r_crop_ISO_lc_rcl_agg.tif"),
                                                                    overwrite = TRUE)
  }

# results in r_crop_ISO_lc_rcl_agg
r_crop_ISO_lc_rcl_agg_get_f <-
  function(r_crop_ISO_lc_rcl_agg_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/r_crop_ISO_lc_rcl_agg.tif"))
  }
 

# results in r_crop_ISO_lc_rcl_agg_values
r_crop_ISO_lc_rcl_agg_values_get_f <-
  function(r_crop_ISO_lc_rcl_agg) {
    #values(r_crop_ISO_lc_rcl_agg) %>% unique() %>% as.numeric(na.omit())
    as.numeric(na.omit(unique(values(
      r_crop_ISO_lc_rcl_agg
    ))))
  }


# # results in v_crop_ISO_lc_rcl_agg_file1
# v_crop_ISO_lc_rcl_agg1_make_write_f <-
#   function(r_crop_ISO_lc_rcl_agg,  ISO, crop) {
#     rasterToPolygons(
#       r_crop_ISO_lc_rcl_agg,
#       fun = NULL,
#       n = 4,
#       na.rm = TRUE,
#       digits = 12,
#       dissolve = TRUE
#     ) %>%
#      st_as_sf %>%
#      write_sf(paste0("data/", ISO, "/", crop, "/v_crop_ISO_lc_rcl_agg1.shp"), overwrite = TRUE)
#   }

# results in v_crop_ISO_lc_rcl_agg_file1
v_crop_ISO_lc_rcl_agg1_make_write_f <-
  function(r_crop_ISO_lc_rcl_agg,  ISO, crop) {
    as.polygons(
      rast(r_crop_ISO_lc_rcl_agg),
      trunc = TRUE,
      dissolve = TRUE,
      values = TRUE,
      na.rm = TRUE,
      extent = FALSE
    ) %>%
      writeVector(paste0("data/", ISO, "/", crop, "/v_crop_ISO_lc_rcl_agg1.shp"),
                  overwrite = TRUE)
  }




# results in v_crop_ISO_lc_rcl_agg
v_crop_ISO_lc_rcl_agg_get_f <-
  function(v_crop_ISO_lc_rcl_agg_file1,
           r_crop_ISO_lc_rcl_agg_values,
           r_crop_ISO_lc_rcl_agg_values_vec0,
           r_crop_ISO_lc_rcl_agg_values_vec1,
           r_crop_ISO_lc_rcl_agg_values_vec2,
           r_crop_ISO_lc_rcl_agg_values_vec01,
           r_crop_ISO_lc_rcl_agg_values_vec02,
           r_crop_ISO_lc_rcl_agg_values_vec12,
           ISO,
           crop,
           v_ISO1) {
    # compare vector of raster values with possibilities    
    # 23/09/2022 Problem while computing `LC = factor(...)`.Caused by error in `unique.default()`:! unique() applies only to vectors
    
    ## try changing r_crop_ISO to r_crop_ISO_lc_rcl_agg - this does not work
    ## try the r_crop_ISO_lc_rcl_agg_values - this does work but r_crop_ISO is meant to be a field
    ## try changing r_crop_ISO to layer
    
    
    if (length(r_crop_ISO_lc_rcl_agg_values) == 3) {
      st_read(paste0("data/", ISO, "/", crop, "/v_crop_ISO_lc_rcl_agg1.shp")) %>%
        st_make_valid() %>%
        mutate(LC = factor(layer,
                           labels = c(
                             "Not Cropland", "Cropland", paste0("'", crop, "'")
                           ))) %>%
        dplyr::filter(layer > 0) %>%
        st_intersection(v_ISO1) %>%
        mutate(crop_ISO1 = paste(NAME_1, LC, sep = '_'))
      
    } else if (identical(r_crop_ISO_lc_rcl_agg_values,
                         r_crop_ISO_lc_rcl_agg_values_vec0)) {
      st_read(paste0("data/", ISO, "/", crop, "/v_crop_ISO_lc_rcl_agg1.shp")) %>%
        st_make_valid() %>%
        mutate(LC = factor(layer,
                           labels = c("Not Cropland"))) %>%
        dplyr::filter(layer > 0) %>%
        st_intersection(v_ISO1) %>%
        mutate(crop_ISO1 = paste(NAME_1, LC, sep = '_'))
      
    } else if (identical(r_crop_ISO_lc_rcl_agg_values,
                         r_crop_ISO_lc_rcl_agg_values_vec1)) {
      st_read(paste0("data/", ISO, "/", crop, "/v_crop_ISO_lc_rcl_agg1.shp")) %>%
        st_make_valid() %>%
        mutate(LC = factor(layer,
                           labels = c("Cropland"))) %>%
        dplyr::filter(layer > 0) %>%
        st_intersection(v_ISO1) %>%
        mutate(crop_ISO1 = paste(NAME_1, LC, sep = '_'))
      
    } else if (identical(r_crop_ISO_lc_rcl_agg_values,
                         r_crop_ISO_lc_rcl_agg_values_vec2)) {
      st_read(paste0("data/", ISO, "/", crop, "/v_crop_ISO_lc_rcl_agg1.shp")) %>%
        st_make_valid() %>%
        mutate(LC = factor(layer,
                           labels = c(paste0("'", crop, "'")))) %>%
        dplyr::filter(layer > 0) %>%
        st_intersection(v_ISO1) %>%
        mutate(crop_ISO1 = paste(NAME_1, LC, sep = '_'))
      
    } else if (identical(r_crop_ISO_lc_rcl_agg_values,
                         r_crop_ISO_lc_rcl_agg_values_vec01)) {
      st_read(paste0("data/", ISO, "/", crop, "/v_crop_ISO_lc_rcl_agg1.shp")) %>%
        st_make_valid() %>%
        mutate(LC = factor(layer,
                           labels = c("Not Cropland", "Cropland"))) %>%
        dplyr::filter(layer > 0) %>%
        st_intersection(v_ISO1) %>%
        mutate(crop_ISO1 = paste(NAME_1, LC, sep = '_'))
      
      # vect(paste0("data/", ISO, "/", crop, "/v_crop_ISO_lc_rcl_agg1.shp")) %>%
      # mutate(LC = factor(r_crop_ISO,
      # labels = c("Not Cropland","Cropland")
      # )) %>%
      # dplyr::filter(r_crop_ISO > 0) %>%
      # intersect(vect(v_ISO1)) %>%
      # mutate(crop_ISO1 = paste(NAME_1, LC, sep = '_'))
      
      
      
      
    } else if (identical(r_crop_ISO_lc_rcl_agg_values,
                         r_crop_ISO_lc_rcl_agg_values_vec02)) {
      st_read(paste0("data/", ISO, "/", crop, "/v_crop_ISO_lc_rcl_agg1.shp")) %>%
        st_make_valid() %>%
        mutate(LC = factor(layer,
                           labels = c("Not Cropland", paste0("'", crop, "'")))) %>%
        dplyr::filter(layer > 0) %>%
        st_intersection(v_ISO1) %>%
        mutate(crop_ISO1 = paste(NAME_1, LC, sep = '_'))
      
    } else {
      st_read(paste0("data/", ISO, "/", crop, "/v_crop_ISO_lc_rcl_agg1.shp")) %>%
        st_make_valid() %>%
        mutate(LC = factor(layer,
                           labels = c("Cropland", paste0("'", crop, "'")))) %>%
        dplyr::filter(layer > 0) %>%
        st_intersection(v_ISO1) %>%
        mutate(crop_ISO1 = paste(NAME_1, LC, sep = '_'))
    }
  }


sf::sf_use_s2(FALSE)

# results in v_crop_ISO_lc_rcl_agg_file2
v_crop_ISO_lc_rcl_agg2_make_write_f <-
  function(v_crop_ISO_lc_rcl_agg,  ISO, crop) {
    v_crop_ISO_lc_rcl_agg %>%
      st_as_sf %>%
      write_sf(paste0("data/", ISO, "/", crop, "/v_crop_ISO_lc_rcl_agg.shp"),
               overwrite = TRUE)
  }

# results in v_crop_ISO_lc_rcl_agg_plot
v_crop_ISO_lc_rcl_agg_plot_f <-
  function(v_crop_ISO_lc_rcl_agg,  v_ISO1, ISO, crop)  {
    ggplot() +
      geom_sf(
        data = v_crop_ISO_lc_rcl_agg,
        aes(fill = crop_ISO1),
        col = NA,
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = v_ISO1,
        fill = NA,
        col = 'black',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      guides(fill = "none") +
      labs(title = paste0("Sourcing Areas - ", ISO, " - ", crop))
  }

### ----- Project the crop/landuse vector to Equal Area projection and get stats----- ###

# results in wkt_lam
wkt_lam_make_f <- function(v_ISO_extent) {
  paste0(
    "PROJCRS[\"unknown\",
    BASEGEOGCRS[\"unknown\",
        DATUM[\"World Geodetic System 1984\",
            ELLIPSOID[\"WGS 84\",6378137,298.257223563,
                LENGTHUNIT[\"metre\",1]],
            ID[\"EPSG\",6326]],
        PRIMEM[\"Greenwich\",0,
            ANGLEUNIT[\"degree\",0.0174532925199433],
            ID[\"EPSG\",8901]]],
    CONVERSION[\"unknown\",
        METHOD[\"Lambert Azimuthal Equal Area\",
            ID[\"EPSG\",9820]],
        PARAMETER[\"Latitude of natural origin\",",
    v_ISO_extent@ymin,
    ",ANGLEUNIT[\"degree\",0.0174532925199433],
            ID[\"EPSG\",8801]],
        PARAMETER[\"Longitude of natural origin\",",
    v_ISO_extent@xmin,
    ",ANGLEUNIT[\"degree\",0.0174532925199433],
            ID[\"EPSG\",8802]],
        PARAMETER[\"False easting\",1000000,
            LENGTHUNIT[\"metre\",1],
            ID[\"EPSG\",8806]],
        PARAMETER[\"False northing\",1000000,
            LENGTHUNIT[\"metre\",1],
            ID[\"EPSG\",8807]]],
    CS[Cartesian,2],
        AXIS[\"(E)\",east,
            ORDER[1],
            LENGTHUNIT[\"metre\",1,
                ID[\"EPSG\",9001]]],
        AXIS[\"(N)\",north,
            ORDER[2],
            LENGTHUNIT[\"metre\",1,
                ID[\"EPSG\",9001]]]]"
  )
}

# results in crs_lam
crs_lam_make_f <- function(wkt_lam) {
  st_crs(wkt_lam)
}

# results in v_crop_ISO_lc_rcl_agg_proj_file
v_crop_ISO_lc_rcl_agg_proj_file_make_write_f <-
  function(v_crop_ISO_lc_rcl_agg, crs_lam, ISO, crop) {
    st_transform(v_crop_ISO_lc_rcl_agg, crs_lam) %>%
      write_sf(paste0("data/", ISO, "/", crop, "/v_crop_ISO_lc_rcl_agg_proj.shp"),
               overwrite = TRUE)
  }

# results in v_crop_ISO_lc_rcl_agg_proj
v_crop_ISO_lc_rcl_agg_proj_get_f <-
  function(v_crop_ISO_lc_rcl_agg_proj_file,
           ISO,
           crop) {
    st_read(paste0("data/", ISO, "/", crop, "/v_crop_ISO_lc_rcl_agg_proj.shp"))
  }

# results in dB_crop_ISO_lc_rcl_agg_proj
dB_crop_ISO_lc_rcl_agg_proj_make_f <-
  function(v_crop_ISO_lc_rcl_agg_proj,
           v_ISO1,
           ISO,
           crop) {
    v_crop_ISO_lc_rcl_agg_proj %>%
      mutate(land_km2  = geometry %>% st_area() %>% set_units(km ^ 2) %>% as.integer()) %>%
      as_tibble() %>%
      dplyr::select(
        !c(
          GID_0,
          GID_1,
          VARNAME_1,
          NL_NAME_1,
          TYPE_1,
          ENGTYPE_1,
          CC_1,
          HASC_1,
          New_ID,
          geometry
        )
      ) %>%
      right_join(dplyr::select(v_ISO1,!geometry)) %>%
      dplyr::select(
        !c(
          GID_0,
          GID_1,
          VARNAME_1,
          NL_NAME_1,
          TYPE_1,
          ENGTYPE_1,
          CC_1,
          HASC_1,
          New_ID,
          geometry
        )
      ) %T>%
      write_excel_csv(paste0("data/", ISO, "/", crop, "/dB_crop_ISO_lc_rcl_agg_proj.csv"), na = "0", quote = "all",
                      append = FALSE)
  }

# results in dB_crop_ISO_lc_rcl_agg_proj_plot
dB_crop_ISO_lc_rcl_agg_proj_plot_f <-
  function(dB_crop_ISO_lc_rcl_agg_proj) {
    
    ggplot(dB_crop_ISO_lc_rcl_agg_proj,                  # Barplot using ggplot2
           aes(x = crop_ISO1,
               y = land_km2,
               fill = crop_ISO1)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      scale_y_continuous(labels = comma) +
      labs(title = "Sourcing Areas - Land Area") +
      labs(y = "Land Area (km^2)") +
      labs(x = "Sourcing Area") +
      geom_text(y = 0, aes(label = paste(land_km2), angle = 90), hjust = 0) +
      theme(
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      ) 
    
      
  }


### ----- Trimmed Climate Mask ----- ###

# results in r_clim_mask_trim_file
r_clim_mask_trim_file_make_write_f <-
  function(v_ISO, r_clim_mask, ISO) {
    #aggregate((r_lc_ISO / r_lc_ISO), (res(r_clim_mask)/res(r_lc_ISO)), fun=modal) %>%
    #resample(r_clim_mask, method="ngb",
    #  filename = paste0("data/", ISO, "/r_clim_mask_trim.tif"), overwrite = TRUE)
    #resample(r_ISO, r_clim_mask, method="ngb", filename = paste0("data/", ISO, "/r_clim_mask_trim.tif"), overwrite = TRUE)
    terra::rasterize(
      vect(v_ISO),
      rast(r_clim_mask),
      field = 1,
      background = NA,
      touches
      = TRUE
    ) %>% raster() %>% writeRaster(paste0("data/", ISO, "/r_clim_mask_trim.tif"), overwrite = TRUE)
  }

# results in r_clim_mask_trim
r_clim_mask_trim_get_f <- function(ISO, r_clim_mask_trim_file) {
  raster(paste0("data/", ISO, "/r_clim_mask_trim.tif"))
}


### ----- Climate Base Data ----- ###

### --- Rainfall ---

### Load original data trim and save and get

# results in r_rainfallc_file
r_rainfallc_make_write_f <-
  function(ISO,
           crop,
           cc_data,
           cc_row,
           r_clim_mask_trim) {
    (raster(paste0(
      "data/", ISO, "/", crop, "/", paste(cc_data[cc_row, 14])
    )) * r_clim_mask_trim) %>%
      writeRaster(paste0("data/", ISO, "/", crop, "/r_rainfallc.tif"),
                  overwrite = TRUE)
  }

# results in r_rainfallc
r_rainfallc_get_f <- function(ISO, crop, r_rainfallc_file) {
  raster(paste0("data/", ISO, "/", crop, "/r_rainfallc.tif"))
}

# results in r_rainfallf_file
r_rainfallf_make_write_f <-
  function(ISO,
           crop,
           cc_data,
           cc_row,
           r_clim_mask_trim) {
    (raster(paste0(
      "data/", ISO, "/", crop, "/", paste(cc_data[cc_row, 15])
    )) * r_clim_mask_trim) %>%
      writeRaster(paste0("data/", ISO, "/", crop, "/r_rainfallf.tif"),
                  overwrite = TRUE)
  }

# results in r_rainfallf
r_rainfallf_get_f <- function(ISO, crop, r_rainfallf_file) {
  raster(paste0("data/", ISO, "/", crop, "/r_rainfallf.tif"))
}

### --- Temperature ---

### Load original data trim and save and get

# results in r_tempc_file
r_tempc_make_write_f <-
  function(ISO,
           crop,
           cc_data,
           cc_row,
           r_clim_mask_trim) {
    (raster(paste0(
      "data/", ISO, "/", crop, "/", paste(cc_data[cc_row, 16])
    )) * r_clim_mask_trim) %>%
      writeRaster(paste0("data/", ISO, "/", crop, "/r_tempc.tif"), overwrite = TRUE)
  }

# results in r_tempc
r_tempc_get_f <- function(ISO, crop, r_tempc_file) {
  raster(paste0("data/", ISO, "/", crop, "/r_tempc.tif"))
}

# results in r_tempf_file
r_tempf_make_write_f <-
  function(ISO,
           crop,
           cc_data,
           cc_row,
           r_clim_mask_trim) {
    (raster(paste0(
      "data/", ISO, "/", crop, "/", paste(cc_data[cc_row, 17])
    )) * r_clim_mask_trim) %>%
      writeRaster(paste0("data/", ISO, "/", crop, "/r_tempf.tif"), overwrite = TRUE)
  }

# results in r_tempf
r_tempf_get_f <- function(ISO, crop, r_tempf_file) {
  raster(paste0("data/", ISO, "/", crop, "/r_tempf.tif"))
}

### --- Season Onset ---

# results in r_onsetc_file
r_onsetc_make_write_f <-
  function(ISO,
           crop,
           cc_data,
           cc_row,
           r_clim_mask_trim) {
    (raster(paste0(
      "data/", ISO, "/", crop, "/", paste(cc_data[cc_row, 18])
    )) * r_clim_mask_trim) %>%
      writeRaster(paste0("data/", ISO, "/", crop, "/r_onsetc.tif"), overwrite = TRUE)
  }

# results in r_onsetc
r_onsetc_get_f <- function(ISO, crop, r_onsetc_file) {
  raster(paste0("data/", ISO, "/", crop, "/r_onsetc.tif"))
}

# results in r_onsetf_file
r_onsetf_make_write_f <-
  function(ISO,
           crop,
           cc_data,
           cc_row,
           r_clim_mask_trim) {
    (raster(paste0(
      "data/", ISO, "/", crop, "/", paste(cc_data[cc_row, 19])
    )) * r_clim_mask_trim) %>%
      writeRaster(paste0("data/", ISO, "/", crop, "/r_onsetf.tif"), overwrite = TRUE)
  }

# results in r_onsetf
r_onsetf_get_f <- function(ISO, crop, r_onsetf_file) {
  raster(paste0("data/", ISO, "/", crop, "/r_onsetf.tif"))
}

### --- Season Duration ---

# results in r_durationc_file
r_durationc_make_write_f <-
  function(ISO,
           crop,
           cc_data,
           cc_row,
           r_clim_mask_trim) {
    (raster(paste0(
      "data/", ISO, "/", crop, "/", paste(cc_data[cc_row, 20])
    )) * r_clim_mask_trim) %>%
      writeRaster(paste0("data/", ISO, "/", crop, "/r_durationc.tif"),
                  overwrite = TRUE)
  }

# results in r_durationc
r_durationc_get_f <- function(ISO, crop, r_durationc_file) {
  raster(paste0("data/", ISO, "/", crop, "/r_durationc.tif"))
}

# results in r_durationf_file
r_durationf_make_write_f <-
  function(ISO,
           crop,
           cc_data,
           cc_row,
           r_clim_mask_trim) {
    (raster(paste0(
      "data/", ISO, "/", crop, "/", paste(cc_data[cc_row, 21])
    )) * r_clim_mask_trim) %>%
      writeRaster(paste0("data/", ISO, "/", crop, "/r_durationf.tif"),
                  overwrite = TRUE)
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
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
          "--------------------\nPast\nRainfall\nr_rainfallc\n",
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
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
          "--------------------\nFuture\nRainfall\nr_rainfallf\n",
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
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
          "--------------------\nPast\nMean\nTemperature\nr_tempc\n",
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
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
          "--------------------\nFuture\nMean\nTemperature\nr_tempf\n",
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
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
          "--------------------\nPast\nSeason\nOnset\nr_onsetc\n",
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
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
          "--------------------\nFuture\nSeason\nOnset\nr_onsetf\n",
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
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
          "--------------------\nPast\nSeason\nDuration\nr_durationc\n",
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
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
          "--------------------\nFuture\nSeason\nDuration\nr_durationf\n",
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
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
          "--------------------\nChange\nRainfall\nr_rainfall_change\n",
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
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
          "--------------------\nChange\nMean\nTemperature\nr_temp_change\n",
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
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
          "--------------------\nChange\nSeason\nOnset\nr_onset_change\n",
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
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
          "--------------------\nChange\nSeason\nDuration\nr_duration_change\n",
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
r_droughtc_make_write_f <-
  function(ISO,
           crop,
           cc_data,
           cc_row,
           r_clim_mask_trim) {
    (raster(paste0(
      "data/", ISO, "/", crop, "/", paste(cc_data[cc_row, 6])
    )) * r_clim_mask_trim) %>%
      writeRaster(paste0("data/", ISO, "/", crop, "/r_droughtc.tif"),
                  overwrite = TRUE)
  }

# results in r_droughtc
r_droughtc_get_f <- function(ISO, crop, r_droughtc_file) {
  paste0("data/", ISO, "/", crop, "/r_droughtc.tif") %>% raster()
}

# results in r_droughtf_file
r_droughtf_make_write_f <-
  function(ISO,
           crop,
           cc_data,
           cc_row,
           r_clim_mask_trim) {
    (raster(paste0(
      "data/", ISO, "/", crop, "/", paste(cc_data[cc_row, 7])
    )) * r_clim_mask_trim) %>%
      writeRaster(paste0("data/", ISO, "/", crop, "/r_droughtf.tif"),
                  overwrite = TRUE)
  }

# results in r_droughtf
r_droughtf_get_f <- function(ISO, crop, r_droughtf_file) {
  paste0("data/", ISO, "/", crop, "/r_droughtf.tif") %>% raster()
}

# results in r_heatc_file
r_heatc_make_write_f <-
  function(ISO,
           crop,
           cc_data,
           cc_row,
           r_clim_mask_trim) {
    (raster(paste0(
      "data/", ISO, "/", crop, "/", paste(cc_data[cc_row, 9])
    )) * r_clim_mask_trim) %>%
      writeRaster(paste0("data/", ISO, "/", crop, "/r_heatc.tif"), overwrite = TRUE)
  }

# results in r_heatc
r_heatc_get_f <- function(ISO, crop, r_heatc_file) {
  paste0("data/", ISO, "/", crop, "/r_heatc.tif") %>% raster()
}

# results in r_heatf_file
r_heatf_make_write_f <-
  function(ISO,
           crop,
           cc_data,
           cc_row,
           r_clim_mask_trim) {
    (raster(paste0(
      "data/", ISO, "/", crop, "/", paste(cc_data[cc_row, 10])
    )) * r_clim_mask_trim) %>%
      writeRaster(paste0("data/", ISO, "/", crop, "/r_heatf.tif"), overwrite = TRUE)
  }

# results in r_heatf
r_heatf_get_f <- function(ISO, crop, r_heatf_file) {
  paste0("data/", ISO, "/", crop, "/r_heatf.tif") %>% raster()
}

# results in r_floodc_file
r_floodc_make_write_f <-
  function(ISO,
           crop,
           cc_data,
           cc_row,
           r_clim_mask_trim) {
    (raster(paste0(
      "data/", ISO, "/", crop, "/", paste(cc_data[cc_row, 12])
    )) * r_clim_mask_trim) %>%
      writeRaster(paste0("data/", ISO, "/", crop, "/r_floodc.tif"), overwrite = TRUE)
  }

# results in r_floodc
r_floodc_get_f <- function(ISO, crop, r_floodc_file) {
  paste0("data/", ISO, "/", crop, "/r_floodc.tif") %>% raster()
}

# results in r_floodf_file
r_floodf_make_write_f <-
  function(ISO,
           crop,
           cc_data,
           cc_row,
           r_clim_mask_trim) {
    (raster(paste0(
      "data/", ISO, "/", crop, "/", paste(cc_data[cc_row, 13])
    )) * r_clim_mask_trim) %>%
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
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
          "\nr_droughtc\n",
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
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
          "\nr_droughtf\n",
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
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
          "\nr_drought_change\n",
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
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
          "\nr_heatc\n",
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
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
          "\nr_heatf\n",
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
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
          "\nr_heat_change\n",
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
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
          "\nr_floodc\n",
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
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
          "\nr_floodf\n",
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
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
          "\nr_flood_change\n",
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
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
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
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

# results in dB_rainfall_change_summary
dB_rainfall_change_summary_make_f <-
  function(r_rainfall_change,
           v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      r_rainfall_change,
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
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
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
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
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
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
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
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
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
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
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
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
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
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
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
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
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

# results in dB_duration_change_summary
dB_duration_change_summary_make_f <-
  function(r_duration_change,
           v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      r_duration_change,
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
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
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
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
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
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
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
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
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
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
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
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
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
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
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
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
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
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
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
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
dB_rainfallc_summary_plot_f <-
  function(dB_rainfallc_summary, ISO, crop) {
    ggplot(dB_rainfallc_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "Annual Rainfall") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0("Past Rainfall - ",
                          ISO,
                          " - ",
                          crop)) +
      ylim(0, 2000) +
      theme(
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }

# results in dB_rainfallf_summary_plot
dB_rainfallf_summary_plot_f <-
  function(dB_rainfallf_summary, ISO, crop) {
    ggplot(dB_rainfallf_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "Annual Rainfall") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0("Future Rainfall - ",
                          ISO,
                          " - ",
                          crop)) +
      ylim(0, 2000) +
      theme(
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }

# results in dB_rainfall_change_summary_plot
dB_rainfall_change_summary_plot_f <-
  function(dB_rainfall_change_summary, ISO, crop) {
    ggplot(dB_rainfall_change_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "Annual Rainfall") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0("Change in Rainfall - ",
                          ISO,
                          " - ",
                          crop)) +
      ylim(-500, 500) +
      geom_hline(
        yintercept = 0,
        linetype = 'dashed',
        colour = 'red',
        size = 1
      ) +
      theme(
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }

### Temperature

# results in dB_tempc_summary_plot
dB_tempc_summary_plot_f <- function(dB_tempc_summary, ISO, crop) {
  ggplot(dB_tempc_summary,
         aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
    geom_boxplot(aes(
      ymin = max_min,
      lower = q25,
      middle = median,
      upper = q75,
      ymax = min_max
    ),
    stat = "identity") +
    guides(fill = "none") +
    scale_y_continuous(labels = comma) +
    labs(y = "Annual Temperature") +
    labs(x = "Sourcing Area")  +
    labs(title = paste0("Past Temperature - ",
                        ISO,
                        " - ",
                        crop)) +
    ylim(10, 35) +
    theme(
      panel.grid.major.x = element_line(colour = "grey"),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_text(
        angle = 90,
        hjust = 1,
        vjust = 0.5,
        size = 7
      )
    )
}

# results in dB_tempf_summary_plot
dB_tempf_summary_plot_f <- function(dB_tempf_summary, ISO, crop) {
  ggplot(dB_tempf_summary,
         aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
    geom_boxplot(aes(
      ymin = max_min,
      lower = q25,
      middle = median,
      upper = q75,
      ymax = min_max
    ),
    stat = "identity") +
    guides(fill = "none") +
    scale_y_continuous(labels = comma) +
    labs(y = "Annual Temperature") +
    labs(x = "Sourcing Area")  +
    labs(title = paste0("Future Temperature - ",
                        ISO,
                        " - ",
                        crop)) +
    ylim(10, 35) +
    theme(
      panel.grid.major.x = element_line(colour = "grey"),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_text(
        angle = 90,
        hjust = 1,
        vjust = 0.5,
        size = 7
      )
    )
}

# results in dB_temp_change_summary_plot
dB_temp_change_summary_plot_f <-
  function(dB_temp_change_summary, ISO, crop) {
    ggplot(dB_temp_change_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "Annual Temperature") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0("Change in Temperature - ",
                          ISO,
                          " - ",
                          crop)) +
      ylim(-3, 3) +
      geom_hline(
        yintercept = 0,
        linetype = 'dashed',
        colour = 'red',
        size = 1
      ) +
      theme(
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }

### Season Onset

# results in dB_onsetc_summary_plot
dB_onsetc_summary_plot_f <- function(dB_onsetc_summary, ISO, crop) {
  ggplot(dB_onsetc_summary,
         aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
    geom_boxplot(aes(
      ymin = max_min,
      lower = q25,
      middle = median,
      upper = q75,
      ymax = min_max
    ),
    stat = "identity") +
    guides(fill = "none") +
    scale_y_continuous(labels = comma) +
    labs(y = "Day number") +
    labs(x = "Sourcing Area")  +
    labs(title = paste0("Past Season Onset - ",
                        ISO,
                        " - ",
                        crop)) +
    ylim(0, 365) +
    theme(
      panel.grid.major.x = element_line(colour = "grey"),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_text(
        angle = 90,
        hjust = 1,
        vjust = 0.5,
        size = 7
      )
    )
}

# results in dB_onsetf_summary_plot
dB_onsetf_summary_plot_f <- function(dB_onsetf_summary, ISO, crop) {
  ggplot(dB_onsetf_summary,
         aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
    geom_boxplot(aes(
      ymin = max_min,
      lower = q25,
      middle = median,
      upper = q75,
      ymax = min_max
    ),
    stat = "identity") +
    guides(fill = "none") +
    scale_y_continuous(labels = comma) +
    labs(y = "Day number") +
    labs(x = "Sourcing Area")  +
    labs(title = paste0("Future Season Onset - ",
                        ISO,
                        " - ",
                        crop)) +
    ylim(0, 365) +
    theme(
      panel.grid.major.x = element_line(colour = "grey"),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_text(
        angle = 90,
        hjust = 1,
        vjust = 0.5,
        size = 7
      )
    )
}

# results in dB_onset_change_summary_plot
dB_onset_change_summary_plot_f <-
  function(dB_onset_change_summary, ISO, crop) {
    ggplot(dB_onset_change_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "Day number") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0("Change in Season Onset - ",
                          ISO,
                          " - ",
                          crop)) +
      ylim(-50, 50) +
      geom_hline(
        yintercept = 0,
        linetype = 'dashed',
        colour = 'red',
        size = 1
      ) +
      theme(
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }

### Season Duration

# results in dB_durationc_summary_plot
dB_durationc_summary_plot_f <-
  function(dB_durationc_summary, ISO, crop) {
    ggplot(dB_durationc_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "Days") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0("Past Season Duration - ",
                          ISO,
                          " - ",
                          crop)) +
      ylim(0, 365) +
      theme(
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }

# results in dB_durationf_summary_plot
dB_durationf_summary_plot_f <-
  function(dB_durationf_summary, ISO, crop) {
    ggplot(dB_durationf_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "Days") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0("Future Season Duration - ",
                          ISO,
                          " - ",
                          crop)) +
      ylim(0, 365) +
      theme(
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }

# results in dB_duration_change_summary_plot
dB_duration_change_summary_plot_f <-
  function(dB_duration_change_summary, ISO, crop) {
    ggplot(dB_duration_change_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "Days") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0("Change in Season Duration - ",
                          ISO,
                          " - ",
                          crop)) +
      ylim(-150, 150) +
      geom_hline(
        yintercept = 0,
        linetype = 'dashed',
        colour = 'red',
        size = 1
      ) +
      theme(
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }

### Drought

# results in dB_droughtc_summary_plot
dB_droughtc_summary_plot_f <-
  function(dB_droughtc_summary,
           drought_threshold_l,
           drought_threshold_u,
           drought,
           ISO,
           crop) {
    ggplot(dB_droughtc_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "# of seasons with hazard") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0("Past ", drought, " - ",
                          ISO,
                          " - ",
                          crop)) +
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
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }

# results in dB_droughtf_summary_plot
dB_droughtf_summary_plot_f <-
  function(dB_droughtf_summary,
           drought_threshold_l,
           drought_threshold_u,
           drought,
           ISO,
           crop) {
    ggplot(dB_droughtf_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "# of seasons with hazard") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0("Future ", drought, " - ",
                          ISO,
                          " - ",
                          crop)) +
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
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }

# results in dB_drought_change_summary_plot
dB_drought_change_summary_plot_f <-
  function(dB_drought_change_summary,
           drought_threshold_l,
           drought_threshold_u,
           drought,
           ISO,
           crop) {
    ggplot(dB_drought_change_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "Change in frequency") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0("Change in ", drought, " - ",
                          ISO,
                          " - ",
                          crop)) +
      ylim(-20, 20) +
      geom_hline(
        yintercept = 0,
        linetype = 'dashed',
        colour = 'red',
        size = 1
      ) +
      theme(
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }


### Heat Stress

# results in dB_heatc_summary_plot
dB_heatc_summary_plot_f <-
  function(dB_heatc_summary,
           heat_threshold_l,
           heat_threshold_u,
           heat,
           ISO,
           crop) {
    ggplot(dB_heatc_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "# of seasons with hazard") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0("Past ", heat, " - ",
                          ISO,
                          " - ",
                          crop)) +
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
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }

# results in dB_heatf_summary_plot
dB_heatf_summary_plot_f <-
  function(dB_heatf_summary,
           heat_threshold_l,
           heat_threshold_u,
           heat,
           ISO,
           crop) {
    ggplot(dB_heatf_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "# of seasons with hazard") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0("Future ", heat, " - ",
                          ISO,
                          " - ",
                          crop)) +
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
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }

# results in dB_heat_change_summary_plot
dB_heat_change_summary_plot_f <-
  function(dB_heat_change_summary,
           heat_threshold_l,
           heat_threshold_u,
           heat,
           ISO,
           crop) {
    ggplot(dB_heat_change_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "Change in frequency") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0("Change in ", heat, " - ",
                          ISO,
                          " - ",
                          crop)) +
      ylim(-20, 20) +
      geom_hline(
        yintercept = 0,
        linetype = 'dashed',
        colour = 'red',
        size = 1
      ) +
      theme(
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }


### Waterlogging

# results in dB_floodc_summary_plot
dB_floodc_summary_plot_f <-
  function(dB_floodc_summary,
           flood_threshold_l,
           flood_threshold_u,
           flood,
           ISO,
           crop) {
    ggplot(dB_floodc_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "Average # per period") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0("Past ", flood, " - ",
                          ISO,
                          " - ",
                          crop)) +
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
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }

# results in dB_floodf_summary_plot
dB_floodf_summary_plot_f <-
  function(dB_floodf_summary,
           flood_threshold_l,
           flood_threshold_u,
           flood,
           ISO,
           crop) {
    ggplot(dB_floodf_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "Average # per period") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0("Future ", flood, " - ",
                          ISO,
                          " - ",
                          crop)) +
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
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }

# results in dB_flood_change_summary_plot
dB_flood_change_summary_plot_f <-
  function(dB_flood_change_summary,
           flood_threshold_l,
           flood_threshold_u,
           flood,
           ISO,
           crop) {
    ggplot(dB_flood_change_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "Change in average # per period") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0("Change in ", flood, " - ",
                          ISO,
                          " - ",
                          crop)) +
      ylim(-10, 10) +
      geom_hline(
        yintercept = 0,
        linetype = 'dashed',
        colour = 'red',
        size = 1
      ) +
      theme(
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }


### ----- Impact maps and charts ----- ###

### Prepare Impact Data

# results in dB_impact_prelim
dB_impact_prelim_make_f <- function(r_droughtc) {
  coordinates(rasterToPoints(r_droughtc)) %>% as_tibble() %>% dplyr::select(x, y)
}

# results in xy_impact
xy_impact_make_f <- function(dB_impact_prelim) {
  cbind(pull(dB_impact_prelim, x), pull(dB_impact_prelim, y))
}

# results in newcol_droughtc
newcol_droughtc_make_f <- function(xy_impact, r_droughtc) {
  raster::extract(r_droughtc, xy_impact)
}

# results in newcol_droughtf
newcol_droughtf_make_f <- function(xy_impact, r_droughtf) {
  raster::extract(r_droughtf, xy_impact)
}

# results in newcol_heatc
newcol_heatc_make_f <- function(xy_impact, r_heatc) {
  raster::extract(r_heatc, xy_impact)
}

# results in newcol_heatf
newcol_heatf_make_f <- function(xy_impact, r_heatf) {
  raster::extract(r_heatf, xy_impact)
}

# results in newcol_floodc
newcol_floodc_make_f <- function(xy_impact, r_floodc) {
  raster::extract(r_floodc, xy_impact)
}

# results in newcol_floodf
newcol_floodf_make_f <- function(xy_impact, r_floodf) {
  raster::extract(r_floodf, xy_impact)
}

# results in dB_impact
dB_impact_make_f <-
  function(dB_impact_prelim,
           newcol_droughtc,
           newcol_droughtf,
           newcol_heatc,
           newcol_heatf,
           newcol_floodc,
           newcol_floodf) {
    dB_impact_prelim %>% mutate(droughtc = newcol_droughtc) %>% mutate(droughtf = newcol_droughtf) %>% mutate(heatc = newcol_heatc) %>% mutate(heatf = newcol_heatf) %>% mutate(floodc = newcol_floodc) %>% mutate(floodf = newcol_floodf)
  }

### Fuzzy Partitions

#### construct Fuzzy Partitions

# results in fp_drought_l
fp_drought_l_make_f <-
  function(drought_threshold_l,
           drought_threshold_l_width) {
    LinearFuzzyPartition(
      level = c("low", "high"),
      crossoverPoint = drought_threshold_l,
      transitionWidth = drought_threshold_l_width
    )
  }

# results in fp_drought_u
fp_drought_u_make_f <-
  function(drought_threshold_u,
           drought_threshold_u_width) {
    LinearFuzzyPartition(
      level = c("low", "high"),
      crossoverPoint = drought_threshold_u,
      transitionWidth = drought_threshold_u_width
    )
  }

# results in fp_heat_l
fp_heat_l_make_f <-
  function(heat_threshold_l,
           heat_threshold_l_width) {
    LinearFuzzyPartition(
      level = c("low", "high"),
      crossoverPoint = heat_threshold_l,
      transitionWidth = heat_threshold_l_width
    )
  }

# results in fp_heat_u
fp_heat_u_make_f <-
  function(heat_threshold_u,
           heat_threshold_u_width) {
    LinearFuzzyPartition(
      level = c("low", "high"),
      crossoverPoint = heat_threshold_u,
      transitionWidth = heat_threshold_u_width
    )
  }

# results in fp_flood_l
fp_flood_l_make_f <-
  function(flood_threshold_l,
           flood_threshold_l_width) {
    LinearFuzzyPartition(
      level = c("low", "high"),
      crossoverPoint = flood_threshold_l,
      transitionWidth = flood_threshold_l_width
    )
  }

# results in fp_flood_u
fp_flood_u_make_f <-
  function(flood_threshold_u,
           flood_threshold_u_width) {
    LinearFuzzyPartition(
      level = c("low", "high"),
      crossoverPoint = flood_threshold_u,
      transitionWidth = flood_threshold_u_width
    )
  }

#### plot Fuzzy Partitions

# results in fp_drought_l_plot
fp_drought_l_plot_f <- function(fp_drought_l, drought) {
  plot(
    fp_drought_l,
    n = 10,
    xlim = c(0, 20),
    xlab = paste(drought),
    title = ""
  )
}

# results in fp_drought_u_plot
fp_drought_u_plot_f <- function(fp_drought_u, drought) {
  plot(
    fp_drought_u,
    n = 10,
    xlim = c(0, 20),
    xlab = paste(drought),
    title = ""
  )
}

# results in fp_heat_l_plot
fp_heat_l_plot_f <- function(fp_heat_l, heat) {
  plot(
    fp_heat_l,
    n = 10,
    xlim = c(0, 20),
    xlab = paste(heat),
    title = ""
  )
}

# results in fp_heat_u_plot
fp_heat_u_plot_f <- function(fp_heat_u, heat) {
  plot(
    fp_heat_u,
    n = 10,
    xlim = c(0, 20),
    xlab = paste(heat),
    title = ""
  )
}

# results in fp_flood_l_plot
fp_flood_l_plot_f <- function(fp_flood_l, flood) {
  plot(
    fp_flood_l,
    n = 10,
    xlim = c(0, 20),
    xlab = paste(flood),
    title = ""
  )
}

# results in fp_flood_u_plot
fp_flood_u_plot_f <- function(fp_flood_u, flood) {
  plot(
    fp_flood_u,
    n = 10,
    xlim = c(0, 20),
    xlab = paste(flood),
    title = ""
  )
}

### Single Hazard Rule Base Construction

# results in rb_droughtc
rb_droughtc_make_f <- function(drought) {
  RuleBase(new("Proposition", table = tibble(
    !!paste0(as.character("droughtc")) := c("low", "high")
  )),
  new("Conclusion", table = tibble(
    !!paste0(as.character("droughtc"),
             "_o") := c("optimal", "suboptimal")
  )))
}


# results in rb_heatc
rb_heatc_make_f <- function(heat) {
  RuleBase(new("Proposition", table = tibble(
    !!paste0(as.character("heatc")) := c("low", "high")
  )),
  new("Conclusion", table = tibble(
    !!paste0(as.character("heatc"),
             "_o") := c("optimal", "suboptimal")
  )))
}

# results in rb_floodc
rb_floodc_make_f <- function(flood) {
  RuleBase(new("Proposition", table = tibble(
    !!paste0(as.character("floodc")) := c("low", "high")
  )),
  new("Conclusion", table = tibble(
    !!paste0(as.character("floodc"),
             "_o") := c("optimal", "suboptimal")
  )))
}

# results in rb_droughtf
rb_droughtf_make_f <- function(drought) {
  RuleBase(new("Proposition", table = tibble(
    !!paste0(as.character("droughtf")) := c("low", "high")
  )),
  new("Conclusion", table = tibble(
    !!paste0(as.character("droughtf"),
             "_o") := c("optimal", "suboptimal")
  )))
}

# results in rb_heatf
rb_heatf_make_f <- function(heat) {
  RuleBase(new("Proposition", table = tibble(
    !!paste0(as.character("heatf")) := c("low", "high")
  )),
  new("Conclusion", table = tibble(
    !!paste0(as.character("heatf"),
             "_o") := c("optimal", "suboptimal")
  )))
}

# results in rb_floodf
rb_floodf_make_f <- function(flood) {
  RuleBase(new("Proposition", table = tibble(
    !!paste0(as.character("floodf")) := c("low", "high")
  )),
  new("Conclusion", table = tibble(
    !!paste0(as.character("floodf"),
             "_o") := c("optimal", "suboptimal")
  )))
}

### Single Hazard Fuzzy Partition Matrix Prediction lower


# results in fpm_droughtc_l
fpm_droughtc_l_make_f <-
  function(rb_droughtc, dB_impact, fp_drought_l) {
    predict(rb_droughtc,
            newdata = dB_impact,
            droughtc = fp_drought_l)
    
  }

# results in fpm_heatc_l
fpm_heatc_l_make_f <- function(rb_heatc, dB_impact, fp_heat_l) {
  predict(rb_heatc,
          newdata = dB_impact,
          heatc = fp_heat_l)
  
}

# results in fpm_floodc_l
fpm_floodc_l_make_f <- function(rb_floodc, dB_impact, fp_flood_l) {
  predict(rb_floodc,
          newdata = dB_impact,
          floodc = fp_flood_l)
  
}


# results in fpm_droughtf_l
fpm_droughtf_l_make_f <-
  function(rb_droughtf, dB_impact, fp_drought_l) {
    predict(rb_droughtf,
            newdata = dB_impact,
            droughtf = fp_drought_l)
    
  }

# results in fpm_heatf_l
fpm_heatf_l_make_f <- function(rb_heatf, dB_impact, fp_heat_l) {
  predict(rb_heatf,
          newdata = dB_impact,
          heatf = fp_heat_l)
  
}

# results in fpm_floodf_l
fpm_floodf_l_make_f <- function(rb_floodf, dB_impact, fp_flood_l) {
  predict(rb_floodf,
          newdata = dB_impact,
          floodf = fp_flood_l)
  
}

### Single Hazard Fuzzy Partition Matrix Prediction upper


# results in fpm_droughtc_u
fpm_droughtc_u_make_f <-
  function(rb_droughtc, dB_impact, fp_drought_u) {
    predict(rb_droughtc,
            newdata = dB_impact,
            droughtc = fp_drought_u)
    
  }

# results in fpm_heatc_u
fpm_heatc_u_make_f <- function(rb_heatc, dB_impact, fp_heat_u) {
  predict(rb_heatc,
          newdata = dB_impact,
          heatc = fp_heat_u)
  
}

# results in fpm_floodc_u
fpm_floodc_u_make_f <- function(rb_floodc, dB_impact, fp_flood_u) {
  predict(rb_floodc,
          newdata = dB_impact,
          floodc = fp_flood_u)
  
}


# results in fpm_droughtf_u
fpm_droughtf_u_make_f <-
  function(rb_droughtf, dB_impact, fp_drought_u) {
    predict(rb_droughtf,
            newdata = dB_impact,
            droughtf = fp_drought_u)
    
  }

# results in fpm_heatf_u
fpm_heatf_u_make_f <- function(rb_heatf, dB_impact, fp_heat_u) {
  predict(rb_heatf,
          newdata = dB_impact,
          heatf = fp_heat_u)
  
}

# results in fpm_floodf_u
fpm_floodf_u_make_f <- function(rb_floodf, dB_impact, fp_flood_u) {
  predict(rb_floodf,
          newdata = dB_impact,
          floodf = fp_flood_u)
  
}

### Multiple Hazard Rule Base Construction

# results in rb_droughtc_heatc
rb_droughtc_heatc_make_f <- function(drought, heat) {
  RuleBase(new("Proposition", table = cross_df(tibble(
    !!paste0(as.character("droughtc")) := c("low", "high"),!!paste0(as.character("heatc")) := c("low", "high")
  ))),
  new("Conclusion", table = tibble(
    !!paste0(as.character("droughtc_heatc_o")) := c("optimal", "drought", "heat", "drought_heat")
  )))
}

# results in rb_droughtc_floodc
rb_droughtc_floodc_make_f <- function(drought, flood) {
  RuleBase(new("Proposition", table = cross_df(tibble(
    !!paste0(as.character("droughtc")) := c("low", "high"),!!paste0(as.character("floodc")) := c("low", "high")
  ))),
  new("Conclusion", table = tibble(
    !!paste0(as.character("droughtc_floodc_o")) := c("optimal", "drought", "flood", "drought_flood")
  )))
}

# results in rb_heatc_floodc
rb_heatc_floodc_make_f <- function(heat, flood) {
  RuleBase(new("Proposition", table = cross_df(tibble(
    !!paste0(as.character("heatc")) := c("low", "high"),!!paste0(as.character("floodc")) := c("low", "high")
  ))),
  new("Conclusion", table = tibble(
    !!paste0(as.character("heatc_floodc_o")) := c("optimal", "heat", "flood", "heat_flood")
  )))
}

# results in rb_droughtc_heatc_floodc
rb_droughtc_heatc_floodc_make_f <- function(drought, heat, flood) {
  RuleBase(new("Proposition", table = cross_df(
    tibble(
      !!paste0(as.character("droughtc")) := c("low", "high"),
      !!paste0(as.character("heatc")) := c("low", "high"),
      !!paste0(as.character("floodc")) := c("low", "high")
    )
  )),
  new("Conclusion", table = tibble(
    !!paste0(as.character("droughtc_heatc_floodc_o")) := c(
      "optimal",
      "drought",
      "heat",
      "drought_heat",
      "flood",
      "drought_flood",
      "heat_flood",
      "drought_heat_flood"
    )
  )))
}

# results in rb_droughtf_heatf
rb_droughtf_heatf_make_f <- function(drought, heat) {
  RuleBase(new("Proposition", table = cross_df(tibble(
    !!paste0(as.character("droughtf")) := c("low", "high"),!!paste0(as.character("heatf")) := c("low", "high")
  ))),
  new("Conclusion", table = tibble(
    !!paste0(as.character("droughtf_heatf_o")) := c("optimal", "drought", "heat", "drought_heat")
  )))
}

# results in rb_droughtf_floodf
rb_droughtf_floodf_make_f <- function(drought, flood) {
  RuleBase(new("Proposition", table = cross_df(tibble(
    !!paste0(as.character("droughtf")) := c("low", "high"),!!paste0(as.character("floodf")) := c("low", "high")
  ))),
  new("Conclusion", table = tibble(
    !!paste0(as.character("droughtf_floodf_o")) := c("optimal", "drought", "flood", "drought_flood")
  )))
}

# results in rb_heatf_floodf
rb_heatf_floodf_make_f <- function(heat, flood) {
  RuleBase(new("Proposition", table = cross_df(tibble(
    !!paste0(as.character("heatf")) := c("low", "high"),!!paste0(as.character("floodf")) := c("low", "high")
  ))),
  new("Conclusion", table = tibble(
    !!paste0(as.character("heatf_floodf_o")) := c("optimal", "heat", "flood", "heat_flood")
  )))
}

# results in rb_droughtf_heatf_floodf
rb_droughtf_heatf_floodf_make_f <- function(drought, heat, flood) {
  RuleBase(new("Proposition", table = cross_df(
    tibble(
      !!paste0(as.character("droughtf")) := c("low", "high"),
      !!paste0(as.character("heatf")) := c("low", "high"),
      !!paste0(as.character("floodf")) := c("low", "high")
    )
  )),
  new("Conclusion", table = tibble(
    !!paste0(as.character("droughtf_heatf_floodf_o")) := c(
      "optimal",
      "drought",
      "heat",
      "drought_heat",
      "flood",
      "drought_flood",
      "heat_flood",
      "drought_heat_flood"
    )
  )))
}

### Multiple Hazard Fuzzy Partition Matrix Prediction

#### Lower Threshold

# results in fpm_droughtc_l_heatc_l
fpm_droughtc_l_heatc_l_make_f <-
  function(rb_droughtc_heatc,
           dB_impact,
           fp_drought_l,
           fp_heat_l) {
    predict(
      rb_droughtc_heatc,
      newdata = dB_impact,
      droughtc = fp_drought_l,
      heatc = fp_heat_l
    )
  }

# results in fpm_droughtc_l_floodc_l
fpm_droughtc_l_floodc_l_make_f <-
  function(rb_droughtc_floodc,
           dB_impact,
           fp_drought_l,
           fp_flood_l) {
    predict(
      rb_droughtc_floodc,
      newdata = dB_impact,
      droughtc = fp_drought_l,
      floodc = fp_flood_l
    )
  }

# results in fpm_heatc_l_floodc_l
fpm_heatc_l_floodc_l_make_f <-
  function(rb_heatc_floodc,
           dB_impact,
           fp_heat_l,
           fp_flood_l) {
    predict(
      rb_heatc_floodc,
      newdata = dB_impact,
      heatc = fp_heat_l,
      floodc = fp_flood_l
    )
  }

# results in fpm_droughtc_l_heatc_l_floodc_l
fpm_droughtc_l_heatc_l_floodc_l_make_f <-
  function(rb_droughtc_heatc_floodc,
           dB_impact,
           fp_drought_l,
           fp_heat_l,
           fp_flood_l) {
    predict(
      rb_droughtc_heatc_floodc,
      newdata = dB_impact,
      droughtc = fp_drought_l,
      heatc = fp_heat_l,
      floodc = fp_flood_l
    )
  }

# results in fpm_droughtf_l_heatf_l
fpm_droughtf_l_heatf_l_make_f <-
  function(rb_droughtf_heatf,
           dB_impact,
           fp_drought_l,
           fp_heat_l) {
    predict(
      rb_droughtf_heatf,
      newdata = dB_impact,
      droughtf = fp_drought_l,
      heatf = fp_heat_l
    )
  }

# results in fpm_droughtf_l_floodf_l
fpm_droughtf_l_floodf_l_make_f <-
  function(rb_droughtf_floodf,
           dB_impact,
           fp_drought_l,
           fp_flood_l) {
    predict(
      rb_droughtf_floodf,
      newdata = dB_impact,
      droughtf = fp_drought_l,
      floodf = fp_flood_l
    )
  }

# results in fpm_heatf_l_floodf_l
fpm_heatf_l_floodf_l_make_f <-
  function(rb_heatf_floodf,
           dB_impact,
           fp_heat_l,
           fp_flood_l) {
    predict(
      rb_heatf_floodf,
      newdata = dB_impact,
      heatf = fp_heat_l,
      floodf = fp_flood_l
    )
  }

# results in fpm_droughtf_l_heatf_l_floodf_l
fpm_droughtf_l_heatf_l_floodf_l_make_f <-
  function(rb_droughtf_heatf_floodf,
           dB_impact,
           fp_drought_l,
           fp_heat_l,
           fp_flood_l) {
    predict(
      rb_droughtf_heatf_floodf,
      newdata = dB_impact,
      droughtf = fp_drought_l,
      heatf = fp_heat_l,
      floodf = fp_flood_l
    )
  }

#### Upper Threshold

# results in fpm_droughtc_u_heatc_u
fpm_droughtc_u_heatc_u_make_f <-
  function(rb_droughtc_heatc,
           dB_impact,
           fp_drought_u,
           fp_heat_u) {
    predict(
      rb_droughtc_heatc,
      newdata = dB_impact,
      droughtc = fp_drought_u,
      heatc = fp_heat_u
    )
  }

# results in fpm_droughtc_u_floodc_u
fpm_droughtc_u_floodc_u_make_f <-
  function(rb_droughtc_floodc,
           dB_impact,
           fp_drought_u,
           fp_flood_u) {
    predict(
      rb_droughtc_floodc,
      newdata = dB_impact,
      droughtc = fp_drought_u,
      floodc = fp_flood_u
    )
  }

# results in fpm_heatc_u_floodc_u
fpm_heatc_u_floodc_u_make_f <-
  function(rb_heatc_floodc,
           dB_impact,
           fp_heat_u,
           fp_flood_u) {
    predict(
      rb_heatc_floodc,
      newdata = dB_impact,
      heatc = fp_heat_u,
      floodc = fp_flood_u
    )
  }

# results in fpm_droughtc_u_heatc_u_floodc_u
fpm_droughtc_u_heatc_u_floodc_u_make_f <-
  function(rb_droughtc_heatc_floodc,
           dB_impact,
           fp_drought_u,
           fp_heat_u,
           fp_flood_u) {
    predict(
      rb_droughtc_heatc_floodc,
      newdata = dB_impact,
      droughtc = fp_drought_u,
      heatc = fp_heat_u,
      floodc = fp_flood_u
    )
  }

# results in fpm_droughtf_u_heatf_u
fpm_droughtf_u_heatf_u_make_f <-
  function(rb_droughtf_heatf,
           dB_impact,
           fp_drought_u,
           fp_heat_u) {
    predict(
      rb_droughtf_heatf,
      newdata = dB_impact,
      droughtf = fp_drought_u,
      heatf = fp_heat_u
    )
  }

# results in fpm_droughtf_u_floodf_u
fpm_droughtf_u_floodf_u_make_f <-
  function(rb_droughtf_floodf,
           dB_impact,
           fp_drought_u,
           fp_flood_u) {
    predict(
      rb_droughtf_floodf,
      newdata = dB_impact,
      droughtf = fp_drought_u,
      floodf = fp_flood_u
    )
  }

# results in fpm_heatf_u_floodf_u
fpm_heatf_u_floodf_u_make_f <-
  function(rb_heatf_floodf,
           dB_impact,
           fp_heat_u,
           fp_flood_u) {
    predict(
      rb_heatf_floodf,
      newdata = dB_impact,
      heatf = fp_heat_u,
      floodf = fp_flood_u
    )
  }

# results in fpm_droughtf_u_heatf_u_floodf_u
fpm_droughtf_u_heatf_u_floodf_u_make_f <-
  function(rb_droughtf_heatf_floodf,
           dB_impact,
           fp_drought_u,
           fp_heat_u,
           fp_flood_u) {
    predict(
      rb_droughtf_heatf_floodf,
      newdata = dB_impact,
      droughtf = fp_drought_u,
      heatf = fp_heat_u,
      floodf = fp_flood_u
    )
  }


#### Mixed Thresholds

##### Past


# results in fpm_droughtc_l_heatc_u
fpm_droughtc_l_heatc_u_make_f <-
  function(rb_droughtc_heatc,
           dB_impact,
           fp_drought_l,
           fp_heat_u) {
    predict(
      rb_droughtc_heatc,
      newdata = dB_impact,
      droughtc = fp_drought_l,
      heatc = fp_heat_u
    )
  }

# results in fpm_droughtc_u_heatc_l
fpm_droughtc_u_heatc_l_make_f <-
  function(rb_droughtc_heatc,
           dB_impact,
           fp_drought_u,
           fp_heat_l) {
    predict(
      rb_droughtc_heatc,
      newdata = dB_impact,
      droughtc = fp_drought_u,
      heatc = fp_heat_l
    )
  }

# results in fpm_droughtc_l_floodc_u
fpm_droughtc_l_floodc_u_make_f <-
  function(rb_droughtc_floodc,
           dB_impact,
           fp_drought_l,
           fp_flood_u) {
    predict(
      rb_droughtc_floodc,
      newdata = dB_impact,
      droughtc = fp_drought_l,
      floodc = fp_flood_u
    )
  }

# results in fpm_droughtc_u_floodc_l
fpm_droughtc_u_floodc_l_make_f <-
  function(rb_droughtc_floodc,
           dB_impact,
           fp_drought_u,
           fp_flood_l) {
    predict(
      rb_droughtc_floodc,
      newdata = dB_impact,
      droughtc = fp_drought_u,
      floodc = fp_flood_l
    )
  }

# results in fpm_heatc_l_floodc_u
fpm_heatc_l_floodc_u_make_f <-
  function(rb_heatc_floodc,
           dB_impact,
           fp_heat_l,
           fp_flood_u) {
    predict(
      rb_heatc_floodc,
      newdata = dB_impact,
      heatc = fp_heat_l,
      floodc = fp_flood_u
    )
  }

# results in fpm_heatc_u_floodc_l
fpm_heatc_u_floodc_l_make_f <-
  function(rb_heatc_floodc,
           dB_impact,
           fp_heat_u,
           fp_flood_l) {
    predict(
      rb_heatc_floodc,
      newdata = dB_impact,
      heatc = fp_heat_u,
      floodc = fp_flood_l
    )
  }

# results in fpm_droughtc_l_heatc_l_floodc_u
fpm_droughtc_l_heatc_l_floodc_u_make_f <-
  function(rb_droughtc_heatc_floodc,
           dB_impact,
           fp_drought_l,
           fp_heat_l,
           fp_flood_u) {
    predict(
      rb_droughtc_heatc_floodc,
      newdata = dB_impact,
      droughtc = fp_drought_l,
      heatc = fp_heat_l,
      floodc = fp_flood_u
    )
  }

# results in fpm_droughtc_l_heatc_u_floodc_l
fpm_droughtc_l_heatc_u_floodc_l_make_f <-
  function(rb_droughtc_heatc_floodc,
           dB_impact,
           fp_drought_l,
           fp_heat_u,
           fp_flood_l) {
    predict(
      rb_droughtc_heatc_floodc,
      newdata = dB_impact,
      droughtc = fp_drought_l,
      heatc = fp_heat_u,
      floodc = fp_flood_l
    )
  }

# results in fpm_droughtc_u_heatc_l_floodc_u
fpm_droughtc_u_heatc_l_floodc_u_make_f <-
  function(rb_droughtc_heatc_floodc,
           dB_impact,
           fp_drought_u,
           fp_heat_l,
           fp_flood_u) {
    predict(
      rb_droughtc_heatc_floodc,
      newdata = dB_impact,
      droughtc = fp_drought_u,
      heatc = fp_heat_l,
      floodc = fp_flood_u
    )
  }

# results in fpm_droughtc_u_heatc_l_floodc_l
fpm_droughtc_u_heatc_l_floodc_l_make_f <-
  function(rb_droughtc_heatc_floodc,
           dB_impact,
           fp_drought_u,
           fp_heat_l,
           fp_flood_l) {
    predict(
      rb_droughtc_heatc_floodc,
      newdata = dB_impact,
      droughtc = fp_drought_u,
      heatc = fp_heat_l,
      floodc = fp_flood_l
    )
  }

# results in fpm_droughtc_u_heatc_u_floodc_l
fpm_droughtc_u_heatc_u_floodc_l_make_f <-
  function(rb_droughtc_heatc_floodc,
           dB_impact,
           fp_drought_u,
           fp_heat_u,
           fp_flood_l) {
    predict(
      rb_droughtc_heatc_floodc,
      newdata = dB_impact,
      droughtc = fp_drought_u,
      heatc = fp_heat_u,
      floodc = fp_flood_l
    )
  }

# results in fpm_droughtc_l_heatc_u_floodc_u
fpm_droughtc_l_heatc_u_floodc_u_make_f <-
  function(rb_droughtc_heatc_floodc,
           dB_impact,
           fp_drought_l,
           fp_heat_u,
           fp_flood_u) {
    predict(
      rb_droughtc_heatc_floodc,
      newdata = dB_impact,
      droughtc = fp_drought_l,
      heatc = fp_heat_u,
      floodc = fp_flood_u
    )
  }

##### Future

# results in fpm_droughtf_l_heatf_u
fpm_droughtf_l_heatf_u_make_f <-
  function(rb_droughtf_heatf,
           dB_impact,
           fp_drought_l,
           fp_heat_u) {
    predict(
      rb_droughtf_heatf,
      newdata = dB_impact,
      droughtf = fp_drought_l,
      heatf = fp_heat_u
    )
  }

# results in fpm_droughtf_u_heatf_l
fpm_droughtf_u_heatf_l_make_f <-
  function(rb_droughtf_heatf,
           dB_impact,
           fp_drought_u,
           fp_heat_l) {
    predict(
      rb_droughtf_heatf,
      newdata = dB_impact,
      droughtf = fp_drought_u,
      heatf = fp_heat_l
    )
  }

# results in fpm_droughtf_l_floodf_u
fpm_droughtf_l_floodf_u_make_f <-
  function(rb_droughtf_floodf,
           dB_impact,
           fp_drought_l,
           fp_flood_u) {
    predict(
      rb_droughtf_floodf,
      newdata = dB_impact,
      droughtf = fp_drought_l,
      floodf = fp_flood_u
    )
  }

# results in fpm_droughtf_u_floodf_l
fpm_droughtf_u_floodf_l_make_f <-
  function(rb_droughtf_floodf,
           dB_impact,
           fp_drought_u,
           fp_flood_l) {
    predict(
      rb_droughtf_floodf,
      newdata = dB_impact,
      droughtf = fp_drought_u,
      floodf = fp_flood_l
    )
  }

# results in fpm_heatf_l_floodf_u
fpm_heatf_l_floodf_u_make_f <-
  function(rb_heatf_floodf,
           dB_impact,
           fp_heat_l,
           fp_flood_u) {
    predict(
      rb_heatf_floodf,
      newdata = dB_impact,
      heatf = fp_heat_l,
      floodf = fp_flood_u
    )
  }

# results in fpm_heatf_u_floodf_l
fpm_heatf_u_floodf_l_make_f <-
  function(rb_heatf_floodf,
           dB_impact,
           fp_heat_u,
           fp_flood_l) {
    predict(
      rb_heatf_floodf,
      newdata = dB_impact,
      heatf = fp_heat_u,
      floodf = fp_flood_l
    )
  }

# results in fpm_droughtf_l_heatf_l_floodf_u
fpm_droughtf_l_heatf_l_floodf_u_make_f <-
  function(rb_droughtf_heatf_floodf,
           dB_impact,
           fp_drought_l,
           fp_heat_l,
           fp_flood_u) {
    predict(
      rb_droughtf_heatf_floodf,
      newdata = dB_impact,
      droughtf = fp_drought_l,
      heatf = fp_heat_l,
      floodf = fp_flood_u
    )
  }

# results in fpm_droughtf_l_heatf_u_floodf_l
fpm_droughtf_l_heatf_u_floodf_l_make_f <-
  function(rb_droughtf_heatf_floodf,
           dB_impact,
           fp_drought_l,
           fp_heat_u,
           fp_flood_l) {
    predict(
      rb_droughtf_heatf_floodf,
      newdata = dB_impact,
      droughtf = fp_drought_l,
      heatf = fp_heat_u,
      floodf = fp_flood_l
    )
  }

# results in fpm_droughtf_u_heatf_l_floodf_u
fpm_droughtf_u_heatf_l_floodf_u_make_f <-
  function(rb_droughtf_heatf_floodf,
           dB_impact,
           fp_drought_u,
           fp_heat_l,
           fp_flood_u) {
    predict(
      rb_droughtf_heatf_floodf,
      newdata = dB_impact,
      droughtf = fp_drought_u,
      heatf = fp_heat_l,
      floodf = fp_flood_u
    )
  }

# results in fpm_droughtf_u_heatf_l_floodf_l
fpm_droughtf_u_heatf_l_floodf_l_make_f <-
  function(rb_droughtf_heatf_floodf,
           dB_impact,
           fp_drought_u,
           fp_heat_l,
           fp_flood_l) {
    predict(
      rb_droughtf_heatf_floodf,
      newdata = dB_impact,
      droughtf = fp_drought_u,
      heatf = fp_heat_l,
      floodf = fp_flood_l
    )
  }

# results in fpm_droughtf_u_heatf_u_floodf_l
fpm_droughtf_u_heatf_u_floodf_l_make_f <-
  function(rb_droughtf_heatf_floodf,
           dB_impact,
           fp_drought_u,
           fp_heat_u,
           fp_flood_l) {
    predict(
      rb_droughtf_heatf_floodf,
      newdata = dB_impact,
      droughtf = fp_drought_u,
      heatf = fp_heat_u,
      floodf = fp_flood_l
    )
  }

# results in fpm_droughtf_l_heatf_u_floodf_u
fpm_droughtf_l_heatf_u_floodf_u_make_f <-
  function(rb_droughtf_heatf_floodf,
           dB_impact,
           fp_drought_l,
           fp_heat_u,
           fp_flood_u) {
    predict(
      rb_droughtf_heatf_floodf,
      newdata = dB_impact,
      droughtf = fp_drought_l,
      heatf = fp_heat_u,
      floodf = fp_flood_u
    )
  }

### Finalise Impact Data

# results in dB_impact_full
dB_impact_full_make_f <- function(dB_impact,
                                  fpm_droughtc_l,
                                  fpm_heatc_l,
                                  fpm_floodc_l,
                                  fpm_droughtf_l,
                                  fpm_heatf_l,
                                  fpm_floodf_l,
                                  fpm_droughtc_l_heatc_l,
                                  fpm_droughtc_l_floodc_l,
                                  fpm_heatc_l_floodc_l,
                                  fpm_droughtc_l_heatc_l_floodc_l,
                                  fpm_droughtf_l_heatf_l,
                                  fpm_droughtf_l_floodf_l,
                                  fpm_heatf_l_floodf_l,
                                  fpm_droughtf_l_heatf_l_floodf_l,
                                  fpm_droughtc_u,
                                  fpm_heatc_u,
                                  fpm_floodc_u,
                                  fpm_droughtf_u,
                                  fpm_heatf_u,
                                  fpm_floodf_u,
                                  fpm_droughtc_u_heatc_u,
                                  fpm_droughtc_u_floodc_u,
                                  fpm_heatc_u_floodc_u,
                                  fpm_droughtc_u_heatc_u_floodc_u,
                                  fpm_droughtf_u_heatf_u,
                                  fpm_droughtf_u_floodf_u,
                                  fpm_heatf_u_floodf_u,
                                  fpm_droughtf_u_heatf_u_floodf_u,
                                  fpm_droughtc_l_heatc_u,
                                  fpm_droughtc_u_heatc_l,
                                  fpm_droughtc_l_floodc_u,
                                  fpm_droughtc_u_floodc_l,
                                  fpm_heatc_l_floodc_u,
                                  fpm_heatc_u_floodc_l,
                                  fpm_droughtc_l_heatc_l_floodc_u,
                                  fpm_droughtc_l_heatc_u_floodc_l,
                                  fpm_droughtc_u_heatc_l_floodc_u,
                                  fpm_droughtc_u_heatc_l_floodc_l,
                                  fpm_droughtc_u_heatc_u_floodc_l,
                                  fpm_droughtc_l_heatc_u_floodc_u,
                                  fpm_droughtf_l_heatf_u,
                                  fpm_droughtf_u_heatf_l,
                                  fpm_droughtf_l_floodf_u,
                                  fpm_droughtf_u_floodf_l,
                                  fpm_heatf_l_floodf_u,
                                  fpm_heatf_u_floodf_l,
                                  fpm_droughtf_l_heatf_l_floodf_u,
                                  fpm_droughtf_l_heatf_u_floodf_l,
                                  fpm_droughtf_u_heatf_l_floodf_u,
                                  fpm_droughtf_u_heatf_l_floodf_l,
                                  fpm_droughtf_u_heatf_u_floodf_l,
                                  fpm_droughtf_l_heatf_u_floodf_u) {
  dB_impact %>% mutate(droughtc_l_s = getMembership(fpm_droughtc_l$suboptimal)) %>%
    
    # lower thresholds
    
    mutate(droughtc_l_o = getMembership(fpm_droughtc_l$optimal)) %>%
    mutate(heatc_l_s = getMembership(fpm_heatc_l$suboptimal)) %>%
    mutate(heatc_l_o = getMembership(fpm_heatc_l$optimal)) %>%
    mutate(floodc_l_s = getMembership(fpm_floodc_l$suboptimal)) %>%
    mutate(floodc_l_o = getMembership(fpm_floodc_l$optimal)) %>%
    mutate(droughtf_l_s = getMembership(fpm_droughtf_l$suboptimal)) %>%
    mutate(droughtf_l_o = getMembership(fpm_droughtf_l$optimal)) %>%
    mutate(heatf_l_s = getMembership(fpm_heatf_l$suboptimal)) %>%
    mutate(heatf_l_o = getMembership(fpm_heatf_l$optimal)) %>%
    mutate(floodf_l_s = getMembership(fpm_floodf_l$suboptimal)) %>%
    mutate(floodf_l_o = getMembership(fpm_floodf_l$optimal)) %>%
    
    mutate(droughtchange_l_o  = (droughtf_l_o -  droughtc_l_o)) %>%
    mutate(droughtchange_l_s  = (droughtf_l_s -  droughtc_l_s)) %>%
    mutate(heatchange_l_o  = (heatf_l_o -  heatc_l_o)) %>%
    mutate(heatchange_l_s  = (heatf_l_s -  heatc_l_s)) %>%
    mutate(floodchange_l_o  = (floodf_l_o -  floodc_l_o)) %>%
    mutate(floodchange_l_s  = (floodf_l_s -  floodc_l_s)) %>%
    
    mutate(droughtc_l_heatc_l_o = getMembership(fpm_droughtc_l_heatc_l$optimal)) %>%
    mutate(droughtc_l_heatc_l_d = getMembership(fpm_droughtc_l_heatc_l$drought)) %>%
    mutate(droughtc_l_heatc_l_h = getMembership(fpm_droughtc_l_heatc_l$heat)) %>%
    mutate(droughtc_l_heatc_l_dh = getMembership(fpm_droughtc_l_heatc_l$drought_heat)) %>%
    mutate(droughtc_l_heatc_l_max =

               
               # A Farrow 10/01/2023 precedence rules required for values == 0.5
               # easiest is to use the order in which the risk classes appear
               # therefore change > operators to >= operators
               
               
               # A Farrow 09/02/2023 sometimes the 0.5 threshold is too high for instance when 
               # four classes have a value of 0.25
               # so need to include the suboptimal option as an explicit if statement
               # then the else is a mixture
           
             ifelse(               
               droughtc_l_heatc_l_o >= 0.5, 
               0,
               ifelse(
                 droughtc_l_heatc_l_d >= 0.5,
                 1,
                 ifelse(droughtc_l_heatc_l_h >= 0.5,
                    2,
                    ifelse(droughtc_l_heatc_l_dh >= 0.5, 4, 8)) # this is now explicit and where 8 is now an 'uncertain' risk profile
               )
             )) %>%
    
    mutate(droughtc_l_floodc_l_o = getMembership(fpm_droughtc_l_floodc_l$optimal)) %>%
    mutate(droughtc_l_floodc_l_d = getMembership(fpm_droughtc_l_floodc_l$drought)) %>%
    mutate(droughtc_l_floodc_l_f = getMembership(fpm_droughtc_l_floodc_l$flood)) %>%
    mutate(droughtc_l_floodc_l_df = getMembership(fpm_droughtc_l_floodc_l$drought_flood)) %>%
    mutate(droughtc_l_floodc_l_max =
             ifelse(
               droughtc_l_floodc_l_o >= 0.5,
               0,
               ifelse(
                 droughtc_l_floodc_l_d >= 0.5,
                 1,
                 ifelse(droughtc_l_floodc_l_f >= 0.5, 
                    3,
                      ifelse(droughtc_l_floodc_l_df >= 0.5, 5, 8))
               )
             )) %>%
    
    mutate(heatc_l_floodc_l_o = getMembership(fpm_heatc_l_floodc_l$optimal)) %>%
    mutate(heatc_l_floodc_l_h = getMembership(fpm_heatc_l_floodc_l$heat)) %>%
    mutate(heatc_l_floodc_l_f = getMembership(fpm_heatc_l_floodc_l$flood)) %>%
    mutate(heatc_l_floodc_l_hf = getMembership(fpm_heatc_l_floodc_l$heat_flood)) %>%
    mutate(heatc_l_floodc_l_max =
             ifelse(
               heatc_l_floodc_l_o >= 0.5,
               0,
               ifelse(
                 heatc_l_floodc_l_h >= 0.5,
                 2,
                 ifelse(heatc_l_floodc_l_f >= 0.5,
                        3,
                        ifelse(heatc_l_floodc_l_hf >= 0.5, 6, 8))
               )
             )) %>%
    
    mutate(droughtc_l_heatc_l_floodc_l_o = getMembership(fpm_droughtc_l_heatc_l_floodc_l$optimal)) %>%
    mutate(droughtc_l_heatc_l_floodc_l_d = getMembership(fpm_droughtc_l_heatc_l_floodc_l$drought)) %>%
    mutate(droughtc_l_heatc_l_floodc_l_h = getMembership(fpm_droughtc_l_heatc_l_floodc_l$heat)) %>%
    mutate(droughtc_l_heatc_l_floodc_l_f = getMembership(fpm_droughtc_l_heatc_l_floodc_l$flood)) %>%
    mutate(
      droughtc_l_heatc_l_floodc_l_dh = getMembership(fpm_droughtc_l_heatc_l_floodc_l$drought_heat)
    ) %>%
    mutate(
      droughtc_l_heatc_l_floodc_l_df = getMembership(fpm_droughtc_l_heatc_l_floodc_l$drought_flood)
    ) %>%
    mutate(droughtc_l_heatc_l_floodc_l_hf = getMembership(fpm_droughtc_l_heatc_l_floodc_l$heat_flood)) %>%
    mutate(
      droughtc_l_heatc_l_floodc_l_dhf = getMembership(fpm_droughtc_l_heatc_l_floodc_l$drought_heat_flood)
    ) %>%
    mutate(
      droughtc_l_heatc_l_floodc_l_max =
        ifelse(
          droughtc_l_heatc_l_floodc_l_o >= 0.5,
          0,
          ifelse(
            droughtc_l_heatc_l_floodc_l_d >= 0.5,
            1,
            ifelse(
              droughtc_l_heatc_l_floodc_l_h >= 0.5,
              2,
              ifelse(
                droughtc_l_heatc_l_floodc_l_f >= 0.5,
                3,
                ifelse(
                  droughtc_l_heatc_l_floodc_l_dh >= 0.5,
                  4,
                  ifelse(
                    droughtc_l_heatc_l_floodc_l_df >= 0.5,
                    5,
                    ifelse(
                      droughtc_l_heatc_l_floodc_l_hf >= 0.5,
                      6,
                      ifelse(
                        droughtc_l_heatc_l_floodc_l_dhf >= 0.5, 7, 8)
                    )
                  )
                )
              )
            )
          )
        )
    ) %>%
    
    mutate(droughtf_l_heatf_l_o = getMembership(fpm_droughtf_l_heatf_l$optimal)) %>%
    mutate(droughtf_l_heatf_l_d = getMembership(fpm_droughtf_l_heatf_l$drought)) %>%
    mutate(droughtf_l_heatf_l_h = getMembership(fpm_droughtf_l_heatf_l$heat)) %>%
    mutate(droughtf_l_heatf_l_dh = getMembership(fpm_droughtf_l_heatf_l$drought_heat)) %>%
    mutate(droughtf_l_heatf_l_max =
             ifelse(
               droughtf_l_heatf_l_o >= 0.5,
               0,
               ifelse(
                 droughtf_l_heatf_l_d >= 0.5,
                 1,
                 ifelse(droughtf_l_heatf_l_h >= 0.5, 
                    2, 
                    ifelse(droughtf_l_heatf_l_dh >= 0.5, 4, 8)
                 )
               )
             )) %>%
    
    mutate(droughtf_l_floodf_l_o = getMembership(fpm_droughtf_l_floodf_l$optimal)) %>%
    mutate(droughtf_l_floodf_l_d = getMembership(fpm_droughtf_l_floodf_l$drought)) %>%
    mutate(droughtf_l_floodf_l_f = getMembership(fpm_droughtf_l_floodf_l$flood)) %>%
    mutate(droughtf_l_floodf_l_df = getMembership(fpm_droughtf_l_floodf_l$drought_flood)) %>%
    mutate(droughtf_l_floodf_l_max =
             ifelse(
               droughtf_l_floodf_l_o >= 0.5,
               0,
               ifelse(
                 droughtf_l_floodf_l_d >= 0.5,
                 1,
                 ifelse(droughtf_l_floodf_l_f >= 0.5,
                    3,
                    ifelse(droughtf_l_floodf_l_df >= 0.5,  5, 8)
                 )
               )
             )) %>%
    
    mutate(heatf_l_floodf_l_o = getMembership(fpm_heatf_l_floodf_l$optimal)) %>%
    mutate(heatf_l_floodf_l_h = getMembership(fpm_heatf_l_floodf_l$heat)) %>%
    mutate(heatf_l_floodf_l_f = getMembership(fpm_heatf_l_floodf_l$flood)) %>%
    mutate(heatf_l_floodf_l_hf = getMembership(fpm_heatf_l_floodf_l$heat_flood)) %>%
    mutate(heatf_l_floodf_l_max =
             ifelse(
               heatf_l_floodf_l_o >= 0.5,
               0,
               ifelse(
                 heatf_l_floodf_l_h >= 0.5,
                 2,
                 ifelse(heatf_l_floodf_l_f >= 0.5, 
                    3, 
                    ifelse(heatf_l_floodf_l_hf >= 0.5, 6, 8)
                 )
               )
             )) %>%
    
    
    mutate(droughtf_l_heatf_l_floodf_l_o = getMembership(fpm_droughtf_l_heatf_l_floodf_l$optimal)) %>%
    mutate(droughtf_l_heatf_l_floodf_l_d = getMembership(fpm_droughtf_l_heatf_l_floodf_l$drought)) %>%
    mutate(droughtf_l_heatf_l_floodf_l_h = getMembership(fpm_droughtf_l_heatf_l_floodf_l$heat)) %>%
    mutate(droughtf_l_heatf_l_floodf_l_f = getMembership(fpm_droughtf_l_heatf_l_floodf_l$flood)) %>%
    mutate(
      droughtf_l_heatf_l_floodf_l_dh = getMembership(fpm_droughtf_l_heatf_l_floodf_l$drought_heat)
    ) %>%
    mutate(
      droughtf_l_heatf_l_floodf_l_df = getMembership(fpm_droughtf_l_heatf_l_floodf_l$drought_flood)
    ) %>%
    mutate(droughtf_l_heatf_l_floodf_l_hf = getMembership(fpm_droughtf_l_heatf_l_floodf_l$heat_flood)) %>%
    mutate(
      droughtf_l_heatf_l_floodf_l_dhf = getMembership(fpm_droughtf_l_heatf_l_floodf_l$drought_heat_flood)
    ) %>%
    mutate(
      droughtf_l_heatf_l_floodf_l_max =
        ifelse(
          droughtf_l_heatf_l_floodf_l_o >= 0.5,
          0,
          ifelse(
            droughtf_l_heatf_l_floodf_l_d >= 0.5,
            1,
            ifelse(
              droughtf_l_heatf_l_floodf_l_h >= 0.5,
              2,
              ifelse(
                droughtf_l_heatf_l_floodf_l_f >= 0.5,
                3,
                ifelse(
                  droughtf_l_heatf_l_floodf_l_dh >= 0.5,
                  4,
                  ifelse(
                    droughtf_l_heatf_l_floodf_l_df >= 0.5,
                    5,
                    ifelse(
                      droughtf_l_heatf_l_floodf_l_hf >= 0.5, 
                      6, 
                      ifelse(
                        droughtf_l_heatf_l_floodf_l_dhf >= 0.5, 7, 8)
                    )
                  )
                )
              )
            )
          )
        )
    ) %>%
    # upper thresholds
    
    mutate(droughtc_u_s = getMembership(fpm_droughtc_u$suboptimal)) %>%
    mutate(droughtc_u_o = getMembership(fpm_droughtc_u$optimal)) %>%
    mutate(heatc_u_s = getMembership(fpm_heatc_u$suboptimal)) %>%
    mutate(heatc_u_o = getMembership(fpm_heatc_u$optimal)) %>%
    mutate(floodc_u_s = getMembership(fpm_floodc_u$suboptimal)) %>%
    mutate(floodc_u_o = getMembership(fpm_floodc_u$optimal)) %>%
    mutate(droughtf_u_s = getMembership(fpm_droughtf_u$suboptimal)) %>%
    mutate(droughtf_u_o = getMembership(fpm_droughtf_u$optimal)) %>%
    mutate(heatf_u_s = getMembership(fpm_heatf_u$suboptimal)) %>%
    mutate(heatf_u_o = getMembership(fpm_heatf_u$optimal)) %>%
    mutate(floodf_u_s = getMembership(fpm_floodf_u$suboptimal)) %>%
    mutate(floodf_u_o = getMembership(fpm_floodf_u$optimal)) %>%
    
    mutate(droughtchange_u_o  = (droughtf_u_o -  droughtc_u_o)) %>%
    mutate(droughtchange_u_s  = (droughtf_u_s -  droughtc_u_s)) %>%
    mutate(heatchange_u_o  = (heatf_u_o -  heatc_u_o)) %>%
    mutate(heatchange_u_s  = (heatf_u_s -  heatc_u_s)) %>%
    mutate(floodchange_u_o  = (floodf_u_o -  floodc_u_o)) %>%
    mutate(floodchange_u_s  = (floodf_u_s -  floodc_u_s)) %>%
    
    mutate(droughtc_u_heatc_u_o = getMembership(fpm_droughtc_u_heatc_u$optimal)) %>%
    mutate(droughtc_u_heatc_u_d = getMembership(fpm_droughtc_u_heatc_u$drought)) %>%
    mutate(droughtc_u_heatc_u_h = getMembership(fpm_droughtc_u_heatc_u$heat)) %>%
    mutate(droughtc_u_heatc_u_dh = getMembership(fpm_droughtc_u_heatc_u$drought_heat)) %>%
    mutate(droughtc_u_heatc_u_max =
             ifelse(
               droughtc_u_heatc_u_o >= 0.5,
               0,
               ifelse(
                 droughtc_u_heatc_u_d >= 0.5,
                 1,
                 ifelse(droughtc_u_heatc_u_h >= 0.5,
                    2,
                    ifelse(droughtc_u_heatc_u_dh >= 0.5, 4, 8)
                 )
               )
             )) %>%
    
    mutate(droughtc_u_floodc_u_o = getMembership(fpm_droughtc_u_floodc_u$optimal)) %>%
    mutate(droughtc_u_floodc_u_d = getMembership(fpm_droughtc_u_floodc_u$drought)) %>%
    mutate(droughtc_u_floodc_u_f = getMembership(fpm_droughtc_u_floodc_u$flood)) %>%
    mutate(droughtc_u_floodc_u_df = getMembership(fpm_droughtc_u_floodc_u$drought_flood)) %>%
    mutate(droughtc_u_floodc_u_max =
             ifelse(
               droughtc_u_floodc_u_o >= 0.5,
               0,
               ifelse(
                 droughtc_u_floodc_u_d >= 0.5,
                 1,
                 ifelse(droughtc_u_floodc_u_f >= 0.5, 
                    3, 
                    ifelse(droughtc_u_floodc_u_df >= 0.5, 5, 8)
                 )
               )
             )) %>%
    
    mutate(heatc_u_floodc_u_o = getMembership(fpm_heatc_u_floodc_u$optimal)) %>%
    mutate(heatc_u_floodc_u_h = getMembership(fpm_heatc_u_floodc_u$heat)) %>%
    mutate(heatc_u_floodc_u_f = getMembership(fpm_heatc_u_floodc_u$flood)) %>%
    mutate(heatc_u_floodc_u_hf = getMembership(fpm_heatc_u_floodc_u$heat_flood)) %>%
    mutate(heatc_u_floodc_u_max =
             ifelse(
               heatc_u_floodc_u_o >= 0.5,
               0,
               ifelse(
                 heatc_u_floodc_u_h >= 0.5,
                 2,
                 ifelse(heatc_u_floodc_u_f >= 0.5, 
                    3,
                    ifelse(heatc_u_floodc_u_hf >= 0.5, 6, 8)
                    )
               )
             )) %>%
    
    mutate(droughtc_u_heatc_u_floodc_u_o = getMembership(fpm_droughtc_u_heatc_u_floodc_u$optimal)) %>%
    mutate(droughtc_u_heatc_u_floodc_u_d = getMembership(fpm_droughtc_u_heatc_u_floodc_u$drought)) %>%
    mutate(droughtc_u_heatc_u_floodc_u_h = getMembership(fpm_droughtc_u_heatc_u_floodc_u$heat)) %>%
    mutate(droughtc_u_heatc_u_floodc_u_f = getMembership(fpm_droughtc_u_heatc_u_floodc_u$flood)) %>%
    mutate(
      droughtc_u_heatc_u_floodc_u_dh = getMembership(fpm_droughtc_u_heatc_u_floodc_u$drought_heat)
    ) %>%
    mutate(
      droughtc_u_heatc_u_floodc_u_df = getMembership(fpm_droughtc_u_heatc_u_floodc_u$drought_flood)
    ) %>%
    mutate(droughtc_u_heatc_u_floodc_u_hf = getMembership(fpm_droughtc_u_heatc_u_floodc_u$heat_flood)) %>%
    mutate(
      droughtc_u_heatc_u_floodc_u_dhf = getMembership(fpm_droughtc_u_heatc_u_floodc_u$drought_heat_flood)
    ) %>%
    mutate(
      droughtc_u_heatc_u_floodc_u_max =
        ifelse(
          droughtc_u_heatc_u_floodc_u_o >= 0.5,
          0,
          ifelse(
            droughtc_u_heatc_u_floodc_u_d >= 0.5,
            1,
            ifelse(
              droughtc_u_heatc_u_floodc_u_h >= 0.5,
              2,
              ifelse(
                droughtc_u_heatc_u_floodc_u_f >= 0.5,
                3,
                ifelse(
                  droughtc_u_heatc_u_floodc_u_dh >= 0.5,
                  4,
                  ifelse(
                    droughtc_u_heatc_u_floodc_u_df >= 0.5,
                    5,
                    ifelse(
                      droughtc_u_heatc_u_floodc_u_hf >= 0.5, 
                      6, 
                      ifelse(
                        droughtc_u_heatc_u_floodc_u_dhf >= 0.5, 7, 8)
                    )
                  )
                )
              )
            )
          )
        )
    ) %>%
    
    mutate(droughtf_u_heatf_u_o = getMembership(fpm_droughtf_u_heatf_u$optimal)) %>%
    mutate(droughtf_u_heatf_u_d = getMembership(fpm_droughtf_u_heatf_u$drought)) %>%
    mutate(droughtf_u_heatf_u_h = getMembership(fpm_droughtf_u_heatf_u$heat)) %>%
    mutate(droughtf_u_heatf_u_dh = getMembership(fpm_droughtf_u_heatf_u$drought_heat)) %>%
    mutate(droughtf_u_heatf_u_max =
             ifelse(
               droughtf_u_heatf_u_o >= 0.5,
               0,
               ifelse(
                 droughtf_u_heatf_u_d >= 0.5,
                 1,
                 ifelse(droughtf_u_heatf_u_h >= 0.5, 
                    2, 
                    ifelse(droughtf_u_heatf_u_dh >= 0.5, 4, 8)
                 )
               )
             )) %>%
    
    mutate(droughtf_u_floodf_u_o = getMembership(fpm_droughtf_u_floodf_u$optimal)) %>%
    mutate(droughtf_u_floodf_u_d = getMembership(fpm_droughtf_u_floodf_u$drought)) %>%
    mutate(droughtf_u_floodf_u_f = getMembership(fpm_droughtf_u_floodf_u$flood)) %>%
    mutate(droughtf_u_floodf_u_df = getMembership(fpm_droughtf_u_floodf_u$drought_flood)) %>%
    mutate(droughtf_u_floodf_u_max =
             ifelse(
               droughtf_u_floodf_u_o >= 0.5,
               0,
               ifelse(
                 droughtf_u_floodf_u_d >= 0.5,
                 1,
                 ifelse(droughtf_u_floodf_u_f >= 0.5, 
                    3, 
                    ifelse(droughtf_u_floodf_u_df >= 0.5, 5, 8)
                 )
               )
             )) %>%
    
    mutate(heatf_u_floodf_u_o = getMembership(fpm_heatf_u_floodf_u$optimal)) %>%
    mutate(heatf_u_floodf_u_h = getMembership(fpm_heatf_u_floodf_u$heat)) %>%
    mutate(heatf_u_floodf_u_f = getMembership(fpm_heatf_u_floodf_u$flood)) %>%
    mutate(heatf_u_floodf_u_hf = getMembership(fpm_heatf_u_floodf_u$heat_flood)) %>%
    mutate(heatf_u_floodf_u_max =
             ifelse(
               heatf_u_floodf_u_o >= 0.5,
               0,
               ifelse(
                 heatf_u_floodf_u_h >= 0.5,
                 2,
                 ifelse(heatf_u_floodf_u_f >= 0.5, 
                    3, 
                    ifelse(heatf_u_floodf_u_hf >= 0.5, 6, 8)
                 )
               )
             )) %>%
    
    
    mutate(droughtf_u_heatf_u_floodf_u_o = getMembership(fpm_droughtf_u_heatf_u_floodf_u$optimal)) %>%
    mutate(droughtf_u_heatf_u_floodf_u_d = getMembership(fpm_droughtf_u_heatf_u_floodf_u$drought)) %>%
    mutate(droughtf_u_heatf_u_floodf_u_h = getMembership(fpm_droughtf_u_heatf_u_floodf_u$heat)) %>%
    mutate(droughtf_u_heatf_u_floodf_u_f = getMembership(fpm_droughtf_u_heatf_u_floodf_u$flood)) %>%
    mutate(
      droughtf_u_heatf_u_floodf_u_dh = getMembership(fpm_droughtf_u_heatf_u_floodf_u$drought_heat)
    ) %>%
    mutate(
      droughtf_u_heatf_u_floodf_u_df = getMembership(fpm_droughtf_u_heatf_u_floodf_u$drought_flood)
    ) %>%
    mutate(droughtf_u_heatf_u_floodf_u_hf = getMembership(fpm_droughtf_u_heatf_u_floodf_u$heat_flood)) %>%
    mutate(
      droughtf_u_heatf_u_floodf_u_dhf = getMembership(fpm_droughtf_u_heatf_u_floodf_u$drought_heat_flood)
    ) %>%
    mutate(
      droughtf_u_heatf_u_floodf_u_max =
        ifelse(
          droughtf_u_heatf_u_floodf_u_o >= 0.5,
          0,
          ifelse(
            droughtf_u_heatf_u_floodf_u_d >= 0.5,
            1,
            ifelse(
              droughtf_u_heatf_u_floodf_u_h >= 0.5,
              2,
              ifelse(
                droughtf_u_heatf_u_floodf_u_f >= 0.5,
                3,
                ifelse(
                  droughtf_u_heatf_u_floodf_u_dh >= 0.5,
                  4,
                  ifelse(
                    droughtf_u_heatf_u_floodf_u_df >= 0.5,
                    5,
                    ifelse(
                      droughtf_u_heatf_u_floodf_u_hf >= 0.5, 
                      6,
                      ifelse(
                        droughtf_u_heatf_u_floodf_u_dhf >= 0.5, 7, 8)
                    )
                  )
                )
              )
            )
          )
        )
    ) %>%
    
    # mixed thresholds
    
    mutate(droughtc_l_heatc_u_o = getMembership(fpm_droughtc_l_heatc_u$optimal)) %>%
    mutate(droughtc_l_heatc_u_d = getMembership(fpm_droughtc_l_heatc_u$drought)) %>%
    mutate(droughtc_l_heatc_u_h = getMembership(fpm_droughtc_l_heatc_u$heat)) %>%
    mutate(droughtc_l_heatc_u_dh = getMembership(fpm_droughtc_l_heatc_u$drought_heat)) %>%
    mutate(droughtc_l_heatc_u_max =
             ifelse(
               droughtc_l_heatc_u_o >= 0.5,
               0,
               ifelse(
                 droughtc_l_heatc_u_d >= 0.5,
                 1,
                 ifelse(droughtc_l_heatc_u_h >= 0.5, 
                    2, 
                    ifelse(droughtc_l_heatc_u_dh >= 0.5, 4, 8)
                    )
               )
             )) %>%
    
    
    mutate(droughtc_u_heatc_l_o = getMembership(fpm_droughtc_u_heatc_l$optimal)) %>%
    mutate(droughtc_u_heatc_l_d = getMembership(fpm_droughtc_u_heatc_l$drought)) %>%
    mutate(droughtc_u_heatc_l_h = getMembership(fpm_droughtc_u_heatc_l$heat)) %>%
    mutate(droughtc_u_heatc_l_dh = getMembership(fpm_droughtc_u_heatc_l$drought_heat)) %>%
    mutate(droughtc_u_heatc_l_max =
             ifelse(
               droughtc_u_heatc_l_o >= 0.5,
               0,
               ifelse(
                 droughtc_u_heatc_l_d >= 0.5,
                 1,
                 ifelse(droughtc_u_heatc_l_h >= 0.5, 
                    2,  
                    ifelse(droughtc_u_heatc_l_dh >= 0.5, 4, 8)
                 )
               )
             )) %>%
    
    
    mutate(droughtc_l_floodc_u_o = getMembership(fpm_droughtc_l_floodc_u$optimal)) %>%
    mutate(droughtc_l_floodc_u_d = getMembership(fpm_droughtc_l_floodc_u$drought)) %>%
    mutate(droughtc_l_floodc_u_f = getMembership(fpm_droughtc_l_floodc_u$flood)) %>%
    mutate(droughtc_l_floodc_u_df = getMembership(fpm_droughtc_l_floodc_u$drought_flood)) %>%
    mutate(droughtc_l_floodc_u_max =
             ifelse(
               droughtc_l_floodc_u_o >= 0.5,
               0,
               ifelse(
                 droughtc_l_floodc_u_d >= 0.5,
                 1,
                 ifelse(droughtc_l_floodc_u_f >= 0.5, 
                    3, 
                    ifelse(droughtc_l_floodc_u_df >= 0.5, 5, 8)
                 )
               )
             )) %>%
    
    
    mutate(droughtc_u_floodc_l_o = getMembership(fpm_droughtc_u_floodc_l$optimal)) %>%
    mutate(droughtc_u_floodc_l_d = getMembership(fpm_droughtc_u_floodc_l$drought)) %>%
    mutate(droughtc_u_floodc_l_f = getMembership(fpm_droughtc_u_floodc_l$flood)) %>%
    mutate(droughtc_u_floodc_l_df = getMembership(fpm_droughtc_u_floodc_l$drought_flood)) %>%
    mutate(droughtc_u_floodc_l_max =
             ifelse(
               droughtc_u_floodc_l_o >= 0.5,
               0,
               ifelse(
                 droughtc_u_floodc_l_d >= 0.5,
                 1,
                 ifelse(droughtc_u_floodc_l_f >= 0.5, 
                   3,
                   ifelse(droughtc_u_floodc_l_df >= 0.5, 5, 8)
                 )
               )
             )) %>%
    
    
    mutate(heatc_l_floodc_u_o = getMembership(fpm_heatc_l_floodc_u$optimal)) %>%
    mutate(heatc_l_floodc_u_h = getMembership(fpm_heatc_l_floodc_u$heat)) %>%
    mutate(heatc_l_floodc_u_f = getMembership(fpm_heatc_l_floodc_u$flood)) %>%
    mutate(heatc_l_floodc_u_hf = getMembership(fpm_heatc_l_floodc_u$heat_flood)) %>%
    mutate(heatc_l_floodc_u_max =
             ifelse(
               heatc_l_floodc_u_o >= 0.5,
               0,
               ifelse(
                 heatc_l_floodc_u_h >= 0.5,
                 2,
                 ifelse(heatc_l_floodc_u_f >= 0.5, 
                    3,
                    ifelse(heatc_l_floodc_u_hf >= 0.5, 6, 8)
                 )
               )
             )) %>%
    
    
    mutate(heatc_u_floodc_l_o = getMembership(fpm_heatc_u_floodc_l$optimal)) %>%
    mutate(heatc_u_floodc_l_h = getMembership(fpm_heatc_u_floodc_l$heat)) %>%
    mutate(heatc_u_floodc_l_f = getMembership(fpm_heatc_u_floodc_l$flood)) %>%
    mutate(heatc_u_floodc_l_hf = getMembership(fpm_heatc_u_floodc_l$heat_flood)) %>%
    mutate(heatc_u_floodc_l_max =
             ifelse(
               heatc_u_floodc_l_o >= 0.5,
               0,
               ifelse(
                 heatc_u_floodc_l_h >= 0.5,
                 2,
                 ifelse(heatc_u_floodc_l_f >= 0.5, 
                    3, 
                    ifelse(heatc_u_floodc_l_hf >= 0.5, 6, 8)
                 )
               )
             )) %>%
    
    
    mutate(droughtc_l_heatc_l_floodc_u_o = getMembership(fpm_droughtc_l_heatc_l_floodc_u$optimal)) %>%
    mutate(droughtc_l_heatc_l_floodc_u_d = getMembership(fpm_droughtc_l_heatc_l_floodc_u$drought)) %>%
    mutate(droughtc_l_heatc_l_floodc_u_h = getMembership(fpm_droughtc_l_heatc_l_floodc_u$heat)) %>%
    mutate(droughtc_l_heatc_l_floodc_u_f = getMembership(fpm_droughtc_l_heatc_l_floodc_u$flood)) %>%
    mutate(
      droughtc_l_heatc_l_floodc_u_dh = getMembership(fpm_droughtc_l_heatc_l_floodc_u$drought_heat)
    ) %>%
    mutate(
      droughtc_l_heatc_l_floodc_u_df = getMembership(fpm_droughtc_l_heatc_l_floodc_u$drought_flood)
    ) %>%
    mutate(droughtc_l_heatc_l_floodc_u_hf = getMembership(fpm_droughtc_l_heatc_l_floodc_u$heat_flood)) %>%
    mutate(
      droughtc_l_heatc_l_floodc_u_dhf = getMembership(fpm_droughtc_l_heatc_l_floodc_u$drought_heat_flood)
    ) %>%
    mutate(
      droughtc_l_heatc_l_floodc_u_max =
        ifelse(
          droughtc_l_heatc_l_floodc_u_o >= 0.5,
          0,
          ifelse(
            droughtc_l_heatc_l_floodc_u_d >= 0.5,
            1,
            ifelse(
              droughtc_l_heatc_l_floodc_u_h >= 0.5,
              2,
              ifelse(
                droughtc_l_heatc_l_floodc_u_f >= 0.5,
                3,
                ifelse(
                  droughtc_l_heatc_l_floodc_u_dh >= 0.5,
                  4,
                  ifelse(
                    droughtc_l_heatc_l_floodc_u_df >= 0.5,
                    5,
                    ifelse(
                      droughtc_l_heatc_l_floodc_u_hf >= 0.5, 
                      6, 
                      ifelse(
                        droughtc_l_heatc_l_floodc_u_dhf >= 0.5, 7, 8)
                    )
                  )
                )
              )
            )
          )
        )
    ) %>%
    
    
    mutate(droughtc_l_heatc_u_floodc_l_o = getMembership(fpm_droughtc_l_heatc_u_floodc_l$optimal)) %>%
    mutate(droughtc_l_heatc_u_floodc_l_d = getMembership(fpm_droughtc_l_heatc_u_floodc_l$drought)) %>%
    mutate(droughtc_l_heatc_u_floodc_l_h = getMembership(fpm_droughtc_l_heatc_u_floodc_l$heat)) %>%
    mutate(droughtc_l_heatc_u_floodc_l_f = getMembership(fpm_droughtc_l_heatc_u_floodc_l$flood)) %>%
    mutate(
      droughtc_l_heatc_u_floodc_l_dh = getMembership(fpm_droughtc_l_heatc_u_floodc_l$drought_heat)
    ) %>%
    mutate(
      droughtc_l_heatc_u_floodc_l_df = getMembership(fpm_droughtc_l_heatc_u_floodc_l$drought_flood)
    ) %>%
    mutate(droughtc_l_heatc_u_floodc_l_hf = getMembership(fpm_droughtc_l_heatc_u_floodc_l$heat_flood)) %>%
    mutate(
      droughtc_l_heatc_u_floodc_l_dhf = getMembership(fpm_droughtc_l_heatc_u_floodc_l$drought_heat_flood)
    ) %>%
    mutate(
      droughtc_l_heatc_u_floodc_l_max =
        ifelse(
          droughtc_l_heatc_u_floodc_l_o >= 0.5,
          0,
          ifelse(
            droughtc_l_heatc_u_floodc_l_d >= 0.5,
            1,
            ifelse(
              droughtc_l_heatc_u_floodc_l_h >= 0.5,
              2,
              ifelse(
                droughtc_l_heatc_u_floodc_l_f >= 0.5,
                3,
                ifelse(
                  droughtc_l_heatc_u_floodc_l_dh >= 0.5,
                  4,
                  ifelse(
                    droughtc_l_heatc_u_floodc_l_df >= 0.5,
                    5,
                    ifelse(
                      droughtc_l_heatc_u_floodc_l_hf >= 0.5, 
                      6, 
                      ifelse(
                        droughtc_l_heatc_u_floodc_l_dhf >= 0.5, 7, 8)
                    )
                  )
                )
              )
            )
          )
        )
    ) %>%
    
    
    mutate(droughtc_u_heatc_l_floodc_u_o = getMembership(fpm_droughtc_u_heatc_l_floodc_u$optimal)) %>%
    mutate(droughtc_u_heatc_l_floodc_u_d = getMembership(fpm_droughtc_u_heatc_l_floodc_u$drought)) %>%
    mutate(droughtc_u_heatc_l_floodc_u_h = getMembership(fpm_droughtc_u_heatc_l_floodc_u$heat)) %>%
    mutate(droughtc_u_heatc_l_floodc_u_f = getMembership(fpm_droughtc_u_heatc_l_floodc_u$flood)) %>%
    mutate(
      droughtc_u_heatc_l_floodc_u_dh = getMembership(fpm_droughtc_u_heatc_l_floodc_u$drought_heat)
    ) %>%
    mutate(
      droughtc_u_heatc_l_floodc_u_df = getMembership(fpm_droughtc_u_heatc_l_floodc_u$drought_flood)
    ) %>%
    mutate(droughtc_u_heatc_l_floodc_u_hf = getMembership(fpm_droughtc_u_heatc_l_floodc_u$heat_flood)) %>%
    mutate(
      droughtc_u_heatc_l_floodc_u_dhf = getMembership(fpm_droughtc_u_heatc_l_floodc_u$drought_heat_flood)
    ) %>%
    mutate(
      droughtc_u_heatc_l_floodc_u_max =
        ifelse(
          droughtc_u_heatc_l_floodc_u_o >= 0.5,
          0,
          ifelse(
            droughtc_u_heatc_l_floodc_u_d >= 0.5,
            1,
            ifelse(
              droughtc_u_heatc_l_floodc_u_h >= 0.5,
              2,
              ifelse(
                droughtc_u_heatc_l_floodc_u_f >= 0.5,
                3,
                ifelse(
                  droughtc_u_heatc_l_floodc_u_dh >= 0.5,
                  4,
                  ifelse(
                    droughtc_u_heatc_l_floodc_u_df >= 0.5,
                    5,
                    ifelse(
                      droughtc_u_heatc_l_floodc_u_hf >= 0.5, 
                      6, 
                      ifelse(
                        droughtc_u_heatc_l_floodc_u_dhf >= 0.5, 7, 8)
                    )
                  )
                )
              )
            )
          )
        )
    ) %>%
    
    
    mutate(droughtc_u_heatc_l_floodc_l_o = getMembership(fpm_droughtc_u_heatc_l_floodc_l$optimal)) %>%
    mutate(droughtc_u_heatc_l_floodc_l_d = getMembership(fpm_droughtc_u_heatc_l_floodc_l$drought)) %>%
    mutate(droughtc_u_heatc_l_floodc_l_h = getMembership(fpm_droughtc_u_heatc_l_floodc_l$heat)) %>%
    mutate(droughtc_u_heatc_l_floodc_l_f = getMembership(fpm_droughtc_u_heatc_l_floodc_l$flood)) %>%
    mutate(
      droughtc_u_heatc_l_floodc_l_dh = getMembership(fpm_droughtc_u_heatc_l_floodc_l$drought_heat)
    ) %>%
    mutate(
      droughtc_u_heatc_l_floodc_l_df = getMembership(fpm_droughtc_u_heatc_l_floodc_l$drought_flood)
    ) %>%
    mutate(droughtc_u_heatc_l_floodc_l_hf = getMembership(fpm_droughtc_u_heatc_l_floodc_l$heat_flood)) %>%
    mutate(
      droughtc_u_heatc_l_floodc_l_dhf = getMembership(fpm_droughtc_u_heatc_l_floodc_l$drought_heat_flood)
    ) %>%
    mutate(
      droughtc_u_heatc_l_floodc_l_max =
        ifelse(
          droughtc_u_heatc_l_floodc_l_o >= 0.5,
          0,
          ifelse(
            droughtc_u_heatc_l_floodc_l_d >= 0.5,
            1,
            ifelse(
              droughtc_u_heatc_l_floodc_l_h >= 0.5,
              2,
              ifelse(
                droughtc_u_heatc_l_floodc_l_f >= 0.5,
                3,
                ifelse(
                  droughtc_u_heatc_l_floodc_l_dh >= 0.5,
                  4,
                  ifelse(
                    droughtc_u_heatc_l_floodc_l_df >= 0.5,
                    5,
                    ifelse(
                      droughtc_u_heatc_l_floodc_l_hf >= 0.5, 
                      6, 
                      ifelse(
                        droughtc_u_heatc_l_floodc_l_dhf >= 0.5, 7, 8)
                    )
                  )
                )
              )
            )
          )
        )
    ) %>%
    
    
    mutate(droughtc_u_heatc_u_floodc_l_o = getMembership(fpm_droughtc_u_heatc_u_floodc_l$optimal)) %>%
    mutate(droughtc_u_heatc_u_floodc_l_d = getMembership(fpm_droughtc_u_heatc_u_floodc_l$drought)) %>%
    mutate(droughtc_u_heatc_u_floodc_l_h = getMembership(fpm_droughtc_u_heatc_u_floodc_l$heat)) %>%
    mutate(droughtc_u_heatc_u_floodc_l_f = getMembership(fpm_droughtc_u_heatc_u_floodc_l$flood)) %>%
    mutate(
      droughtc_u_heatc_u_floodc_l_dh = getMembership(fpm_droughtc_u_heatc_u_floodc_l$drought_heat)
    ) %>%
    mutate(
      droughtc_u_heatc_u_floodc_l_df = getMembership(fpm_droughtc_u_heatc_u_floodc_l$drought_flood)
    ) %>%
    mutate(droughtc_u_heatc_u_floodc_l_hf = getMembership(fpm_droughtc_u_heatc_u_floodc_l$heat_flood)) %>%
    mutate(
      droughtc_u_heatc_u_floodc_l_dhf = getMembership(fpm_droughtc_u_heatc_u_floodc_l$drought_heat_flood)
    ) %>%
    mutate(
      droughtc_u_heatc_u_floodc_l_max =
        ifelse(
          droughtc_u_heatc_u_floodc_l_o >= 0.5,
          0,
          ifelse(
            droughtc_u_heatc_u_floodc_l_d >= 0.5,
            1,
            ifelse(
              droughtc_u_heatc_u_floodc_l_h >= 0.5,
              2,
              ifelse(
                droughtc_u_heatc_u_floodc_l_f >= 0.5,
                3,
                ifelse(
                  droughtc_u_heatc_u_floodc_l_dh >= 0.5,
                  4,
                  ifelse(
                    droughtc_u_heatc_u_floodc_l_df >= 0.5,
                    5,
                    ifelse(
                      droughtc_u_heatc_u_floodc_l_hf >= 0.5, 
                      6, 
                      ifelse(
                        droughtc_u_heatc_u_floodc_l_dhf >= 0.5, 7, 8)
                    )
                  )
                )
              )
            )
          )
        )
    ) %>%
    
    
    mutate(droughtc_l_heatc_u_floodc_u_o = getMembership(fpm_droughtc_l_heatc_u_floodc_u$optimal)) %>%
    mutate(droughtc_l_heatc_u_floodc_u_d = getMembership(fpm_droughtc_l_heatc_u_floodc_u$drought)) %>%
    mutate(droughtc_l_heatc_u_floodc_u_h = getMembership(fpm_droughtc_l_heatc_u_floodc_u$heat)) %>%
    mutate(droughtc_l_heatc_u_floodc_u_f = getMembership(fpm_droughtc_l_heatc_u_floodc_u$flood)) %>%
    mutate(
      droughtc_l_heatc_u_floodc_u_dh = getMembership(fpm_droughtc_l_heatc_u_floodc_u$drought_heat)
    ) %>%
    mutate(
      droughtc_l_heatc_u_floodc_u_df = getMembership(fpm_droughtc_l_heatc_u_floodc_u$drought_flood)
    ) %>%
    mutate(droughtc_l_heatc_u_floodc_u_hf = getMembership(fpm_droughtc_l_heatc_u_floodc_u$heat_flood)) %>%
    mutate(
      droughtc_l_heatc_u_floodc_u_dhf = getMembership(fpm_droughtc_l_heatc_u_floodc_u$drought_heat_flood)
    ) %>%
    mutate(
      droughtc_l_heatc_u_floodc_u_max =
        ifelse(
          droughtc_l_heatc_u_floodc_u_o >= 0.5,
          0,
          ifelse(
            droughtc_l_heatc_u_floodc_u_d >= 0.5,
            1,
            ifelse(
              droughtc_l_heatc_u_floodc_u_h >= 0.5,
              2,
              ifelse(
                droughtc_l_heatc_u_floodc_u_f >= 0.5,
                3,
                ifelse(
                  droughtc_l_heatc_u_floodc_u_dh >= 0.5,
                  4,
                  ifelse(
                    droughtc_l_heatc_u_floodc_u_df >= 0.5,
                    5,
                    ifelse(droughtc_l_heatc_u_floodc_u_hf >= 0.5, 
                      6, 
                      ifelse(droughtc_l_heatc_u_floodc_u_dhf >= 0.5, 7, 8)
                    )
                  )
                )
              )
            )
          )
        )
    ) %>%
    
    
    mutate(droughtf_l_heatf_u_o = getMembership(fpm_droughtf_l_heatf_u$optimal)) %>%
    mutate(droughtf_l_heatf_u_d = getMembership(fpm_droughtf_l_heatf_u$drought)) %>%
    mutate(droughtf_l_heatf_u_h = getMembership(fpm_droughtf_l_heatf_u$heat)) %>%
    mutate(droughtf_l_heatf_u_dh = getMembership(fpm_droughtf_l_heatf_u$drought_heat)) %>%
    mutate(droughtf_l_heatf_u_max =
             ifelse(
               droughtf_l_heatf_u_o >= 0.5,
               0,
               ifelse(
                 droughtf_l_heatf_u_d >= 0.5,
                 1,
                 ifelse(droughtf_l_heatf_u_h >= 0.5, 
                    2, 
                    ifelse(droughtf_l_heatf_u_dh >= 0.5, 4, 8)
                 )
               )
             )) %>%
    
    
    mutate(droughtf_u_heatf_l_o = getMembership(fpm_droughtf_u_heatf_l$optimal)) %>%
    mutate(droughtf_u_heatf_l_d = getMembership(fpm_droughtf_u_heatf_l$drought)) %>%
    mutate(droughtf_u_heatf_l_h = getMembership(fpm_droughtf_u_heatf_l$heat)) %>%
    mutate(droughtf_u_heatf_l_dh = getMembership(fpm_droughtf_u_heatf_l$drought_heat)) %>%
    mutate(droughtf_u_heatf_l_max =
             ifelse(
               droughtf_u_heatf_l_o >= 0.5,
               0,
               ifelse(
                 droughtf_u_heatf_l_d >= 0.5,
                 1,
                 ifelse(droughtf_u_heatf_l_h >= 0.5, 
                   2, 
                   ifelse(droughtf_u_heatf_l_dh >= 0.5, 4, 8)
                 )
               )
             )) %>%
    
    
    mutate(droughtf_l_floodf_u_o = getMembership(fpm_droughtf_l_floodf_u$optimal)) %>%
    mutate(droughtf_l_floodf_u_d = getMembership(fpm_droughtf_l_floodf_u$drought)) %>%
    mutate(droughtf_l_floodf_u_f = getMembership(fpm_droughtf_l_floodf_u$flood)) %>%
    mutate(droughtf_l_floodf_u_df = getMembership(fpm_droughtf_l_floodf_u$drought_flood)) %>%
    mutate(droughtf_l_floodf_u_max =
             ifelse(
               droughtf_l_floodf_u_o >= 0.5,
               0,
               ifelse(
                 droughtf_l_floodf_u_d >= 0.5,
                 1,
                 ifelse(droughtf_l_floodf_u_f >= 0.5, 
                   3, 
                   ifelse(droughtf_l_floodf_u_df >= 0.5, 5, 8)
                 )
               )
             )) %>%
    
    
    mutate(droughtf_u_floodf_l_o = getMembership(fpm_droughtf_u_floodf_l$optimal)) %>%
    mutate(droughtf_u_floodf_l_d = getMembership(fpm_droughtf_u_floodf_l$drought)) %>%
    mutate(droughtf_u_floodf_l_f = getMembership(fpm_droughtf_u_floodf_l$flood)) %>%
    mutate(droughtf_u_floodf_l_df = getMembership(fpm_droughtf_u_floodf_l$drought_flood)) %>%
    mutate(droughtf_u_floodf_l_max =
             ifelse(
               droughtf_u_floodf_l_o >= 0.5,
               0,
               ifelse(
                 droughtf_u_floodf_l_d >= 0.5,
                 1,
                 ifelse(droughtf_u_floodf_l_f >= 0.5, 
                   3, 
                   ifelse(droughtf_u_floodf_l_df >= 0.5, 5, 8)
                 )
               )
             )) %>%
    
    
    mutate(heatf_l_floodf_u_o = getMembership(fpm_heatf_l_floodf_u$optimal)) %>%
    mutate(heatf_l_floodf_u_h = getMembership(fpm_heatf_l_floodf_u$heat)) %>%
    mutate(heatf_l_floodf_u_f = getMembership(fpm_heatf_l_floodf_u$flood)) %>%
    mutate(heatf_l_floodf_u_hf = getMembership(fpm_heatf_l_floodf_u$heat_flood)) %>%
    mutate(heatf_l_floodf_u_max =
             ifelse(
               heatf_l_floodf_u_o >= 0.5,
               0,
               ifelse(
                 heatf_l_floodf_u_h >= 0.5,
                 2,
                 ifelse(heatf_l_floodf_u_f >= 0.5, 
                   3, 
                   ifelse(heatf_l_floodf_u_hf >= 0.5, 6, 8)
                 )
               )
             )) %>%
    
    
    mutate(heatf_u_floodf_l_o = getMembership(fpm_heatf_u_floodf_l$optimal)) %>%
    mutate(heatf_u_floodf_l_h = getMembership(fpm_heatf_u_floodf_l$heat)) %>%
    mutate(heatf_u_floodf_l_f = getMembership(fpm_heatf_u_floodf_l$flood)) %>%
    mutate(heatf_u_floodf_l_hf = getMembership(fpm_heatf_u_floodf_l$heat_flood)) %>%
    mutate(heatf_u_floodf_l_max =
             ifelse(
               heatf_u_floodf_l_o >= 0.5,
               0,
               ifelse(
                 heatf_u_floodf_l_h >= 0.5,
                 2,
                 ifelse(heatf_u_floodf_l_f >= 0.5, 
                    3, 
                    ifelse(heatf_u_floodf_l_hf >= 0.5, 6, 8)
                 )
               )
             )) %>%
    
    
    mutate(droughtf_l_heatf_l_floodf_u_o = getMembership(fpm_droughtf_l_heatf_l_floodf_u$optimal)) %>%
    mutate(droughtf_l_heatf_l_floodf_u_d = getMembership(fpm_droughtf_l_heatf_l_floodf_u$drought)) %>%
    mutate(droughtf_l_heatf_l_floodf_u_h = getMembership(fpm_droughtf_l_heatf_l_floodf_u$heat)) %>%
    mutate(droughtf_l_heatf_l_floodf_u_f = getMembership(fpm_droughtf_l_heatf_l_floodf_u$flood)) %>%
    mutate(
      droughtf_l_heatf_l_floodf_u_dh = getMembership(fpm_droughtf_l_heatf_l_floodf_u$drought_heat)
    ) %>%
    mutate(
      droughtf_l_heatf_l_floodf_u_df = getMembership(fpm_droughtf_l_heatf_l_floodf_u$drought_flood)
    ) %>%
    mutate(droughtf_l_heatf_l_floodf_u_hf = getMembership(fpm_droughtf_l_heatf_l_floodf_u$heat_flood)) %>%
    mutate(
      droughtf_l_heatf_l_floodf_u_dhf = getMembership(fpm_droughtf_l_heatf_l_floodf_u$drought_heat_flood)
    ) %>%
    mutate(
      droughtf_l_heatf_l_floodf_u_max =
        ifelse(
          droughtf_l_heatf_l_floodf_u_o >= 0.5,
          0,
          ifelse(
            droughtf_l_heatf_l_floodf_u_d >= 0.5,
            1,
            ifelse(
              droughtf_l_heatf_l_floodf_u_h >= 0.5,
              2,
              ifelse(
                droughtf_l_heatf_l_floodf_u_f >= 0.5,
                3,
                ifelse(
                  droughtf_l_heatf_l_floodf_u_dh >= 0.5,
                  4,
                  ifelse(
                    droughtf_l_heatf_l_floodf_u_df >= 0.5,
                    5,
                    ifelse(
                      droughtf_l_heatf_l_floodf_u_hf >= 0.5, 
                      6, 
                      ifelse(
                        droughtf_l_heatf_l_floodf_u_dhf >= 0.5, 7, 8)
                    )
                  )
                )
              )
            )
          )
        )
    ) %>%
    
    
    mutate(droughtf_l_heatf_u_floodf_l_o = getMembership(fpm_droughtf_l_heatf_u_floodf_l$optimal)) %>%
    mutate(droughtf_l_heatf_u_floodf_l_d = getMembership(fpm_droughtf_l_heatf_u_floodf_l$drought)) %>%
    mutate(droughtf_l_heatf_u_floodf_l_h = getMembership(fpm_droughtf_l_heatf_u_floodf_l$heat)) %>%
    mutate(droughtf_l_heatf_u_floodf_l_f = getMembership(fpm_droughtf_l_heatf_u_floodf_l$flood)) %>%
    mutate(
      droughtf_l_heatf_u_floodf_l_dh = getMembership(fpm_droughtf_l_heatf_u_floodf_l$drought_heat)
    ) %>%
    mutate(
      droughtf_l_heatf_u_floodf_l_df = getMembership(fpm_droughtf_l_heatf_u_floodf_l$drought_flood)
    ) %>%
    mutate(droughtf_l_heatf_u_floodf_l_hf = getMembership(fpm_droughtf_l_heatf_u_floodf_l$heat_flood)) %>%
    mutate(
      droughtf_l_heatf_u_floodf_l_dhf = getMembership(fpm_droughtf_l_heatf_u_floodf_l$drought_heat_flood)
    ) %>%
    mutate(
      droughtf_l_heatf_u_floodf_l_max =
        ifelse(
          droughtf_l_heatf_u_floodf_l_o >= 0.5,
          0,
          ifelse(
            droughtf_l_heatf_u_floodf_l_d >= 0.5,
            1,
            ifelse(
              droughtf_l_heatf_u_floodf_l_h >= 0.5,
              2,
              ifelse(
                droughtf_l_heatf_u_floodf_l_f >= 0.5,
                3,
                ifelse(
                  droughtf_l_heatf_u_floodf_l_dh >= 0.5,
                  4,
                  ifelse(
                    droughtf_l_heatf_u_floodf_l_df >= 0.5,
                    5,
                    ifelse(
                      droughtf_l_heatf_u_floodf_l_hf >= 0.5, 
                      6, 
                      ifelse(
                        droughtf_l_heatf_u_floodf_l_dhf >= 0.5, 
                         7, 8)
                    )
                  )
                )
              )
            )
          )
        )
    ) %>%
    
    
    mutate(droughtf_u_heatf_l_floodf_u_o = getMembership(fpm_droughtf_u_heatf_l_floodf_u$optimal)) %>%
    mutate(droughtf_u_heatf_l_floodf_u_d = getMembership(fpm_droughtf_u_heatf_l_floodf_u$drought)) %>%
    mutate(droughtf_u_heatf_l_floodf_u_h = getMembership(fpm_droughtf_u_heatf_l_floodf_u$heat)) %>%
    mutate(droughtf_u_heatf_l_floodf_u_f = getMembership(fpm_droughtf_u_heatf_l_floodf_u$flood)) %>%
    mutate(
      droughtf_u_heatf_l_floodf_u_dh = getMembership(fpm_droughtf_u_heatf_l_floodf_u$drought_heat)
    ) %>%
    mutate(
      droughtf_u_heatf_l_floodf_u_df = getMembership(fpm_droughtf_u_heatf_l_floodf_u$drought_flood)
    ) %>%
    mutate(droughtf_u_heatf_l_floodf_u_hf = getMembership(fpm_droughtf_u_heatf_l_floodf_u$heat_flood)) %>%
    mutate(
      droughtf_u_heatf_l_floodf_u_dhf = getMembership(fpm_droughtf_u_heatf_l_floodf_u$drought_heat_flood)
    ) %>%
    mutate(
      droughtf_u_heatf_l_floodf_u_max =
        ifelse(
          droughtf_u_heatf_l_floodf_u_o >= 0.5,
          0,
          ifelse(
            droughtf_u_heatf_l_floodf_u_d >= 0.5,
            1,
            ifelse(
              droughtf_u_heatf_l_floodf_u_h >= 0.5,
              2,
              ifelse(
                droughtf_u_heatf_l_floodf_u_f >= 0.5,
                3,
                ifelse(
                  droughtf_u_heatf_l_floodf_u_dh >= 0.5,
                  4,
                  ifelse(
                    droughtf_u_heatf_l_floodf_u_df >= 0.5,
                    5,
                    ifelse(
                      droughtf_u_heatf_l_floodf_u_hf >= 0.5, 
                      6,
                      ifelse(
                        droughtf_u_heatf_l_floodf_u_dhf >= 0.5, 7, 8)
                    )
                  )
                )
              )
            )
          )
        )
    ) %>%
    
    
    mutate(droughtf_u_heatf_l_floodf_l_o = getMembership(fpm_droughtf_u_heatf_l_floodf_l$optimal)) %>%
    mutate(droughtf_u_heatf_l_floodf_l_d = getMembership(fpm_droughtf_u_heatf_l_floodf_l$drought)) %>%
    mutate(droughtf_u_heatf_l_floodf_l_h = getMembership(fpm_droughtf_u_heatf_l_floodf_l$heat)) %>%
    mutate(droughtf_u_heatf_l_floodf_l_f = getMembership(fpm_droughtf_u_heatf_l_floodf_l$flood)) %>%
    mutate(
      droughtf_u_heatf_l_floodf_l_dh = getMembership(fpm_droughtf_u_heatf_l_floodf_l$drought_heat)
    ) %>%
    mutate(
      droughtf_u_heatf_l_floodf_l_df = getMembership(fpm_droughtf_u_heatf_l_floodf_l$drought_flood)
    ) %>%
    mutate(droughtf_u_heatf_l_floodf_l_hf = getMembership(fpm_droughtf_u_heatf_l_floodf_l$heat_flood)) %>%
    mutate(
      droughtf_u_heatf_l_floodf_l_dhf = getMembership(fpm_droughtf_u_heatf_l_floodf_l$drought_heat_flood)
    ) %>%
    mutate(
      droughtf_u_heatf_l_floodf_l_max =
        ifelse(
          droughtf_u_heatf_l_floodf_l_o >= 0.5,
          0,
          ifelse(
            droughtf_u_heatf_l_floodf_l_d >= 0.5,
            1,
            ifelse(
              droughtf_u_heatf_l_floodf_l_h >= 0.5,
              2,
              ifelse(
                droughtf_u_heatf_l_floodf_l_f >= 0.5,
                3,
                ifelse(
                  droughtf_u_heatf_l_floodf_l_dh >= 0.5,
                  4,
                  ifelse(
                    droughtf_u_heatf_l_floodf_l_df >= 0.5,
                    5,
                    ifelse(
                      droughtf_u_heatf_l_floodf_l_hf >= 0.5, 
                      6, 
                      ifelse(
                        droughtf_u_heatf_l_floodf_l_dhf >= 0.5, 7, 8)
                    )
                  )
                )
              )
            )
          )
        )
    ) %>%
    
    
    mutate(droughtf_u_heatf_u_floodf_l_o = getMembership(fpm_droughtf_u_heatf_u_floodf_l$optimal)) %>%
    mutate(droughtf_u_heatf_u_floodf_l_d = getMembership(fpm_droughtf_u_heatf_u_floodf_l$drought)) %>%
    mutate(droughtf_u_heatf_u_floodf_l_h = getMembership(fpm_droughtf_u_heatf_u_floodf_l$heat)) %>%
    mutate(droughtf_u_heatf_u_floodf_l_f = getMembership(fpm_droughtf_u_heatf_u_floodf_l$flood)) %>%
    mutate(
      droughtf_u_heatf_u_floodf_l_dh = getMembership(fpm_droughtf_u_heatf_u_floodf_l$drought_heat)
    ) %>%
    mutate(
      droughtf_u_heatf_u_floodf_l_df = getMembership(fpm_droughtf_u_heatf_u_floodf_l$drought_flood)
    ) %>%
    mutate(droughtf_u_heatf_u_floodf_l_hf = getMembership(fpm_droughtf_u_heatf_u_floodf_l$heat_flood)) %>%
    mutate(
      droughtf_u_heatf_u_floodf_l_dhf = getMembership(fpm_droughtf_u_heatf_u_floodf_l$drought_heat_flood)
    ) %>%
    mutate(
      droughtf_u_heatf_u_floodf_l_max =
        ifelse(
          droughtf_u_heatf_u_floodf_l_o >= 0.5,
          0,
          ifelse(
            droughtf_u_heatf_u_floodf_l_d >= 0.5,
            1,
            ifelse(
              droughtf_u_heatf_u_floodf_l_h >= 0.5,
              2,
              ifelse(
                droughtf_u_heatf_u_floodf_l_f >= 0.5,
                3,
                ifelse(
                  droughtf_u_heatf_u_floodf_l_dh >= 0.5,
                  4,
                  ifelse(
                    droughtf_u_heatf_u_floodf_l_df >= 0.5,
                    5,
                    ifelse(
                      droughtf_u_heatf_u_floodf_l_hf >= 0.5, 
                      6, 
                      ifelse(
                        droughtf_u_heatf_u_floodf_l_dhf >= 0.5, 7, 8)
                    )
                  )
                )
              )
            )
          )
        )
    ) %>%
    
    
    mutate(droughtf_l_heatf_u_floodf_u_o = getMembership(fpm_droughtf_l_heatf_u_floodf_u$optimal)) %>%
    mutate(droughtf_l_heatf_u_floodf_u_d = getMembership(fpm_droughtf_l_heatf_u_floodf_u$drought)) %>%
    mutate(droughtf_l_heatf_u_floodf_u_h = getMembership(fpm_droughtf_l_heatf_u_floodf_u$heat)) %>%
    mutate(droughtf_l_heatf_u_floodf_u_f = getMembership(fpm_droughtf_l_heatf_u_floodf_u$flood)) %>%
    mutate(
      droughtf_l_heatf_u_floodf_u_dh = getMembership(fpm_droughtf_l_heatf_u_floodf_u$drought_heat)
    ) %>%
    mutate(
      droughtf_l_heatf_u_floodf_u_df = getMembership(fpm_droughtf_l_heatf_u_floodf_u$drought_flood)
    ) %>%
    mutate(droughtf_l_heatf_u_floodf_u_hf = getMembership(fpm_droughtf_l_heatf_u_floodf_u$heat_flood)) %>%
    mutate(
      droughtf_l_heatf_u_floodf_u_dhf = getMembership(fpm_droughtf_l_heatf_u_floodf_u$drought_heat_flood)
    ) %>%
    mutate(
      droughtf_l_heatf_u_floodf_u_max =
        ifelse(
          droughtf_l_heatf_u_floodf_u_o >= 0.5,
          0,
          ifelse(
            droughtf_l_heatf_u_floodf_u_d >= 0.5,
            1,
            ifelse(
              droughtf_l_heatf_u_floodf_u_h >= 0.5,
              2,
              ifelse(
                droughtf_l_heatf_u_floodf_u_f >= 0.5,
                3,
                ifelse(
                  droughtf_l_heatf_u_floodf_u_dh >= 0.5,
                  4,
                  ifelse(
                    droughtf_l_heatf_u_floodf_u_df >= 0.5,
                    5,
                    ifelse(
                      droughtf_l_heatf_u_floodf_u_hf >= 0.5, 
                      6, 
                      ifelse(
                        droughtf_l_heatf_u_floodf_u_dhf >= 0.5, 7, 8)
                    )
                  )
                )
              )
            )
          )
        )
    )
  
}

### Spatialise Impact Data

# results in v_impact
v_impact_make_f <- function(dB_impact_full) {
  dB_impact_full %>% st_as_sf(coords = c("x", "y"), crs = st_crs(4326))
}

# results in rB_impact_prelim
rB_impact_prelim_make_f <- function(v_impact, r_droughtc) {
  rasterize(v_impact, r_droughtc, field = 'droughtc_l_o') %>% brick
}

# results in rB_impact
rB_impact_make_f <-
  function(v_impact, r_droughtc, rB_impact_prelim) {
    rB_impact_prelim %>%
      
      # lower thresholds
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_s')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatc_l_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatc_l_s')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'floodc_l_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'floodc_l_s')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_s')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatf_l_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatf_l_s')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'floodf_l_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'floodf_l_s'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtchange_l_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtchange_l_s')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatchange_l_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatchange_l_s')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'floodchange_l_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'floodchange_l_s'))  %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_l_o'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_l_d'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_l_h'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_l_dh'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_l_max'))  %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_floodc_l_o'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_floodc_l_d'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_floodc_l_f'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_floodc_l_df'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_floodc_l_max'))  %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatc_l_floodc_l_o'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatc_l_floodc_l_h'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatc_l_floodc_l_f'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatc_l_floodc_l_hf'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatc_l_floodc_l_max'))  %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_l_floodc_l_o'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_l_floodc_l_d'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_l_floodc_l_h'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_l_floodc_l_f'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_l_floodc_l_dh'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_l_floodc_l_df'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_l_floodc_l_hf'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_l_floodc_l_dhf'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_l_floodc_l_max'))  %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_l_o'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_l_d'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_l_h'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_l_dh'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_l_max'))  %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_floodf_l_o'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_floodf_l_d'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_floodf_l_f'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_floodf_l_df'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_floodf_l_max'))  %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatf_l_floodf_l_o'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatf_l_floodf_l_h'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatf_l_floodf_l_f'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatf_l_floodf_l_hf'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatf_l_floodf_l_max'))  %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_l_floodf_l_o'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_l_floodf_l_d'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_l_floodf_l_h'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_l_floodf_l_f'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_l_floodf_l_dh'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_l_floodf_l_df'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_l_floodf_l_hf'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_l_floodf_l_dhf'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_l_floodf_l_max'))  %>%
      
      # upper thresholds
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_s')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatc_u_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatc_u_s')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'floodc_u_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'floodc_u_s')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_s')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatf_u_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatf_u_s')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'floodf_u_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'floodf_u_s'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtchange_u_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtchange_u_s')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatchange_u_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatchange_u_s')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'floodchange_u_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'floodchange_u_s'))  %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_u_o'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_u_d'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_u_h'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_u_dh'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_u_max'))  %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_floodc_u_o'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_floodc_u_d'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_floodc_u_f'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_floodc_u_df'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_floodc_u_max'))  %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatc_u_floodc_u_o'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatc_u_floodc_u_h'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatc_u_floodc_u_f'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatc_u_floodc_u_hf'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatc_u_floodc_u_max'))  %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_u_floodc_u_o'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_u_floodc_u_d'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_u_floodc_u_h'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_u_floodc_u_f'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_u_floodc_u_dh'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_u_floodc_u_df'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_u_floodc_u_hf'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_u_floodc_u_dhf'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_u_floodc_u_max'))  %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_u_o'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_u_d'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_u_h'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_u_dh'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_u_max'))  %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_floodf_u_o'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_floodf_u_d'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_floodf_u_f'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_floodf_u_df'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_floodf_u_max'))  %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatf_u_floodf_u_o'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatf_u_floodf_u_h'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatf_u_floodf_u_f'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatf_u_floodf_u_hf'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatf_u_floodf_u_max'))  %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_u_floodf_u_o'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_u_floodf_u_d'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_u_floodf_u_h'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_u_floodf_u_f'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_u_floodf_u_dh'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_u_floodf_u_df'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_u_floodf_u_hf'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_u_floodf_u_dhf'))  %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_u_floodf_u_max'))    %>%
      
      # mixed thresholds
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_u_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_u_d')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_u_h')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_u_dh')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_u_max')) %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_l_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_l_d')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_l_h')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_l_dh')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_l_max')) %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_floodc_u_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_floodc_u_d')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_floodc_u_f')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_floodc_u_df')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_floodc_u_max')) %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_floodc_l_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_floodc_l_d')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_floodc_l_f')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_floodc_l_df')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_floodc_l_max')) %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatc_l_floodc_u_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatc_l_floodc_u_h')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatc_l_floodc_u_f')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatc_l_floodc_u_hf')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatc_l_floodc_u_max')) %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatc_u_floodc_l_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatc_u_floodc_l_h')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatc_u_floodc_l_f')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatc_u_floodc_l_hf')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatc_u_floodc_l_max')) %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_l_floodc_u_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_l_floodc_u_d')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_l_floodc_u_h')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_l_floodc_u_f')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_l_floodc_u_dh')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_l_floodc_u_df')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_l_floodc_u_hf')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_l_floodc_u_dhf')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_l_floodc_u_max')) %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_u_floodc_l_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_u_floodc_l_d')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_u_floodc_l_h')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_u_floodc_l_f')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_u_floodc_l_dh')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_u_floodc_l_df')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_u_floodc_l_hf')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_u_floodc_l_dhf')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_u_floodc_l_max')) %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_l_floodc_u_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_l_floodc_u_d')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_l_floodc_u_h')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_l_floodc_u_f')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_l_floodc_u_dh')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_l_floodc_u_df')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_l_floodc_u_hf')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_l_floodc_u_dhf')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_l_floodc_u_max')) %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_l_floodc_l_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_l_floodc_l_d')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_l_floodc_l_h')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_l_floodc_l_f')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_l_floodc_l_dh')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_l_floodc_l_df')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_l_floodc_l_hf')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_l_floodc_l_dhf')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_l_floodc_l_max')) %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_u_floodc_l_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_u_floodc_l_d')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_u_floodc_l_h')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_u_floodc_l_f')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_u_floodc_l_dh')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_u_floodc_l_df')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_u_floodc_l_hf')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_u_floodc_l_dhf')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_u_heatc_u_floodc_l_max')) %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_u_floodc_u_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_u_floodc_u_d')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_u_floodc_u_h')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_u_floodc_u_f')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_u_floodc_u_dh')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_u_floodc_u_df')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_u_floodc_u_hf')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_u_floodc_u_dhf')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtc_l_heatc_u_floodc_u_max')) %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_u_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_u_d')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_u_h')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_u_dh')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_u_max')) %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_l_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_l_d')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_l_h')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_l_dh')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_l_max')) %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_floodf_u_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_floodf_u_d')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_floodf_u_f')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_floodf_u_df')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_floodf_u_max')) %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_floodf_l_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_floodf_l_d')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_floodf_l_f')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_floodf_l_df')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_floodf_l_max')) %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatf_l_floodf_u_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatf_l_floodf_u_h')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatf_l_floodf_u_f')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatf_l_floodf_u_hf')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatf_l_floodf_u_max')) %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatf_u_floodf_l_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatf_u_floodf_l_h')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatf_u_floodf_l_f')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatf_u_floodf_l_hf')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'heatf_u_floodf_l_max')) %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_l_floodf_u_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_l_floodf_u_d')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_l_floodf_u_h')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_l_floodf_u_f')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_l_floodf_u_dh')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_l_floodf_u_df')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_l_floodf_u_hf')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_l_floodf_u_dhf')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_l_floodf_u_max')) %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_u_floodf_l_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_u_floodf_l_d')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_u_floodf_l_h')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_u_floodf_l_f')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_u_floodf_l_dh')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_u_floodf_l_df')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_u_floodf_l_hf')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_u_floodf_l_dhf')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_u_floodf_l_max')) %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_l_floodf_u_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_l_floodf_u_d')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_l_floodf_u_h')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_l_floodf_u_f')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_l_floodf_u_dh')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_l_floodf_u_df')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_l_floodf_u_hf')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_l_floodf_u_dhf')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_l_floodf_u_max')) %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_l_floodf_l_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_l_floodf_l_d')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_l_floodf_l_h')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_l_floodf_l_f')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_l_floodf_l_dh')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_l_floodf_l_df')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_l_floodf_l_hf')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_l_floodf_l_dhf')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_l_floodf_l_max')) %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_u_floodf_l_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_u_floodf_l_d')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_u_floodf_l_h')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_u_floodf_l_f')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_u_floodf_l_dh')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_u_floodf_l_df')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_u_floodf_l_hf')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_u_floodf_l_dhf')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_u_heatf_u_floodf_l_max')) %>%
      
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_u_floodf_u_o')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_u_floodf_u_d')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_u_floodf_u_h')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_u_floodf_u_f')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_u_floodf_u_dh')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_u_floodf_u_df')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_u_floodf_u_hf')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_u_floodf_u_dhf')) %>%
      addLayer(rasterize(v_impact, r_droughtc, field = 'droughtf_l_heatf_u_floodf_u_max')) %>%
      
      
      # names
      
      `names<-`(
        c(
          "hist drought l opt",
          "hist drought l sub",
          "hist heat l opt",
          "hist heat l sub",
          "hist flood l opt",
          "hist flood l sub",
          "future drought l opt",
          "future drought l sub",
          "future heat l opt",
          "future heat l sub",
          "future flood l opt",
          "future flood l sub",
          "drought l change opt",
          "drought l change sub",
          "heat l change opt",
          "heat l change sub",
          "flood l change opt",
          "flood l change sub",
          
          "hist drought l opt heat l opt" ,
          "hist drought l sub heat l opt" ,
          "hist drought l opt heat l sub" ,
          "hist drought l sub heat l sub" ,
          "hist drought l heat l max",
          
          "hist drought l opt flood l opt",
          "hist drought l sub flood l opt" ,
          "hist drought l opt flood l sub" ,
          "hist drought l sub flood l sub" ,
          "hist drought l flood l max",
          
          "hist heat l opt flood l opt" ,
          "hist heat l sub flood l opt" ,
          "hist heat l opt flood l sub" ,
          "hist heat l sub flood l sub" ,
          "hist heat l flood l max",
          
          "hist drought l opt heat l opt flood l opt" ,
          "hist drought l sub heat l opt flood l opt" ,
          "hist drought l opt heat l sub flood l opt" ,
          "hist drought l opt heat l opt flood l sub" ,
          "hist drought l sub heat l sub flood l opt" ,
          "hist drought l sub heat l opt flood l sub" ,
          "hist drought l opt heat l sub flood l sub" ,
          "hist drought l sub heat l sub flood l sub" ,
          "hist drought l heat l flood l max",
          
          "future drought l opt heat l opt" ,
          "future drought l sub heat l opt" ,
          "future drought l opt heat l sub" ,
          "future drought l sub heat l sub" ,
          "future drought l heat l max",
          
          "future drought l opt flood l opt",
          "future drought l sub flood l opt" ,
          "future drought l opt flood l sub" ,
          "future drought l sub flood l sub" ,
          "future drought l flood l max",
          
          "future heat l opt flood l opt" ,
          "future heat l sub flood l opt" ,
          "future heat l opt flood l sub" ,
          "future heat l sub flood l sub" ,
          "future heat l flood l max",
          
          "future drought l opt heat l opt flood l opt" ,
          "future drought l sub heat l opt flood l opt" ,
          "future drought l opt heat l sub flood l opt" ,
          "future drought l opt heat l opt flood l sub" ,
          "future drought l sub heat l sub flood l opt" ,
          "future drought l sub heat l opt flood l sub" ,
          "future drought l opt heat l sub flood l sub" ,
          "future drought l sub heat l sub flood l sub",
          "future drought l heat l flood l max",
          
          #upper thresholds
          
          "hist drought u opt",
          "hist drought u sub",
          "hist heat u opt",
          "hist heat u sub",
          "hist flood u opt",
          "hist flood u sub",
          "future drought u opt",
          "future drought u sub",
          "future heat u opt",
          "future heat u sub",
          "future flood u opt",
          "future flood u sub",
          "drought u change opt",
          "drought u change sub",
          "heat u change opt",
          "heat u change sub",
          "flood u change opt",
          "flood u change sub",
          
          "hist drought u opt heat u opt" ,
          "hist drought u sub heat u opt" ,
          "hist drought u opt heat u sub" ,
          "hist drought u sub heat u sub" ,
          "hist drought u heat u max",
          
          "hist drought u opt flood u opt",
          "hist drought u sub flood u opt" ,
          "hist drought u opt flood u sub" ,
          "hist drought u sub flood u sub" ,
          "hist drought u flood u max",
          
          "hist heat u opt flood u opt" ,
          "hist heat u sub flood u opt" ,
          "hist heat u opt flood u sub" ,
          "hist heat u sub flood u sub" ,
          "hist heat u flood u max",
          
          "hist drought u opt heat u opt flood u opt" ,
          "hist drought u sub heat u opt flood u opt" ,
          "hist drought u opt heat u sub flood u opt" ,
          "hist drought u opt heat u opt flood u sub" ,
          "hist drought u sub heat u sub flood u opt" ,
          "hist drought u sub heat u opt flood u sub" ,
          "hist drought u opt heat u sub flood u sub" ,
          "hist drought u sub heat u sub flood u sub" ,
          "hist drought u heat u flood u max",
          
          "future drought u opt heat u opt" ,
          "future drought u sub heat u opt" ,
          "future drought u opt heat u sub" ,
          "future drought u sub heat u sub" ,
          "future drought u heat u max",
          
          "future drought u opt flood u opt",
          "future drought u sub flood u opt" ,
          "future drought u opt flood u sub" ,
          "future drought u sub flood u sub" ,
          "future drought u flood u max",
          
          "future heat u opt flood u opt" ,
          "future heat u sub flood u opt" ,
          "future heat u opt flood u sub" ,
          "future heat u sub flood u sub" ,
          "future heat u flood u max",
          
          "future drought u opt heat u opt flood u opt" ,
          "future drought u sub heat u opt flood u opt" ,
          "future drought u opt heat u sub flood u opt" ,
          "future drought u opt heat u opt flood u sub" ,
          "future drought u sub heat u sub flood u opt" ,
          "future drought u sub heat u opt flood u sub" ,
          "future drought u opt heat u sub flood u sub" ,
          "future drought u sub heat u sub flood u sub",
          "future drought u heat u flood u max",
          
          #mixed thresholds
          
          "hist drought l opt heat u opt",
          "hist drought l sub heat u opt",
          "hist drought l opt heat u sub",
          "hist drought l sub heat u sub",
          "hist drought l heat u max",
          
          "hist drought u opt heat l opt",
          "hist drought u sub heat l opt",
          "hist drought u opt heat l sub",
          "hist drought u sub heat l sub",
          "hist drought u heat l max",
          
          "hist drought l opt flood u opt",
          "hist drought l sub flood u opt",
          "hist drought l opt flood u sub",
          "hist drought l sub flood u sub",
          "hist drought l flood u max",
          
          "hist drought u opt flood l opt",
          "hist drought u sub flood l opt",
          "hist drought u opt flood l sub",
          "hist drought u sub flood l sub",
          "hist drought u flood l max",
          
          "hist heat l opt flood u opt",
          "hist heat l sub flood u opt",
          "hist heat l opt flood u sub",
          "hist heat l sub flood u sub",
          "hist heat l flood u max",
          
          "hist heat u opt flood l opt",
          "hist heat u sub flood l opt",
          "hist heat u opt flood l sub",
          "hist heat u sub flood l sub",
          "hist heat u flood l max",
          
          "hist drought l opt heat l opt flood u opt",
          "hist drought l sub heat l opt flood u opt",
          "hist drought l opt heat l sub flood u opt",
          "hist drought l opt heat l opt flood u sub",
          "hist drought l sub heat l sub flood u opt",
          "hist drought l sub heat l opt flood u sub",
          "hist drought l opt heat l sub flood u sub",
          "hist drought l sub heat l sub flood u sub",
          "hist drought l heat l flood u max",
          
          "hist drought l opt heat u opt flood l opt",
          "hist drought l sub heat u opt flood l opt",
          "hist drought l opt heat u sub flood l opt",
          "hist drought l opt heat u opt flood l sub",
          "hist drought l sub heat u sub flood l opt",
          "hist drought l sub heat u opt flood l sub",
          "hist drought l opt heat u sub flood l sub",
          "hist drought l sub heat u sub flood l sub",
          "hist drought l heat u flood l max",
          
          "hist drought u opt heat l opt flood u opt",
          "hist drought u sub heat l opt flood u opt",
          "hist drought u opt heat l sub flood u opt",
          "hist drought u opt heat l opt flood u sub",
          "hist drought u sub heat l sub flood u opt",
          "hist drought u sub heat l opt flood u sub",
          "hist drought u opt heat l sub flood u sub",
          "hist drought u sub heat l sub flood u sub",
          "hist drought u heat l flood u max",
          
          "hist drought u opt heat l opt flood l opt",
          "hist drought u sub heat l opt flood l opt",
          "hist drought u opt heat l sub flood l opt",
          "hist drought u opt heat l opt flood l sub",
          "hist drought u sub heat l sub flood l opt",
          "hist drought u sub heat l opt flood l sub",
          "hist drought u opt heat l sub flood l sub",
          "hist drought u sub heat l sub flood l sub",
          "hist drought u heat l flood l max",
          
          "hist drought u opt heat u opt flood l opt",
          "hist drought u sub heat u opt flood l opt",
          "hist drought u opt heat u sub flood l opt",
          "hist drought u opt heat u opt flood l sub",
          "hist drought u sub heat u sub flood l opt",
          "hist drought u sub heat u opt flood l sub",
          "hist drought u opt heat u sub flood l sub",
          "hist drought u sub heat u sub flood l sub",
          "hist drought u heat u flood l max",
          
          "hist drought l opt heat u opt flood u opt",
          "hist drought l sub heat u opt flood u opt",
          "hist drought l opt heat u sub flood u opt",
          "hist drought l opt heat u opt flood u sub",
          "hist drought l sub heat u sub flood u opt",
          "hist drought l sub heat u opt flood u sub",
          "hist drought l opt heat u sub flood u sub",
          "hist drought l sub heat u sub flood u sub",
          "hist drought l heat u flood u max",
          
          "future drought l opt heat u opt",
          "future drought l sub heat u opt",
          "future drought l opt heat u sub",
          "future drought l sub heat u sub",
          "future drought l heat u max",
          
          "future drought u opt heat l opt",
          "future drought u sub heat l opt",
          "future drought u opt heat l sub",
          "future drought u sub heat l sub",
          "future drought u heat l max",
          
          "future drought l opt flood u opt",
          "future drought l sub flood u opt",
          "future drought l opt flood u sub",
          "future drought l sub flood u sub",
          "future drought l flood u max",
          
          "future drought u opt flood l opt",
          "future drought u sub flood l opt",
          "future drought u opt flood l sub",
          "future drought u sub flood l sub",
          "future drought u flood l max",
          
          "future heat l opt flood u opt",
          "future heat l sub flood u opt",
          "future heat l opt flood u sub",
          "future heat l sub flood u sub",
          "future heat l flood u max",
          
          "future heat u opt flood l opt",
          "future heat u sub flood l opt",
          "future heat u opt flood l sub",
          "future heat u sub flood l sub",
          "future heat u flood l max",
          
          "future drought l opt heat l opt flood u opt",
          "future drought l sub heat l opt flood u opt",
          "future drought l opt heat l sub flood u opt",
          "future drought l opt heat l opt flood u sub",
          "future drought l sub heat l sub flood u opt",
          "future drought l sub heat l opt flood u sub",
          "future drought l opt heat l sub flood u sub",
          "future drought l sub heat l sub flood u sub",
          "future drought l heat l flood u max",
          
          "future drought l opt heat u opt flood l opt",
          "future drought l sub heat u opt flood l opt",
          "future drought l opt heat u sub flood l opt",
          "future drought l opt heat u opt flood l sub",
          "future drought l sub heat u sub flood l opt",
          "future drought l sub heat u opt flood l sub",
          "future drought l opt heat u sub flood l sub",
          "future drought l sub heat u sub flood l sub",
          "future drought l heat u flood l max",
          
          "future drought u opt heat l opt flood u opt",
          "future drought u sub heat l opt flood u opt",
          "future drought u opt heat l sub flood u opt",
          "future drought u opt heat l opt flood u sub",
          "future drought u sub heat l sub flood u opt",
          "future drought u sub heat l opt flood u sub",
          "future drought u opt heat l sub flood u sub",
          "future drought u sub heat l sub flood u sub",
          "future drought u heat l flood u max",
          
          "future drought u opt heat l opt flood l opt",
          "future drought u sub heat l opt flood l opt",
          "future drought u opt heat l sub flood l opt",
          "future drought u opt heat l opt flood l sub",
          "future drought u sub heat l sub flood l opt",
          "future drought u sub heat l opt flood l sub",
          "future drought u opt heat l sub flood l sub",
          "future drought u sub heat l sub flood l sub",
          "future drought u heat l flood l max",
          
          "future drought u opt heat u opt flood l opt",
          "future drought u sub heat u opt flood l opt",
          "future drought u opt heat u sub flood l opt",
          "future drought u opt heat u opt flood l sub",
          "future drought u sub heat u sub flood l opt",
          "future drought u sub heat u opt flood l sub",
          "future drought u opt heat u sub flood l sub",
          "future drought u sub heat u sub flood l sub",
          "future drought u heat u flood l max",
          
          "future drought l opt heat u opt flood u opt",
          "future drought l sub heat u opt flood u opt",
          "future drought l opt heat u sub flood u opt",
          "future drought l opt heat u opt flood u sub",
          "future drought l sub heat u sub flood u opt",
          "future drought l sub heat u opt flood u sub",
          "future drought l opt heat u sub flood u sub",
          "future drought l sub heat u sub flood u sub",
          "future drought l heat u flood u max"
        )
      )
  }

# results in rB_impact_file
rB_impact_file_make_f <- function(rB_impact, ISO, crop) {
  rB_impact %>%
    writeRaster(
      paste0("data/", ISO, "/", crop, "/rB_impact.tif"),
      bylayer = TRUE, 
      format="GTiff",
      suffix = 'names',
      overwrite = TRUE
    )
}

### Reload impact raster files

##### Past Lower threshold

# results in r_droughtc_l_heatc_l_max
r_droughtc_l_heatc_l_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_hist.drought.l.heat.l.max.tif"))
}

# results in r_droughtc_l_floodc_l_max
r_droughtc_l_floodc_l_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_hist.drought.l.flood.l.max.tif"))
}

# results in r_heatc_l_floodc_l_max
r_heatc_l_floodc_l_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_hist.heat.l.flood.l.max.tif"))
}
 
# results in r_droughtc_l_heatc_l_floodc_l_max
r_droughtc_l_heatc_l_floodc_l_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_hist.drought.l.heat.l.flood.l.max.tif"))
  }

##### Past Upper threshold

# results in r_droughtc_u_heatc_u_max
r_droughtc_u_heatc_u_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_hist.drought.u.heat.u.max.tif"))
  }

# results in r_droughtc_u_floodc_u_max
r_droughtc_u_floodc_u_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_hist.drought.u.flood.u.max.tif"))
  }

# results in r_heatc_u_floodc_u_max
r_heatc_u_floodc_u_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_hist.heat.u.flood.u.max.tif"))
  }

# results in r_droughtc_u_heatc_u_floodc_u_max
r_droughtc_u_heatc_u_floodc_u_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_hist.drought.u.heat.u.flood.u.max.tif"))
  }   


#### Past Mixed threshold

# results in r_droughtc_l_heatc_u_max
r_droughtc_l_heatc_u_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_hist.drought.l.heat.u.max.tif"))
  }

# results in r_droughtc_u_heatc_l_max
r_droughtc_u_heatc_l_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_hist.drought.u.heat.l.max.tif"))
  }

# results in r_droughtc_l_floodc_u_max
r_droughtc_l_floodc_u_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_hist.drought.l.flood.u.max.tif"))
  }

# results in r_droughtc_u_floodc_l_max
r_droughtc_u_floodc_l_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_hist.drought.u.flood.l.max.tif"))
  }

# results in r_heatc_l_floodc_u_max
r_heatc_l_floodc_u_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_hist.heat.l.flood.u.max.tif"))
  }

# results in r_heatc_u_floodc_l_max
r_heatc_u_floodc_l_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_hist.heat.u.flood.l.max.tif"))
  }

# results in r_droughtc_l_heatc_l_floodc_u_max
r_droughtc_l_heatc_l_floodc_u_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_hist.drought.l.heat.l.flood.u.max.tif"))
  }  

# results in r_droughtc_l_heatc_u_floodc_l_max
r_droughtc_l_heatc_u_floodc_l_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_hist.drought.l.heat.u.flood.l.max.tif"))
  }  

# results in r_droughtc_u_heatc_l_floodc_l_max
r_droughtc_u_heatc_l_floodc_l_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_hist.drought.u.heat.l.flood.l.max.tif"))
  }  

# results in r_droughtc_l_heatc_u_floodc_u_max
r_droughtc_l_heatc_u_floodc_u_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_hist.drought.l.heat.u.flood.u.max.tif"))
  }  

# results in r_droughtc_u_heatc_l_floodc_u_max
r_droughtc_u_heatc_l_floodc_u_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_hist.drought.u.heat.l.flood.u.max.tif"))
  }  

# results in r_droughtc_u_heatc_u_floodc_l_max
r_droughtc_u_heatc_u_floodc_l_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_hist.drought.u.heat.u.flood.l.max.tif"))
  }  

##### Future Lower threshold

# results in r_droughtf_l_heatf_l_max
r_droughtf_l_heatf_l_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_future.drought.l.heat.l.max.tif"))
  }

# results in r_droughtf_l_floodf_l_max
r_droughtf_l_floodf_l_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_future.drought.l.flood.l.max.tif"))
  }

# results in r_heatf_l_floodf_l_max
r_heatf_l_floodf_l_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_future.heat.l.flood.l.max.tif"))
  }

# results in r_droughtf_l_heatf_l_floodf_l_max
r_droughtf_l_heatf_l_floodf_l_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_future.drought.l.heat.l.flood.l.max.tif"))
  }

##### Future Upper threshold

# results in r_droughtf_u_heatf_u_max
r_droughtf_u_heatf_u_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_future.drought.u.heat.u.max.tif"))
  }

# results in r_droughtf_u_floodf_u_max
r_droughtf_u_floodf_u_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_future.drought.u.flood.u.max.tif"))
  }

# results in r_heatf_u_floodf_u_max
r_heatf_u_floodf_u_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_future.heat.u.flood.u.max.tif"))
  }

# results in r_droughtf_u_heatf_u_floodf_u_max
r_droughtf_u_heatf_u_floodf_u_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_future.drought.u.heat.u.flood.u.max.tif"))
  }   


#### Future Mixed threshold

# results in r_droughtf_l_heatf_u_max
r_droughtf_l_heatf_u_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_future.drought.l.heat.u.max.tif"))
  }

# results in r_droughtf_u_heatf_l_max
r_droughtf_u_heatf_l_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_future.drought.u.heat.l.max.tif"))
  }

# results in r_droughtf_l_floodf_u_max
r_droughtf_l_floodf_u_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_future.drought.l.flood.u.max.tif"))
  }

# results in r_droughtf_u_floodf_l_max
r_droughtf_u_floodf_l_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_future.drought.u.flood.l.max.tif"))
  }

# results in r_heatf_l_floodf_u_max
r_heatf_l_floodf_u_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_future.heat.l.flood.u.max.tif"))
  }

# results in r_heatf_u_floodf_l_max
r_heatf_u_floodf_l_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_future.heat.u.flood.l.max.tif"))
  }

# results in r_droughtf_l_heatf_l_floodf_u_max
r_droughtf_l_heatf_l_floodf_u_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_future.drought.l.heat.l.flood.u.max.tif"))
  }  

# results in r_droughtf_l_heatf_u_floodf_l_max
r_droughtf_l_heatf_u_floodf_l_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_future.drought.l.heat.u.flood.l.max.tif"))
  }  

# results in r_droughtf_u_heatf_l_floodf_l_max
r_droughtf_u_heatf_l_floodf_l_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_future.drought.u.heat.l.flood.l.max.tif"))
  }  

# results in r_droughtf_l_heatf_u_floodf_u_max
r_droughtf_l_heatf_u_floodf_u_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_future.drought.l.heat.u.flood.u.max.tif"))
  }  

# results in r_droughtf_u_heatf_l_floodf_u_max
r_droughtf_u_heatf_l_floodf_u_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_future.drought.u.heat.l.flood.u.max.tif"))
  }  

# results in r_droughtf_u_heatf_u_floodf_l_max
r_droughtf_u_heatf_u_floodf_l_max_get_f <-
  function(rB_impact_file, ISO, crop) {
    raster(paste0("data/", ISO, "/", crop, "/rB_impact_future.drought.u.heat.u.flood.l.max.tif"))
  } 


### Plot Spatial Impact Data

#### Past climate
#### Lower thresholds

# results in r_droughtc_l_heatc_l_max_plot
r_droughtc_l_heatc_l_max_plot_f <-
  function(r_droughtc_l_heatc_l_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {

    gplot(r_droughtc_l_heatc_l_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nPast\n",
          "Impact\n",
          "drought (l)\n",
          "heat (l)\n\n",
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

# results in r_droughtc_l_floodc_l_max_plot
r_droughtc_l_floodc_l_max_plot_f <-
  function(r_droughtc_l_floodc_l_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    gplot(r_droughtc_l_floodc_l_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nPast\n",
          "Impact\n",
          "drought (l)\n",
          "flood (l)\n\n",
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

# results in r_heatc_l_floodc_l_max_plot
r_heatc_l_floodc_l_max_plot_f <-
  function(r_heatc_l_floodc_l_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    gplot(r_heatc_l_floodc_l_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nPast\n",
          "Impact\n",
          "heat (l)\n",
          "flood (l)\n\n",
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


# results in r_droughtc_l_heatc_l_floodc_l_max_plot
r_droughtc_l_heatc_l_floodc_l_max_plot_f <-
  function(r_droughtc_l_heatc_l_floodc_l_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    gplot(r_droughtc_l_heatc_l_floodc_l_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nPast\n",
          "Impact\n",
          "drought (l)\n",
          "heat (l)\n",
          "flood (l)\n\n",
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


#### Upper thresholds


# results in r_droughtc_u_heatc_u_max_plot
r_droughtc_u_heatc_u_max_plot_f <-
  function(r_droughtc_u_heatc_u_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    gplot(r_droughtc_u_heatc_u_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nPast\n",
          "Impact\n",
          "drought (u)\n",
          "heat (u)\n\n",
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

# results in r_droughtc_u_floodc_u_max_plot
r_droughtc_u_floodc_u_max_plot_f <-
  function(r_droughtc_u_floodc_u_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    gplot(r_droughtc_u_floodc_u_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nPast\n",
          "Impact\n",
          "drought (u)\n",
          "flood (u)\n\n",
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

# results in r_heatc_u_floodc_u_max_plot
r_heatc_u_floodc_u_max_plot_f <-
  function(r_heatc_u_floodc_u_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    gplot(r_heatc_u_floodc_u_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nPast\n",
          "Impact\n",
          "heat (u)\n",
          "flood (u)\n\n",
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


# results in r_droughtc_u_heatc_u_floodc_u_max_plot
r_droughtc_u_heatc_u_floodc_u_max_plot_f <-
  function(r_droughtc_u_heatc_u_floodc_u_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    gplot(r_droughtc_u_heatc_u_floodc_u_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nPast\n",
          "Impact\n",
          "drought (u)\n",
          "heat (u)\n",
          "flood (u)\n\n",
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



#### Mixed thresholds


# results in r_droughtc_l_heatc_l_max_plot
r_droughtc_l_heatc_u_max_plot_f <-
  function(r_droughtc_l_heatc_u_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    
    gplot(r_droughtc_l_heatc_u_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nPast\n",
          "Impact\n",
          "drought (l)\n",
          "heat (u)\n\n",
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




# results in r_droughtc_u_heatc_l_max_plot
r_droughtc_u_heatc_l_max_plot_f <-
  function(r_droughtc_u_heatc_l_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    
    gplot(r_droughtc_u_heatc_l_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nPast\n",
          "Impact\n",
          "drought (u)\n",
          "heat (l)\n\n",
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



r_droughtc_l_floodc_u_max_plot_f <-
  function(r_droughtc_l_floodc_u_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    
    gplot(r_droughtc_l_floodc_u_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nPast\n",
          "Impact\n",
          "drought (l)\n",
          "flood (u)\n\n",
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


r_droughtc_u_floodc_l_max_plot_f <-
  function(r_droughtc_u_floodc_l_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    
    gplot(r_droughtc_u_floodc_l_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nPast\n",
          "Impact\n",
          "drought (u)\n",
          "flood (l)\n\n",
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



r_heatc_l_floodc_u_max_plot_f <-
  function(r_heatc_l_floodc_u_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    
    gplot(r_heatc_l_floodc_u_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nPast\n",
          "Impact\n",
          "heat (l)\n",
          "flood (u)\n\n",
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




r_heatc_u_floodc_l_max_plot_f <-
  function(r_heatc_u_floodc_l_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    
    gplot(r_heatc_u_floodc_l_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nPast\n",
          "Impact\n",
          "heat (l)\n",
          "flood (u)\n\n",
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

r_droughtc_l_heatc_l_floodc_u_max_plot_f <-
  function(r_droughtc_l_heatc_l_floodc_u_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    
    gplot(r_droughtc_l_heatc_l_floodc_u_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nPast\n",
          "Impact\n",
          "drought (l)",
          "heat (l)\n",
          "flood (u)\n\n",
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


r_droughtc_l_heatc_u_floodc_l_max_plot_f <-
  function(r_droughtc_l_heatc_u_floodc_l_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    
    gplot(r_droughtc_l_heatc_u_floodc_l_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nPast\n",
          "Impact\n",
          "drought (l)",
          "heat (u)\n",
          "flood (l)\n\n",
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


r_droughtc_u_heatc_l_floodc_l_max_plot_f <-
  function(r_droughtc_u_heatc_l_floodc_l_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    
    gplot(r_droughtc_u_heatc_l_floodc_l_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nPast\n",
          "Impact\n",
          "drought (u)",
          "heat (l)\n",
          "flood (l)\n\n",
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


r_droughtc_l_heatc_u_floodc_u_max_plot_f <-
  function(r_droughtc_l_heatc_u_floodc_u_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    
    gplot(r_droughtc_l_heatc_u_floodc_u_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nPast\n",
          "Impact\n",
          "drought (l)",
          "heat (u)\n",
          "flood (u)\n\n",
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


r_droughtc_u_heatc_l_floodc_u_max_plot_f <-
  function(r_droughtc_u_heatc_l_floodc_u_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    
    gplot(r_droughtc_u_heatc_l_floodc_u_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nPast\n",
          "Impact\n",
          "drought (u)",
          "heat (l)\n",
          "flood (u)\n\n",
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


r_droughtc_u_heatc_u_floodc_l_max_plot_f <-
  function(r_droughtc_u_heatc_u_floodc_l_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    
    gplot(r_droughtc_u_heatc_u_floodc_l_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nPast\n",
          "Impact\n",
          "drought (u)",
          "heat (u)\n",
          "flood (l)\n\n",
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

#### Future climate
#### Lower thresholds

# results in r_droughtf_l_heatf_l_max_plot
r_droughtf_l_heatf_l_max_plot_f <-
  function(r_droughtf_l_heatf_l_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    
    gplot(r_droughtf_l_heatf_l_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nFuture\n",
          "Impact\n",
          "drought (l)\n",
          "heat (l)\n\n",
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

# results in r_droughtf_l_floodf_l_max_plot
r_droughtf_l_floodf_l_max_plot_f <-
  function(r_droughtf_l_floodf_l_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    gplot(r_droughtf_l_floodf_l_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nFuture\n",
          "Impact\n",
          "drought (l)\n",
          "flood (l)\n\n",
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

# results in r_heatf_l_floodf_l_max_plot
r_heatf_l_floodf_l_max_plot_f <-
  function(r_heatf_l_floodf_l_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    gplot(r_heatf_l_floodf_l_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nFuture\n",
          "Impact\n",
          "heat (l)\n",
          "flood (l)\n\n",
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


# results in r_droughtf_l_heatf_l_floodf_l_max_plot
r_droughtf_l_heatf_l_floodf_l_max_plot_f <-
  function(r_droughtf_l_heatf_l_floodf_l_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    gplot(r_droughtf_l_heatf_l_floodf_l_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nFuture\n",
          "Impact\n",
          "drought (l)\n",
          "heat (l)\n",
          "flood (l)\n\n",
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


#### Upper thresholds


# results in r_droughtf_u_heatf_u_max_plot
r_droughtf_u_heatf_u_max_plot_f <-
  function(r_droughtf_u_heatf_u_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    gplot(r_droughtf_u_heatf_u_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nFuture\n",
          "Impact\n",
          "drought (u)\n",
          "heat (u)\n\n",
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

# results in r_droughtf_u_floodf_u_max_plot
r_droughtf_u_floodf_u_max_plot_f <-
  function(r_droughtf_u_floodf_u_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    gplot(r_droughtf_u_floodf_u_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nFuture\n",
          "Impact\n",
          "drought (u)\n",
          "flood (u)\n\n",
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

# results in r_heatf_u_floodf_u_max_plot
r_heatf_u_floodf_u_max_plot_f <-
  function(r_heatf_u_floodf_u_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    gplot(r_heatf_u_floodf_u_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nFuture\n",
          "Impact\n",
          "heat (u)\n",
          "flood (u)\n\n",
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


# results in r_droughtf_u_heatf_u_floodf_u_max_plot
r_droughtf_u_heatf_u_floodf_u_max_plot_f <-
  function(r_droughtf_u_heatf_u_floodf_u_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    gplot(r_droughtf_u_heatf_u_floodf_u_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nFuture\n",
          "Impact\n",
          "drought (u)\n",
          "heat (u)\n",
          "flood (u)\n\n",
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



#### Mixed thresholds


# results in r_droughtf_l_heatf_l_max_plot
r_droughtf_l_heatf_u_max_plot_f <-
  function(r_droughtf_l_heatf_u_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    
    gplot(r_droughtf_l_heatf_u_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nFuture\n",
          "Impact\n",
          "drought (l)\n",
          "heat (u)\n\n",
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




# results in r_droughtf_u_heatf_l_max_plot
r_droughtf_u_heatf_l_max_plot_f <-
  function(r_droughtf_u_heatf_l_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    
    gplot(r_droughtf_u_heatf_l_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nFuture\n",
          "Impact\n",
          "drought (u)\n",
          "heat (l)\n\n",
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



r_droughtf_l_floodf_u_max_plot_f <-
  function(r_droughtf_l_floodf_u_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    
    gplot(r_droughtf_l_floodf_u_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nFuture\n",
          "Impact\n",
          "drought (l)\n",
          "flood (u)\n\n",
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


r_droughtf_u_floodf_l_max_plot_f <-
  function(r_droughtf_u_floodf_l_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    
    gplot(r_droughtf_u_floodf_l_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nFuture\n",
          "Impact\n",
          "drought (u)\n",
          "flood (l)\n\n",
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



r_heatf_l_floodf_u_max_plot_f <-
  function(r_heatf_l_floodf_u_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    
    gplot(r_heatf_l_floodf_u_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nFuture\n",
          "Impact\n",
          "heat (l)\n",
          "flood (u)\n\n",
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




r_heatf_u_floodf_l_max_plot_f <-
  function(r_heatf_u_floodf_l_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    
    gplot(r_heatf_u_floodf_l_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nFuture\n",
          "Impact\n",
          "heat (l)\n",
          "flood (u)\n\n",
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

r_droughtf_l_heatf_l_floodf_u_max_plot_f <-
  function(r_droughtf_l_heatf_l_floodf_u_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    
    gplot(r_droughtf_l_heatf_l_floodf_u_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nFuture\n",
          "Impact\n",
          "drought (l)",
          "heat (l)\n",
          "flood (u)\n\n",
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


r_droughtf_l_heatf_u_floodf_l_max_plot_f <-
  function(r_droughtf_l_heatf_u_floodf_l_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    
    gplot(r_droughtf_l_heatf_u_floodf_l_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nFuture\n",
          "Impact\n",
          "drought (l)",
          "heat (u)\n",
          "flood (l)\n\n",
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


r_droughtf_u_heatf_l_floodf_l_max_plot_f <-
  function(r_droughtf_u_heatf_l_floodf_l_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    
    gplot(r_droughtf_u_heatf_l_floodf_l_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nFuture\n",
          "Impact\n",
          "drought (u)",
          "heat (l)\n",
          "flood (l)\n\n",
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


r_droughtf_l_heatf_u_floodf_u_max_plot_f <-
  function(r_droughtf_l_heatf_u_floodf_u_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    
    gplot(r_droughtf_l_heatf_u_floodf_u_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nFuture\n",
          "Impact\n",
          "drought (l)",
          "heat (u)\n",
          "flood (u)\n\n",
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


r_droughtf_u_heatf_l_floodf_u_max_plot_f <-
  function(r_droughtf_u_heatf_l_floodf_u_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    
    gplot(r_droughtf_u_heatf_l_floodf_u_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nFuture\n",
          "Impact\n",
          "drought (u)",
          "heat (l)\n",
          "flood (u)\n\n",
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


r_droughtf_u_heatf_u_floodf_l_max_plot_f <-
  function(r_droughtf_u_heatf_u_floodf_l_max,
           v_ISO1,
           v_crop_ISO_lc_rcl_agg,
           v_ISO_extent,
           world,
           ISO,
           crop,
           flood) {
    
    gplot(r_droughtf_u_heatf_u_floodf_l_max, maxpixels = 50000) + #this uses gplot from the rastervis package
      geom_tile(aes(fill = factor(value, levels = c("0", "1", "2", "3", "4", "5", "6","7", "8", "NA"),
                                  labels = c("No Risk", "D", "H", "F", "DH", "DF", "HF", "DHF", "Mix", "NA"))), alpha = 1) +
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
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 1),
        fill = NA,
        col = 'dark blue',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      geom_sf(
        data = dplyr::filter(v_crop_ISO_lc_rcl_agg, layer == 2),
        fill = NA,
        col = 'dark green',
        na.rm = TRUE,
        inherit.aes = FALSE
      )  +
      scale_fill_manual(
        values = c(
          "No Risk"  = rgb(127, 201, 127, maxColorValue = 255), # all 3 opt
          "D"  = rgb(253, 192, 134, maxColorValue = 255), #  drought
          "H"  = rgb(255, 255, 153, maxColorValue = 255), # heat
          "F"  = rgb(190, 174, 212, maxColorValue = 255), # flood  
          "DH"  = rgb(191, 91, 23, maxColorValue = 255), # drought and heat
          "DF"  = rgb(56, 108, 176, maxColorValue = 255), # drought and flood
          "HF"  = rgb(102, 102, 102, maxColorValue = 255),  # heat and flood
          "DHF"  = rgb(240, 2, 127, maxColorValue = 255),   # all 3 sub
          "Mix"  = rgb(125, 125, 125, maxColorValue = 255),   # mixture uncertain
          "NA" = rgb(255, 255, 255, maxColorValue = 255)   # NA
        ),
        na.translate = F
      ) +
      xlim ((v_ISO_extent@xmin - 1), (v_ISO_extent@xmax) + 1) +
      ylim ((v_ISO_extent@ymin - 1), (v_ISO_extent@ymax) + 1) +
      labs(
        fill = paste0(
          "--------------------\nFuture\n",
          "Impact\n",
          "drought (u)",
          "heat (u)\n",
          "flood (l)\n\n",
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


  ## Impact zonal statistics
### Prepare Climate Risk Profile Data
#### Drought
##### Lower threshold

# results in dB_droughtc_l_summary
dB_droughtc_l_summary_make_f <-
  function(rB_impact, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      subset(rB_impact, 2),
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

# results in dB_droughtc_l_summary_plot
dB_droughtc_l_summary_plot_f <-
  function(dB_droughtc_l_summary, drought, ISO, crop) {
    ggplot(dB_droughtc_l_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "Impact") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0(
        "Past Impact of ",
        drought,
        " - ",
        ISO,
        " - ",
        crop,
        " - Lower threshold"
      )) +
      ylim(0, 1) +
      theme(
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }

# results in dB_droughtf_l_summary
dB_droughtf_l_summary_make_f <-
  function(rB_impact, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      subset(rB_impact, 8),
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

# results in dB_droughtf_l_summary_plot
dB_droughtf_l_summary_plot_f <-
  function(dB_droughtf_l_summary, drought, ISO, crop) {
    ggplot(dB_droughtf_l_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "Impact") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0(
        "Future Impact of ",
        drought,
        " - ",
        ISO,
        " - ",
        crop,
        " - Lower threshold"
      )) +
      ylim(0, 1) +
      theme(
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }

# results in dB_drought_change_l_summary
dB_drought_change_l_summary_make_f <-
  function(rB_impact, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      subset(rB_impact, 14),
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

# results in dB_drought_change_l_summary_plot
dB_drought_change_l_summary_plot_f <-
  function(dB_drought_change_l_summary,
           drought,
           ISO,
           crop) {
    ggplot(dB_drought_change_l_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "Impact") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0(
        "Change in Impact of ",
        drought,
        " - ",
        ISO,
        " - ",
        crop,
        " - Lower threshold"
      )) +
      ylim(-1, 1) +
      theme(
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }

##### Upper threshold

# results in dB_droughtc_u_summary
dB_droughtc_u_summary_make_f <-
  function(rB_impact, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      subset(rB_impact, 68),
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

# results in dB_droughtc_u_summary_plot
dB_droughtc_u_summary_plot_f <-
  function(dB_droughtc_u_summary, drought, ISO, crop) {
    ggplot(dB_droughtc_u_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "Impact") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0(
        "Past Impact of ",
        drought,
        " - ",
        ISO,
        " - ",
        crop,
        " - Upper  threshold"
      )) +
      ylim(0, 1) +
      theme(
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }

# results in dB_droughtf_u_summary
dB_droughtf_u_summary_make_f <-
  function(rB_impact, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      subset(rB_impact, 74),
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

# results in dB_droughtf_u_summary_plot
dB_droughtf_u_summary_plot_f <-
  function(dB_droughtf_u_summary, drought, ISO, crop) {
    ggplot(dB_droughtf_u_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "Impact") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0(
        "Future Impact of ",
        drought,
        " - ",
        ISO,
        " - ",
        crop,
        " - Upper threshold"
      )) +
      ylim(0, 1) +
      theme(
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }

# results in dB_drought_change_u_summary
dB_drought_change_u_summary_make_f <-
  function(rB_impact, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      subset(rB_impact, 80),
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

# results in dB_drought_change_u_summary_plot
dB_drought_change_u_summary_plot_f <-
  function(dB_drought_change_u_summary,
           drought,
           ISO,
           crop) {
    ggplot(dB_drought_change_u_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "Impact") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0(
        "Change in Impact of ",
        drought,
        " - ",
        ISO,
        " - ",
        crop,
        " - Upper threshold"
      )) +
      ylim(-1, 1) +
      theme(
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }

#### Heat
##### Lower threshold


# results in dB_heatc_l_summary_values
dB_heatc_l_summary_values_make_f <-
  function(rB_impact, v_crop_ISO_lc_rcl_agg) {
    exact_extract(subset(rB_impact, 4),
                  v_crop_ISO_lc_rcl_agg,
                  include_cols = c("crop_ISO1"))
  }

# results in dB_heatc_l_summary
dB_heatc_l_summary_make_f <-
  function(rB_impact, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      subset(rB_impact, 4),
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75),
      default_weight = 1,
      append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

# results in dB_heatc_l_summary_plot
dB_heatc_l_summary_plot_f <-
  function(dB_heatc_l_summary, heat, ISO, crop) {
    ggplot(dB_heatc_l_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "Impact") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0("Past Impact of ",
                          heat,
                          " - ",
                          ISO,
                          " - ",
                          crop,
                          " - Lower threshold")) +
      ylim(0, 1) +
      theme(
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }

# results in dB_heatf_l_summary
dB_heatf_l_summary_make_f <-
  function(rB_impact, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      subset(rB_impact, 10),
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

# results in dB_heatf_l_summary_plot
dB_heatf_l_summary_plot_f <-
  function(dB_heatf_l_summary, heat, ISO, crop) {
    ggplot(dB_heatf_l_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "Impact") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0("Future Impact of ",
                          heat,
                          " - ",
                          ISO,
                          " - ",
                          crop,
                          " - Lower threshold")) +
      ylim(0, 1) +
      theme(
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }

# results in dB_heat_change_l_summary
dB_heat_change_l_summary_make_f <-
  function(rB_impact, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      subset(rB_impact, 16),
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

# results in dB_heat_change_l_summary_plot
dB_heat_change_l_summary_plot_f <-
  function(dB_heat_change_l_summary, heat, ISO, crop) {
    ggplot(dB_heat_change_l_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "Impact") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0(
        "Change in Impact of ",
        heat,
        " - ",
        ISO,
        " - ",
        crop,
        " - Lower threshold"
      )) +
      ylim(-1, 1) +
      theme(
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }

##### Upper threshold

# results in dB_heatc_u_summary
dB_heatc_u_summary_make_f <-
  function(rB_impact, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      subset(rB_impact, 70),
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

# results in dB_heatc_u_summary_plot
dB_heatc_u_summary_plot_f <-
  function(dB_heatc_u_summary, heat, ISO, crop) {
    ggplot(dB_heatc_u_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "Impact") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0("Past Impact of ",
                          heat,
                          " - ",
                          ISO,
                          " - ",
                          crop,
                          " - Upper  threshold")) +
      ylim(0, 1) +
      theme(
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }

# results in dB_heatf_u_summary
dB_heatf_u_summary_make_f <-
  function(rB_impact, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      subset(rB_impact, 76),
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

# results in dB_heatf_u_summary_plot
dB_heatf_u_summary_plot_f <-
  function(dB_heatf_u_summary, heat, ISO, crop) {
    ggplot(dB_heatf_u_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "Impact") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0("Future Impact of ",
                          heat,
                          " - ",
                          ISO,
                          " - ",
                          crop,
                          " - Upper threshold")) +
      ylim(0, 1) +
      theme(
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }

# results in dB_heat_change_u_summary
dB_heat_change_u_summary_make_f <-
  function(rB_impact, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      subset(rB_impact, 82),
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

# results in dB_heat_change_u_summary_plot
dB_heat_change_u_summary_plot_f <-
  function(dB_heat_change_u_summary, heat, ISO, crop) {
    ggplot(dB_heat_change_u_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "Impact") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0(
        "Change in Impact of ",
        heat,
        " - ",
        ISO,
        " - ",
        crop,
        " - Upper threshold"
      )) +
      ylim(-1, 1) +
      theme(
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }

#### Flood
##### Lower threshold

# results in dB_floodc_l_summary
dB_floodc_l_summary_make_f <-
  function(rB_impact, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      subset(rB_impact, 6),
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

# results in dB_floodc_l_summary_plot
dB_floodc_l_summary_plot_f <-
  function(dB_floodc_l_summary, flood, ISO, crop) {
    ggplot(dB_floodc_l_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "Impact") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0("Past Impact of ",
                          flood,
                          " - ",
                          ISO,
                          " - ",
                          crop,
                          " - Lower threshold")) +
      ylim(0, 1) +
      theme(
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }

# results in dB_floodf_l_summary
dB_floodf_l_summary_make_f <-
  function(rB_impact, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      subset(rB_impact, 12),
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

# results in dB_floodf_l_summary_plot
dB_floodf_l_summary_plot_f <-
  function(dB_floodf_l_summary, flood, ISO, crop) {
    ggplot(dB_floodf_l_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "Impact") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0(
        "Future Impact of ",
        flood,
        " - ",
        ISO,
        " - ",
        crop,
        " - Lower threshold"
      )) +
      ylim(0, 1) +
      theme(
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }

# results in dB_flood_change_l_summary
dB_flood_change_l_summary_make_f <-
  function(rB_impact, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      subset(rB_impact, 18),
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

# results in dB_flood_change_l_summary_plot
dB_flood_change_l_summary_plot_f <-
  function(dB_flood_change_l_summary,
           flood,
           ISO,
           crop) {
    ggplot(dB_flood_change_l_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "Impact") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0(
        "Change in Impact of ",
        flood,
        " - ",
        ISO,
        " - ",
        crop,
        " - Lower threshold"
      )) +
      ylim(-1, 1) +
      theme(
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }

##### Upper threshold

# results in dB_floodc_u_summary
dB_floodc_u_summary_make_f <-
  function(rB_impact, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      subset(rB_impact, 72),
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

# results in dB_floodc_u_summary_plot
dB_floodc_u_summary_plot_f <-
  function(dB_floodc_u_summary, flood, ISO, crop) {
    ggplot(dB_floodc_u_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "Impact") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0("Past Impact of ",
                          flood,
                          " - ",
                          ISO,
                          " - ",
                          crop,
                          " - Upper  threshold")) +
      ylim(0, 1) +
      theme(
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }

# results in dB_floodf_u_summary
dB_floodf_u_summary_make_f <-
  function(rB_impact, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      subset(rB_impact, 78),
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

# results in dB_floodf_u_summary_plot
dB_floodf_u_summary_plot_f <-
  function(dB_floodf_u_summary, flood, ISO, crop) {
    ggplot(dB_floodf_u_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "Impact") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0(
        "Future Impact of ",
        flood,
        " - ",
        ISO,
        " - ",
        crop,
        " - Upper threshold"
      )) +
      ylim(0, 1) +
      theme(
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }

# results in dB_flood_change_u_summary
dB_flood_change_u_summary_make_f <-
  function(rB_impact, v_crop_ISO_lc_rcl_agg) {
    exact_extract(
      subset(rB_impact, 84),
      v_crop_ISO_lc_rcl_agg,
      fun = c('min', 'max', 'mean', 'stdev', 'median', 'quantile', 'count'),
      quantiles = c(0.25, 0.75),
      append_cols = c("crop_ISO1")
    ) %>%
      mutate(IQR = (q75 - q25))  %>%
      mutate(l_whisker = (q25 -  (1.5 * IQR))) %>%
      mutate(u_whisker = (q75 +  (1.5 * IQR))) %>%
      mutate(max_min = pmax(min, l_whisker)) %>%
      mutate(min_max = pmin(max, u_whisker))
  }

# results in dB_flood_change_u_summary_plot
dB_flood_change_u_summary_plot_f <-
  function(dB_flood_change_u_summary,
           flood,
           ISO,
           crop) {
    ggplot(dB_flood_change_u_summary,
           aes(crop_ISO1, group = crop_ISO1, fill = crop_ISO1)) +
      geom_boxplot(aes(
        ymin = max_min,
        lower = q25,
        middle = median,
        upper = q75,
        ymax = min_max
      ),
      stat = "identity") +
      guides(fill = "none") +
      scale_y_continuous(labels = comma) +
      labs(y = "Impact") +
      labs(x = "Sourcing Area")  +
      labs(title = paste0(
        "Change in Impact of ",
        flood,
        " - ",
        ISO,
        " - ",
        crop,
        " - Upper threshold"
      )) +
      ylim(-1, 1) +
      theme(
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 7
        )
      )
  }

## Climate Risk Profiles
### Prepare Climate Risk Profile Data

# results in dB_profile_summary
dB_profile_summary_make_f <-
  function(rB_impact, v_crop_ISO_lc_rcl_agg) {
    exact_extract(dropLayer(
      rB_impact,
      c(
        1:18,
        23,
        28,
        33,
        42,
        47,
        52,
        57,
        66:84,
        89,
        94,
        99,
        108,
        113,
        118,
        123,
        132,
        137,
        142,
        147,
        152,
        157,
        162,
        171,
        180,
        189,
        198,
        207,
        216,
        221,
        226,
        231,
        236,
        241,
        246,
        255,
        264,
        273,
        282,
        291,
        300
      )
    ),
    v_crop_ISO_lc_rcl_agg,
    fun = c('mean'))
  }

# results in dB_profile_summary_file
dB_profile_summary_file_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dB_profile_summary) %>%
      write_csv(paste0("data/", ISO, "/", crop, "/dB_profile_summary.csv"),
                append = FALSE)
  }

### Plot Climate Risk Profile Data

## historical

## lower thresholds

# results in dB_profilec_d_l_h_l_plot
dB_profilec_d_l_h_l_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 1:4)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.hist.drought.l.opt.heat.l.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.hist.drought.l.opt.heat.l.sub = rgb(255, 255, 153, maxColorValue = 255),
          mean.hist.drought.l.sub.heat.l.opt = rgb(253, 192, 134, maxColorValue = 255),
          mean.hist.drought.l.sub.heat.l.sub = rgb(191, 91, 23, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - drought l heat l") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }



# results in dB_profilec_d_l_f_l_plot
dB_profilec_d_l_f_l_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 5:8)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.hist.drought.l.opt.flood.l.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.hist.drought.l.opt.flood.l.sub = rgb(190, 174, 212, maxColorValue = 255),
          mean.hist.drought.l.sub.flood.l.opt = rgb(253, 192, 134, maxColorValue = 255),
          mean.hist.drought.l.sub.flood.l.sub = rgb(56, 108, 176, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - drought l flood l") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }

# results in dB_profilec_h_l_f_l_plot
dB_profilec_h_l_f_l_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 9:12)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.hist.heat.l.opt.flood.l.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.hist.heat.l.opt.flood.l.sub = rgb(190, 174, 212, maxColorValue = 255),
          mean.hist.heat.l.sub.flood.l.opt = rgb(255, 255, 153, maxColorValue = 255),
          mean.hist.heat.l.sub.flood.l.sub = rgb(102, 102, 102, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - heat l flood l") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }

# results in dB_profilec_d_l_h_l_f_l_plot
dB_profilec_d_l_h_l_f_l_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 13:20)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.hist.drought.l.opt.heat.l.opt.flood.l.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.hist.drought.l.opt.heat.l.sub.flood.l.sub = rgb(102, 102, 102, maxColorValue = 255),
          mean.hist.drought.l.sub.heat.l.opt.flood.l.sub = rgb(56, 108, 176, maxColorValue = 255),
          mean.hist.drought.l.sub.heat.l.sub.flood.l.opt = rgb(191, 91, 23, maxColorValue = 255),
          mean.hist.drought.l.opt.heat.l.opt.flood.l.sub = rgb(190, 174, 212, maxColorValue = 255),
          mean.hist.drought.l.opt.heat.l.sub.flood.l.opt = rgb(255, 255, 153, maxColorValue = 255),
          mean.hist.drought.l.sub.heat.l.opt.flood.l.opt = rgb(253, 192, 134, maxColorValue = 255),
          mean.hist.drought.l.sub.heat.l.sub.flood.l.sub = rgb(240, 2, 127, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - drought l heat l flood l") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }

## upper thresholds

# results in dB_profilec_d_u_h_u_plot
dB_profilec_d_u_h_u_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 41:44)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.hist.drought.u.opt.heat.u.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.hist.drought.u.opt.heat.u.sub = rgb(255, 255, 153, maxColorValue = 255),
          mean.hist.drought.u.sub.heat.u.opt = rgb(253, 192, 134, maxColorValue = 255),
          mean.hist.drought.u.sub.heat.u.sub = rgb(191, 91, 23, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - drought u heat u") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }

# results in dB_profilec_d_u_f_u_plot
dB_profilec_d_u_f_u_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 45:48)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.hist.drought.u.opt.flood.u.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.hist.drought.u.opt.flood.u.sub = rgb(190, 174, 212, maxColorValue = 255),
          mean.hist.drought.u.sub.flood.u.opt = rgb(253, 192, 134, maxColorValue = 255),
          mean.hist.drought.u.sub.flood.u.sub = rgb(56, 108, 176, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - drought u flood u") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }

# results in dB_profilec_h_u_f_u_plot
dB_profilec_h_u_f_u_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 49:52)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.hist.heat.u.opt.flood.u.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.hist.heat.u.opt.flood.u.sub = rgb(190, 174, 212, maxColorValue = 255),
          mean.hist.heat.u.sub.flood.u.opt = rgb(255, 255, 153, maxColorValue = 255),
          mean.hist.heat.u.sub.flood.u.sub = rgb(102, 102, 102, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - heat u flood u") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }

# results in dB_profilec_d_u_h_u_f_u_plot
dB_profilec_d_u_h_u_f_u_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 53:60)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.hist.drought.u.opt.heat.u.opt.flood.u.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.hist.drought.u.opt.heat.u.sub.flood.u.sub = rgb(102, 102, 102, maxColorValue = 255),
          mean.hist.drought.u.sub.heat.u.opt.flood.u.sub = rgb(56, 108, 176, maxColorValue = 255),
          mean.hist.drought.u.sub.heat.u.sub.flood.u.opt = rgb(191, 91, 23, maxColorValue = 255),
          mean.hist.drought.u.opt.heat.u.opt.flood.u.sub = rgb(190, 174, 212, maxColorValue = 255),
          mean.hist.drought.u.opt.heat.u.sub.flood.u.opt = rgb(255, 255, 153, maxColorValue = 255),
          mean.hist.drought.u.sub.heat.u.opt.flood.u.opt = rgb(253, 192, 134, maxColorValue = 255),
          mean.hist.drought.u.sub.heat.u.sub.flood.u.sub = rgb(240, 2, 127, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - drought u heat u flood u") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }

## mixed thresholds

# results in dB_profilec_d_u_h_l_plot
dB_profilec_d_u_h_l_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 85:88)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.hist.drought.u.opt.heat.l.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.hist.drought.u.opt.heat.l.sub = rgb(255, 255, 153, maxColorValue = 255),
          mean.hist.drought.u.sub.heat.l.opt = rgb(253, 192, 134, maxColorValue = 255),
          mean.hist.drought.u.sub.heat.l.sub = rgb(191, 91, 23, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - drought u heat l") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }

# results in dB_profilec_d_l_h_u_plot
dB_profilec_d_l_h_u_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 81:84)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.hist.drought.l.opt.heat.u.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.hist.drought.l.opt.heat.u.sub = rgb(255, 255, 153, maxColorValue = 255),
          mean.hist.drought.l.sub.heat.u.opt = rgb(253, 192, 134, maxColorValue = 255),
          mean.hist.drought.l.sub.heat.u.sub = rgb(191, 91, 23, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - drought l heat u") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }

# results in dB_profilec_d_u_f_l_plot
dB_profilec_d_u_f_l_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 93:96)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.hist.drought.u.opt.flood.l.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.hist.drought.u.opt.flood.l.sub = rgb(190, 174, 212, maxColorValue = 255),
          mean.hist.drought.u.sub.flood.l.opt = rgb(253, 192, 134, maxColorValue = 255),
          mean.hist.drought.u.sub.flood.l.sub = rgb(56, 108, 176, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - drought u flood l") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }

# results in dB_profilec_d_l_f_u_plot
dB_profilec_d_l_f_u_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 89:92)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.hist.drought.l.opt.flood.u.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.hist.drought.l.opt.flood.u.sub = rgb(190, 174, 212, maxColorValue = 255),
          mean.hist.drought.l.sub.flood.u.opt = rgb(253, 192, 134, maxColorValue = 255),
          mean.hist.drought.l.sub.flood.u.sub = rgb(56, 108, 176, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - drought l flood u") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }

# results in dB_profilec_h_u_f_l_plot
dB_profilec_h_u_f_l_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 101:104)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.hist.heat.u.opt.flood.l.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.hist.heat.u.opt.flood.l.sub = rgb(190, 174, 212, maxColorValue = 255),
          mean.hist.heat.u.sub.flood.l.opt = rgb(255, 255, 153, maxColorValue = 255),
          mean.hist.heat.u.sub.flood.l.sub = rgb(102, 102, 102, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - heat u flood l") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }


# results in dB_profilec_h_l_f_u_plot
dB_profilec_h_l_f_u_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 97:100)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.hist.heat.l.opt.flood.u.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.hist.heat.l.opt.flood.u.sub = rgb(190, 174, 212, maxColorValue = 255),
          mean.hist.heat.l.sub.flood.u.opt = rgb(255, 255, 153, maxColorValue = 255),
          mean.hist.heat.l.sub.flood.u.sub = rgb(102, 102, 102, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - heat l flood u") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }


# results in dB_profilec_d_u_h_l_f_l_plot
dB_profilec_d_u_h_l_f_l_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 129:136)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.hist.drought.u.opt.heat.l.opt.flood.l.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.hist.drought.u.opt.heat.l.sub.flood.l.sub = rgb(102, 102, 102, maxColorValue = 255),
          mean.hist.drought.u.sub.heat.l.opt.flood.l.sub = rgb(56, 108, 176, maxColorValue = 255),
          mean.hist.drought.u.sub.heat.l.sub.flood.l.opt = rgb(191, 91, 23, maxColorValue = 255),
          mean.hist.drought.u.opt.heat.l.opt.flood.l.sub = rgb(190, 174, 212, maxColorValue = 255),
          mean.hist.drought.u.opt.heat.l.sub.flood.l.opt = rgb(255, 255, 153, maxColorValue = 255),
          mean.hist.drought.u.sub.heat.l.opt.flood.l.opt = rgb(253, 192, 134, maxColorValue = 255),
          mean.hist.drought.u.sub.heat.l.sub.flood.l.sub = rgb(240, 2, 127, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - drought u heat l flood l") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }

# results in dB_profilec_d_l_h_u_f_l_plot
dB_profilec_d_l_h_u_f_l_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 113:120)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.hist.drought.l.opt.heat.u.opt.flood.l.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.hist.drought.l.opt.heat.u.sub.flood.l.sub = rgb(102, 102, 102, maxColorValue = 255),
          mean.hist.drought.l.sub.heat.u.opt.flood.l.sub = rgb(56, 108, 176, maxColorValue = 255),
          mean.hist.drought.l.sub.heat.u.sub.flood.l.opt = rgb(191, 91, 23, maxColorValue = 255),
          mean.hist.drought.l.opt.heat.u.opt.flood.l.sub = rgb(190, 174, 212, maxColorValue = 255),
          mean.hist.drought.l.opt.heat.u.sub.flood.l.opt = rgb(255, 255, 153, maxColorValue = 255),
          mean.hist.drought.l.sub.heat.u.opt.flood.l.opt = rgb(253, 192, 134, maxColorValue = 255),
          mean.hist.drought.l.sub.heat.u.sub.flood.l.sub = rgb(240, 2, 127, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - drought l heat u flood l") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }

# results in dB_profilec_d_l_h_l_f_u_plot
dB_profilec_d_l_h_l_f_u_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 105:112)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.hist.drought.l.opt.heat.l.opt.flood.u.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.hist.drought.l.opt.heat.l.sub.flood.u.sub = rgb(102, 102, 102, maxColorValue = 255),
          mean.hist.drought.l.sub.heat.l.opt.flood.u.sub = rgb(56, 108, 176, maxColorValue = 255),
          mean.hist.drought.l.sub.heat.l.sub.flood.u.opt = rgb(191, 91, 23, maxColorValue = 255),
          mean.hist.drought.l.opt.heat.l.opt.flood.u.sub = rgb(190, 174, 212, maxColorValue = 255),
          mean.hist.drought.l.opt.heat.l.sub.flood.u.opt = rgb(255, 255, 153, maxColorValue = 255),
          mean.hist.drought.l.sub.heat.l.opt.flood.u.opt = rgb(253, 192, 134, maxColorValue = 255),
          mean.hist.drought.l.sub.heat.l.sub.flood.u.sub = rgb(240, 2, 127, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - drought l heat l flood u") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }

# results in dB_profilec_d_u_h_u_f_l_plot
dB_profilec_d_u_h_u_f_l_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 137:144)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.hist.drought.u.opt.heat.u.opt.flood.l.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.hist.drought.u.opt.heat.u.sub.flood.l.sub = rgb(102, 102, 102, maxColorValue = 255),
          mean.hist.drought.u.sub.heat.u.opt.flood.l.sub = rgb(56, 108, 176, maxColorValue = 255),
          mean.hist.drought.u.sub.heat.u.sub.flood.l.opt = rgb(191, 91, 23, maxColorValue = 255),
          mean.hist.drought.u.opt.heat.u.opt.flood.l.sub = rgb(190, 174, 212, maxColorValue = 255),
          mean.hist.drought.u.opt.heat.u.sub.flood.l.opt = rgb(255, 255, 153, maxColorValue = 255),
          mean.hist.drought.u.sub.heat.u.opt.flood.l.opt = rgb(253, 192, 134, maxColorValue = 255),
          mean.hist.drought.u.sub.heat.u.sub.flood.l.sub = rgb(240, 2, 127, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - drought u heat u flood l") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }
# results in dB_profilec_d_u_h_l_f_u_plot
dB_profilec_d_u_h_l_f_u_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 121:128)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.hist.drought.u.opt.heat.l.opt.flood.u.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.hist.drought.u.opt.heat.l.sub.flood.u.sub = rgb(102, 102, 102, maxColorValue = 255),
          mean.hist.drought.u.sub.heat.l.opt.flood.u.sub = rgb(56, 108, 176, maxColorValue = 255),
          mean.hist.drought.u.sub.heat.l.sub.flood.u.opt = rgb(191, 91, 23, maxColorValue = 255),
          mean.hist.drought.u.opt.heat.l.opt.flood.u.sub = rgb(190, 174, 212, maxColorValue = 255),
          mean.hist.drought.u.opt.heat.l.sub.flood.u.opt = rgb(255, 255, 153, maxColorValue = 255),
          mean.hist.drought.u.sub.heat.l.opt.flood.u.opt = rgb(253, 192, 134, maxColorValue = 255),
          mean.hist.drought.u.sub.heat.l.sub.flood.u.sub = rgb(240, 2, 127, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - drought u heat l flood u") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }
# results in dB_profilec_d_l_h_u_f_u_plot
dB_profilec_d_l_h_u_f_u_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 145:152)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.hist.drought.l.opt.heat.u.opt.flood.u.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.hist.drought.l.opt.heat.u.sub.flood.u.sub = rgb(102, 102, 102, maxColorValue = 255),
          mean.hist.drought.l.sub.heat.u.opt.flood.u.sub = rgb(56, 108, 176, maxColorValue = 255),
          mean.hist.drought.l.sub.heat.u.sub.flood.u.opt = rgb(191, 91, 23, maxColorValue = 255),
          mean.hist.drought.l.opt.heat.u.opt.flood.u.sub = rgb(190, 174, 212, maxColorValue = 255),
          mean.hist.drought.l.opt.heat.u.sub.flood.u.opt = rgb(255, 255, 153, maxColorValue = 255),
          mean.hist.drought.l.sub.heat.u.opt.flood.u.opt = rgb(253, 192, 134, maxColorValue = 255),
          mean.hist.drought.l.sub.heat.u.sub.flood.u.sub = rgb(240, 2, 127, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - drought l heat u flood u") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }


## future

## lower thresholds

# results in dB_profilef_d_l_h_l_plot
dB_profilef_d_l_h_l_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 21:24)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.future.drought.l.opt.heat.l.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.future.drought.l.opt.heat.l.sub = rgb(255, 255, 153, maxColorValue = 255),
          mean.future.drought.l.sub.heat.l.opt = rgb(253, 192, 134, maxColorValue = 255),
          mean.future.drought.l.sub.heat.l.sub = rgb(191, 91, 23, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - future drought l heat l") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }

# results in dB_profilef_d_l_f_l_plot
dB_profilef_d_l_f_l_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 25:28)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.future.drought.l.opt.flood.l.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.future.drought.l.opt.flood.l.sub = rgb(190, 174, 212, maxColorValue = 255),
          mean.future.drought.l.sub.flood.l.opt = rgb(253, 192, 134, maxColorValue = 255),
          mean.future.drought.l.sub.flood.l.sub = rgb(56, 108, 176, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - future drought l flood l") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }

# results in dB_profilef_h_l_f_l_plot
dB_profilef_h_l_f_l_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 29:32)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.future.heat.l.opt.flood.l.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.future.heat.l.opt.flood.l.sub = rgb(190, 174, 212, maxColorValue = 255),
          mean.future.heat.l.sub.flood.l.opt = rgb(255, 255, 153, maxColorValue = 255),
          mean.future.heat.l.sub.flood.l.sub = rgb(102, 102, 102, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - future heat l flood l") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }

# results in dB_profilef_d_l_h_l_f_l_plot
dB_profilef_d_l_h_l_f_l_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 33:40)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.future.drought.l.opt.heat.l.opt.flood.l.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.future.drought.l.opt.heat.l.sub.flood.l.sub = rgb(102, 102, 102, maxColorValue = 255),
          mean.future.drought.l.sub.heat.l.opt.flood.l.sub = rgb(56, 108, 176, maxColorValue = 255),
          mean.future.drought.l.sub.heat.l.sub.flood.l.opt = rgb(191, 91, 23, maxColorValue = 255),
          mean.future.drought.l.opt.heat.l.opt.flood.l.sub = rgb(190, 174, 212, maxColorValue = 255),
          mean.future.drought.l.opt.heat.l.sub.flood.l.opt = rgb(255, 255, 153, maxColorValue = 255),
          mean.future.drought.l.sub.heat.l.opt.flood.l.opt = rgb(253, 192, 134, maxColorValue = 255),
          mean.future.drought.l.sub.heat.l.sub.flood.l.sub = rgb(240, 2, 127, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - future drought l heat l flood l") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }

## upper thresholds

# results in dB_profilef_d_u_h_u_plot
dB_profilef_d_u_h_u_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 61:64)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.future.drought.u.opt.heat.u.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.future.drought.u.opt.heat.u.sub = rgb(255, 255, 153, maxColorValue = 255),
          mean.future.drought.u.sub.heat.u.opt = rgb(253, 192, 134, maxColorValue = 255),
          mean.future.drought.u.sub.heat.u.sub = rgb(191, 91, 23, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - future drought u heat u") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }

# results in dB_profilef_d_u_f_u_plot
dB_profilef_d_u_f_u_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 65:68)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.future.drought.u.opt.flood.u.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.future.drought.u.opt.flood.u.sub = rgb(190, 174, 212, maxColorValue = 255),
          mean.future.drought.u.sub.flood.u.opt = rgb(253, 192, 134, maxColorValue = 255),
          mean.future.drought.u.sub.flood.u.sub = rgb(56, 108, 176, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - future drought u flood u") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }

# results in dB_profilef_h_u_f_u_plot
dB_profilef_h_u_f_u_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 69:72)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.future.heat.u.opt.flood.u.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.future.heat.u.opt.flood.u.sub = rgb(190, 174, 212, maxColorValue = 255),
          mean.future.heat.u.sub.flood.u.opt = rgb(255, 255, 153, maxColorValue = 255),
          mean.future.heat.u.sub.flood.u.sub = rgb(102, 102, 102, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - future heat u flood u") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }

# results in dB_profilef_d_u_h_u_f_u_plot
dB_profilef_d_u_h_u_f_u_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 73:80)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.future.drought.u.opt.heat.u.opt.flood.u.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.future.drought.u.opt.heat.u.sub.flood.u.sub = rgb(102, 102, 102, maxColorValue = 255),
          mean.future.drought.u.sub.heat.u.opt.flood.u.sub = rgb(56, 108, 176, maxColorValue = 255),
          mean.future.drought.u.sub.heat.u.sub.flood.u.opt = rgb(191, 91, 23, maxColorValue = 255),
          mean.future.drought.u.opt.heat.u.opt.flood.u.sub = rgb(190, 174, 212, maxColorValue = 255),
          mean.future.drought.u.opt.heat.u.sub.flood.u.opt = rgb(255, 255, 153, maxColorValue = 255),
          mean.future.drought.u.sub.heat.u.opt.flood.u.opt = rgb(253, 192, 134, maxColorValue = 255),
          mean.future.drought.u.sub.heat.u.sub.flood.u.sub = rgb(240, 2, 127, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - future drought u heat u flood u") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }

## mixed thresholds

# results in dB_profilef_d_u_h_l_plot
dB_profilef_d_u_h_l_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 157:160)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.future.drought.u.opt.heat.l.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.future.drought.u.opt.heat.l.sub = rgb(255, 255, 153, maxColorValue = 255),
          mean.future.drought.u.sub.heat.l.opt = rgb(253, 192, 134, maxColorValue = 255),
          mean.future.drought.u.sub.heat.l.sub = rgb(191, 91, 23, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - future drought u heat l") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }

# results in dB_profilef_d_l_h_u_plot
dB_profilef_d_l_h_u_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 153:156)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.future.drought.l.opt.heat.u.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.future.drought.l.opt.heat.u.sub = rgb(255, 255, 153, maxColorValue = 255),
          mean.future.drought.l.sub.heat.u.opt = rgb(253, 192, 134, maxColorValue = 255),
          mean.future.drought.l.sub.heat.u.sub = rgb(191, 91, 23, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - future drought l heat u") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }

# results in dB_profilef_d_u_f_l_plot
dB_profilef_d_u_f_l_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 165:168)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.future.drought.u.opt.flood.l.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.future.drought.u.opt.flood.l.sub = rgb(190, 174, 212, maxColorValue = 255),
          mean.future.drought.u.sub.flood.l.opt = rgb(253, 192, 134, maxColorValue = 255),
          mean.future.drought.u.sub.flood.l.sub = rgb(56, 108, 176, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - future drought u flood l") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }

# results in dB_profilef_d_l_f_u_plot
dB_profilef_d_l_f_u_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 161:164)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.future.drought.l.opt.flood.u.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.future.drought.l.opt.flood.u.sub = rgb(190, 174, 212, maxColorValue = 255),
          mean.future.drought.l.sub.flood.u.opt = rgb(253, 192, 134, maxColorValue = 255),
          mean.future.drought.l.sub.flood.u.sub = rgb(56, 108, 176, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - future drought l flood u") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }

# results in dB_profilef_h_u_f_l_plot
dB_profilef_h_u_f_l_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 173:176)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.future.heat.u.opt.flood.l.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.future.heat.u.opt.flood.l.sub = rgb(190, 174, 212, maxColorValue = 255),
          mean.future.heat.u.sub.flood.l.opt = rgb(255, 255, 153, maxColorValue = 255),
          mean.future.heat.u.sub.flood.l.sub = rgb(102, 102, 102, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - future heat u flood l") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }


# results in dB_profilef_h_l_f_u_plot
dB_profilef_h_l_f_u_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 169:172)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.future.heat.l.opt.flood.u.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.future.heat.l.opt.flood.u.sub = rgb(190, 174, 212, maxColorValue = 255),
          mean.future.heat.l.sub.flood.u.opt = rgb(255, 255, 153, maxColorValue = 255),
          mean.future.heat.l.sub.flood.u.sub = rgb(102, 102, 102, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - future heat l flood u") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }


# results in dB_profilef_d_u_h_l_f_l_plot
dB_profilef_d_u_h_l_f_l_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 201:208)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.future.drought.u.opt.heat.l.opt.flood.l.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.future.drought.u.opt.heat.l.sub.flood.l.sub = rgb(102, 102, 102, maxColorValue = 255),
          mean.future.drought.u.sub.heat.l.opt.flood.l.sub = rgb(56, 108, 176, maxColorValue = 255),
          mean.future.drought.u.sub.heat.l.sub.flood.l.opt = rgb(191, 91, 23, maxColorValue = 255),
          mean.future.drought.u.opt.heat.l.opt.flood.l.sub = rgb(190, 174, 212, maxColorValue = 255),
          mean.future.drought.u.opt.heat.l.sub.flood.l.opt = rgb(255, 255, 153, maxColorValue = 255),
          mean.future.drought.u.sub.heat.l.opt.flood.l.opt = rgb(253, 192, 134, maxColorValue = 255),
          mean.future.drought.u.sub.heat.l.sub.flood.l.sub = rgb(240, 2, 127, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - future drought u heat l flood l") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }

# results in dB_profilef_d_l_h_u_f_l_plot
dB_profilef_d_l_h_u_f_l_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 185:192)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.future.drought.l.opt.heat.u.opt.flood.l.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.future.drought.l.opt.heat.u.sub.flood.l.sub = rgb(102, 102, 102, maxColorValue = 255),
          mean.future.drought.l.sub.heat.u.opt.flood.l.sub = rgb(56, 108, 176, maxColorValue = 255),
          mean.future.drought.l.sub.heat.u.sub.flood.l.opt = rgb(191, 91, 23, maxColorValue = 255),
          mean.future.drought.l.opt.heat.u.opt.flood.l.sub = rgb(190, 174, 212, maxColorValue = 255),
          mean.future.drought.l.opt.heat.u.sub.flood.l.opt = rgb(255, 255, 153, maxColorValue = 255),
          mean.future.drought.l.sub.heat.u.opt.flood.l.opt = rgb(253, 192, 134, maxColorValue = 255),
          mean.future.drought.l.sub.heat.u.sub.flood.l.sub = rgb(240, 2, 127, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - future drought l heat u flood l") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }

# results in dB_profilef_d_l_h_l_f_u_plot
dB_profilef_d_l_h_l_f_u_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 177:184)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.future.drought.l.opt.heat.l.opt.flood.u.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.future.drought.l.opt.heat.l.sub.flood.u.sub = rgb(102, 102, 102, maxColorValue = 255),
          mean.future.drought.l.sub.heat.l.opt.flood.u.sub = rgb(56, 108, 176, maxColorValue = 255),
          mean.future.drought.l.sub.heat.l.sub.flood.u.opt = rgb(191, 91, 23, maxColorValue = 255),
          mean.future.drought.l.opt.heat.l.opt.flood.u.sub = rgb(190, 174, 212, maxColorValue = 255),
          mean.future.drought.l.opt.heat.l.sub.flood.u.opt = rgb(255, 255, 153, maxColorValue = 255),
          mean.future.drought.l.sub.heat.l.opt.flood.u.opt = rgb(253, 192, 134, maxColorValue = 255),
          mean.future.drought.l.sub.heat.l.sub.flood.u.sub = rgb(240, 2, 127, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - future drought l heat l flood u") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }

# results in dB_profilef_d_u_h_u_f_l_plot
dB_profilef_d_u_h_u_f_l_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 209:216)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.future.drought.u.opt.heat.u.opt.flood.l.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.future.drought.u.opt.heat.u.sub.flood.l.sub = rgb(102, 102, 102, maxColorValue = 255),
          mean.future.drought.u.sub.heat.u.opt.flood.l.sub = rgb(56, 108, 176, maxColorValue = 255),
          mean.future.drought.u.sub.heat.u.sub.flood.l.opt = rgb(191, 91, 23, maxColorValue = 255),
          mean.future.drought.u.opt.heat.u.opt.flood.l.sub = rgb(190, 174, 212, maxColorValue = 255),
          mean.future.drought.u.opt.heat.u.sub.flood.l.opt = rgb(255, 255, 153, maxColorValue = 255),
          mean.future.drought.u.sub.heat.u.opt.flood.l.opt = rgb(253, 192, 134, maxColorValue = 255),
          mean.future.drought.u.sub.heat.u.sub.flood.l.sub = rgb(240, 2, 127, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - future drought u heat u flood l") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }
# results in dB_profilef_d_u_h_l_f_u_plot
dB_profilef_d_u_h_l_f_u_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 193:200)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.future.drought.u.opt.heat.l.opt.flood.u.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.future.drought.u.opt.heat.l.sub.flood.u.sub = rgb(102, 102, 102, maxColorValue = 255),
          mean.future.drought.u.sub.heat.l.opt.flood.u.sub = rgb(56, 108, 176, maxColorValue = 255),
          mean.future.drought.u.sub.heat.l.sub.flood.u.opt = rgb(191, 91, 23, maxColorValue = 255),
          mean.future.drought.u.opt.heat.l.opt.flood.u.sub = rgb(190, 174, 212, maxColorValue = 255),
          mean.future.drought.u.opt.heat.l.sub.flood.u.opt = rgb(255, 255, 153, maxColorValue = 255),
          mean.future.drought.u.sub.heat.l.opt.flood.u.opt = rgb(253, 192, 134, maxColorValue = 255),
          mean.future.drought.u.sub.heat.l.sub.flood.u.sub = rgb(240, 2, 127, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - future drought u heat l flood u") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }
# results in dB_profilef_d_l_h_u_f_u_plot
dB_profilef_d_l_h_u_f_u_plot_make_f <-
  function(v_crop_ISO_lc_rcl_agg,
           dB_profile_summary,
           ISO,
           crop) {
    tibble(v_crop_ISO_lc_rcl_agg$crop_ISO1) %>%
      `names<-`(c("Landuse")) %>%
      cbind(dplyr::select(dB_profile_summary, 217:224)) %>%
      reshape2::melt(value.name = "limits",
                     na.rm = FALSE,
                     id.vars = "Landuse") %>%
      
      ggplot(aes(x = Landuse,
                 y = limits,
                 fill = variable)) +
      geom_bar(position = "fill",
               stat = "identity",
               show.legend = T) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(
        values = c(
          mean.future.drought.l.opt.heat.u.opt.flood.u.opt = rgb(127, 201, 127, maxColorValue = 255),
          mean.future.drought.l.opt.heat.u.sub.flood.u.sub = rgb(102, 102, 102, maxColorValue = 255),
          mean.future.drought.l.sub.heat.u.opt.flood.u.sub = rgb(56, 108, 176, maxColorValue = 255),
          mean.future.drought.l.sub.heat.u.sub.flood.u.opt = rgb(191, 91, 23, maxColorValue = 255),
          mean.future.drought.l.opt.heat.u.opt.flood.u.sub = rgb(190, 174, 212, maxColorValue = 255),
          mean.future.drought.l.opt.heat.u.sub.flood.u.opt = rgb(255, 255, 153, maxColorValue = 255),
          mean.future.drought.l.sub.heat.u.opt.flood.u.opt = rgb(253, 192, 134, maxColorValue = 255),
          mean.future.drought.l.sub.heat.u.sub.flood.u.sub = rgb(240, 2, 127, maxColorValue = 255)
        )
      ) +
      labs(fill = "") +
      labs(y = "Risk Types") +
      labs(x = "Sourcing Area")  +
      labs(title = "Climate Risk Profile per Crop Sourcing Area - future drought l heat u flood u") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          size = 7
        )
      )
  }

# sankey diagram
