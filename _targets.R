# _targets.R file

library(targets)
source("R/functions.R")
options(tidyverse.quiet = TRUE)
tar_option_set(
  packages = c(
    "biglm",
    "tidyverse",
    "rnaturalearth",
    "sf",
    "raster",
    "terra",
    "rasterVis",
    "exactextractr",
    "scales"
  )
)
list(
  tar_target(world, ne_countries(
    scale = "medium", returnclass = "sf"
  )),
  tar_target(world_plot, world_plot_f(world)),
  
  tar_target(cc_data_path,
             "data/cc_data.csv",
             format = "file"),
  tar_target(cc_data,
             read_csv(cc_data_path, col_types = cols())),
  tar_target(ISO, ISO_get_f(cc_data)),
  tar_target(crop, crop_get_f(cc_data)),
  
  tar_target(v_crop, v_crop_get_f(cc_data)),
  tar_target(v_crop_extent, v_crop_extent_f(v_crop)),
  tar_target(
    v_crop_plot,
    v_crop_plot_f(v_crop, v_crop_extent, ISO, crop, world)
  ),
  
  tar_target(v_ISO, v_ISO_get_f(ISO)),
  tar_target(v_ISO_extent, v_ISO_extent_f(v_ISO)),
  tar_target(v_ISO_plot, v_ISO_plot_f(v_ISO, ISO)),
  
  tar_target(r_ind1, r_ind1_get_f(cc_data, ISO, crop)),
  tar_target(r_ind1_plot, r_ind1_plot_f(r_ind1, v_ISO, v_ISO_extent, world, ISO, crop)),
  
  tar_target(r_lc_file, r_lc_get_write_f(r_ind1, ISO)),
  tar_target(r_lc, r_lc_get_f(ISO, r_lc_file)),
  
  tar_target(r_ISO_file, r_ISO_make_write_f(v_ISO, r_lc, ISO)),
  tar_target(r_ISO, r_ISO_get_f(ISO, r_ISO_file)),
  
  tar_target(r_lc_ISO_file, r_lc_ISO_make_write_f(r_ISO, r_lc, ISO)),
  tar_target(r_lc_ISO, r_lc_ISO_get_f(ISO, r_lc_ISO_file)),
  tar_target(r_lc_ISO_plot, r_lc_ISO_plot_f(r_lc_ISO, v_ISO, ISO)),
  
  tar_target(r_crop_file, r_crop_make_write_f(v_crop, r_lc_ISO, ISO, crop)),
  tar_target(r_crop, r_crop_get_f(r_crop_file, ISO, crop)),
  tar_target(r_crop_plot, r_crop_plot_f(r_crop, v_ISO, ISO, crop)),
  
  tar_target(
    r_crop_ISO_file,
    r_crop_ISO_make_write_f(r_crop, r_ISO, ISO, crop)
  ),
  tar_target(r_crop_ISO, r_crop_ISO_get_f(r_crop_ISO_file, ISO, crop)),
  tar_target(
    r_crop_ISO_plot,
    r_crop_ISO_plot_f(r_crop_ISO, v_ISO, ISO, crop)
  ),
  
  tar_target(
    r_crop_ISO_lc_file,
    r_crop_ISO_lc_make_write_f(r_lc_ISO, r_crop_ISO, ISO, crop)
  ),
  tar_target(
    r_crop_ISO_lc,
    r_crop_ISO_lc_get_f(r_crop_ISO_lc_file, ISO, crop)
  ),
  tar_target(
    r_crop_ISO_lc_plot,
    r_crop_ISO_lc_plot_f(r_crop_ISO_lc, v_ISO, ISO, crop)
  ),
  
  tar_target(
    r_crop_ISO_lc_rcl_file,
    r_crop_ISO_lc_rcl_make_write_f(r_crop_ISO_lc, ISO, crop)
  ),
  tar_target(
    r_crop_ISO_lc_rcl,
    r_crop_ISO_lc_rcl_get_f(r_crop_ISO_lc_rcl_file, ISO, crop)
  ),
  tar_target(
    r_crop_ISO_lc_rcl_plot,
    r_crop_ISO_lc_rcl_plot_f(r_crop_ISO_lc_rcl, v_ISO, ISO, crop)
  ),
  
  tar_target(
    r_crop_ISO_lc_rcl_agg_file,
    r_crop_ISO_lc_rcl_agg_make_write_f(r_crop_ISO_lc_rcl, ISO, crop)
  ),
  tar_target(
    r_crop_ISO_lc_rcl_agg,
    r_crop_ISO_lc_rcl_agg_get_f(r_crop_ISO_lc_rcl_agg_file, ISO, crop)
  ),
  
  tar_target(
    v_crop_ISO_lc_rcl_agg_file,
    v_crop_ISO_lc_rcl_agg_make_write_f(r_crop_ISO_lc_rcl_agg, ISO, crop)
  ),
  tar_target(
    v_crop_ISO_lc_rcl_agg,
    v_crop_ISO_lc_rcl_agg_get_f(v_crop_ISO_lc_rcl_agg_file, ISO, crop)
  ),
  
  tar_target(
    dB_ind1_summary,
    dB_ind1_summary_make(r_ind1, v_crop_ISO_lc_rcl_agg)
  ),
  tar_target(
    dB_sourcing_stats,
    dB_sourcing_stats_make(v_crop_ISO_lc_rcl_agg, dB_ind1_summary)
  ),
  tar_target(
    dB_sourcing_stats_plot,
    dB_sourcing_stats_plot_f(dB_sourcing_stats, ISO, crop)
  )
  
  
)


