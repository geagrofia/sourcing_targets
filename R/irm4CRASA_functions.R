# temporary function to load raster data

# where: data_file_prefix is the file prefix showing the theme
#        raster_name is the name to be given to the raster layer or brick
#        raster_or_brick has two possible values, either raster for a single layer or brick for multiple layers


load_raster_data <-
  function(data_file_prefix,
           raster_name,
           rast_or_brick) {
    raster_filename <-
      as.character(paste("data/input/",
                         data_file_prefix,
                         ".tif",
                         sep = ""))
    raster_name <-  rast_or_brick(here(raster_filename))
    return(raster_name)
  }

ISO_get_f <- 
function(cc_data, cc_row) {
  paste(cc_data[cc_row, 2])
}

crop_get_f <- 
function(cc_data, cc_row) {
  paste(cc_data[cc_row, 3])
}

# temporary function for projecting

# where: x is the original raster data
#        y is the crs of the projected raster
#        z is the method used in the project function

#raster_project <- function(x, y, z) {
#  raster_proj <- projectRaster(
#    from = x,
#    crs = y,
#    method = z)
#  return(raster_proj)
#  gc()
#}

# Alternative for SpatRasters using terra - 
# this is far quicker than the raster method and in principle only requires two inputs:
# the SpatRaster to be projected (x) and a template SpatRaster 
# the resolution and crs from the template are used

# optional arguments are the method (y) bilinear or ngb, mask

raster_project <- function(x, y) {
  raster_proj <- terra::project(
    x,
    rast_mask_proj,
    y)
  return(raster_proj)
  gc()
}

# temporary function for aggregating and resampling

# where: w is the original raster data
#        x is the calculated aggregation value
#        y is the method used in the aggregate function
#        z is the method used in the resample function

agg_resample <- function(w, x, y, z) {
  mask_x <- aggregate(w,
                      fact = x,
                      fun = y,
                      expand = TRUE) %>%
    terra::resample(rast_mask_proj, method = z) %>%
    terra::mask(rast_mask_proj)
  return(mask_x)
  gc()
}

# temporary function for determining growth periods which do not fit in a calendar year
# based on the .growth_period function in the irm package

# where: 

.growth_period_long <- function (day_begin, day_end, num_years) 
{
  days <- 1:365
  day_months <- days %>% as.character %>% as.Date("%j") %>% 
    format("%m") %>% as.integer 
  months <- rep.int(day_months, num_years)
  days <- round(day_begin):round(day_end)
  tabulate(bin = months[days], nbins = 12L)* num_years/tabulate(bin = months, 
                                                                nbins = 12L)
}


# https://github.com/tidyverse/lubridate/issues/617

#####################################################################################
#' Format date as yearly or monthly dekad (10-day period)
#'
#' @param x date to convert to dekad
#' @param type dekad of month (1:3) or year (1:36)
#' @inheritDotParams base::as.Date
#' @return integer dekad
#' @importFrom lubridate day
#' @examples
#' dekad(Sys.Date())
#' @export
#' 

dekad <- function(x, type = c("month", "year"), ...) {
  type <- match.arg(type)
  x <- as.Date(x, ...)
  res <- ifelse(day(x) > 20,  3, ifelse(day(x) > 10, 2, 1))
  if(type == "year") res <- month(x)*3 + res - 3
  return(res)
}



# temporary function for determining growth periods which do not fit in a calendar year
# based on the .growth_period function in the irm package

# where: 

.growth_period_long_dekad <- function (day_begin, day_end, num_years) 
{
  days <- 1:365
  day_dekads <- days %>% as.character %>% as.Date("%j") %>% 
    dekad(type = "year") %>% as.integer 
  dekads <- rep.int(day_dekads, num_years)
  days <- round(day_begin):round(day_end)
  tabulate(bin = dekads[days], nbins = 36L)* num_years/tabulate(bin = dekads, 
                                                                nbins = 36L)
}

# developed in 'sowing_date_test_4_CRASA.rmd'
# takes a data frame with fields for day_begin and day_end
# num_years is predefined
# produces a matrix of proportion of each month in growing period for each cell
# afterwards needs to be transposed and binded to original data frame, 
# converted to sf point object and rasterized

.growth_period_long_tbl <- function(x, day_begin, day_end, num_years) {
  
  days <- 1:365
  day_months <- days %>% as.character %>% as.Date("%j") %>% 
    format("%m") %>% as.integer
  months <- rep.int(day_months, num_years)
  days <- round(x[day_begin]):round(x[day_end])
  tabulate(bin = months[days], nbins = 12L)* num_years/tabulate(bin = months, 
                                                     nbins = 12L)
  
}



# developed in 'sowing_date_test_4_CRASA.rmd'
# takes a data frame with fields for day_begin and day_end
# num_years is predefined
# produces a matrix of proportion of each dekad in growing period for each cell
# afterwards needs to be transposed and binded to original data frame, 
# converted to sf point object and rasterized


.growth_period_long_dekad_tbl <- function (x, day_begin, day_end, num_years) {
  days <- 1:365
  day_dekads <- days %>% as.character %>% as.Date("%j") %>% 
    dekad(type = "year") %>% as.integer 
  dekads <- rep.int(day_dekads, num_years)
  days <- round(x[day_begin]):round(x[day_end])
  tabulate(bin = dekads[days], nbins = 36L)* num_years/tabulate(bin = dekads, 
                                                                nbins = 36L)
}


# temporary function for rasterizing and plotting fuzzy partition matrices

# where: n is the number of conclusions from the fuzzy partition matrix
#        fpm_conc_var is a list of the variables assigned to each conclusion
#        fpm_conc_name is a list of the names used for the layers of the raster brick and which will be seen in the title of each plot
#        fpm_plot_title is the title of the plot
#        df_data is the name of the data frame or tibble used to store the data



rasterize_plot_fpm <- function(n, fpm_conc_var, fpm_conc_name, df_data) {
  d0 <- df_data %>%
    dplyr::select(unlist(as.character(noquote(fpm_conc_var)))) %>%
    na.omit %>%
    st_as_sf(coords = c("x", "y"))
  
  # initialise the dB rasterbrick using the first layer
  
  dB <- rasterize(d0, rast_mask_proj, fpm_conc_var[[1]]) #%>% brick   # SpatRasters don't need to be bricks
  
  # in a loop add subsequent layers to the dB SpatRaster starting from the second layer 
  
  for (i in 2:n) {
    dB <-
      rast(list(dB, rasterize(d0, rast_mask_proj, field = fpm_conc_var[[i]])))
  }
  
  # give the dB layers sensible names
  
  names(dB) <- fpm_conc_name
  
  # plot dB using title
  
  dB %>% plot(main = fpm_plot_title)
}

# temporary function to write output to geojson

# where: x is the projected sf data
#        data_file_prefix is the prefix used to define the name of the output geojson

output_sf <- function(results_sf, data_file_prefix) {
  vector_filename <-
    as.character(paste("data/output/",
                       data_file_prefix,
                       ".geojson",
                       sep = ""))
  
  st_write(
    st_transform(results_sf,
                 crs = 4326),
    dsn = (here(vector_filename)),
    delete_layer = TRUE,
    delete_dsn = TRUE
  )
  
}

# temporary function to write output to geotiff

# where: results_raster is the projected raster data
#        data_file_prefix is the prefix used to define the name of the output tif

output_geotiff <- function(results_raster, data_file_prefix) {
  geotiff_filename <-
    as.character(paste("data/output/",
                       data_file_prefix,
                       ".tif",
                       sep = ""))
  
  writeRaster(
    results_raster,
    here(geotiff_filename),
    overwrite = TRUE,
    filetype = "GTiff"
  )
  
}
