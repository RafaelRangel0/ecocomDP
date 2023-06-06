library(ecocomDP)
library(tidyverse)
library(ggplot2)
library(devtools)
library(terra)
library(rnaturalearth)
library(maps)
library(geodata)

library(devtools)

rm(list = ls())

load_all()


#deprecating packages: maptools, rgdal,  (retiring October 2023)
#In the provided code, the only method from the rgdal package is not explicitly called. However, the package is listed as a suggestion in the suggs vector, which is used for checking if required packages are installed. If the rgdal package is not installed, it will be mentioned in the error message. The code does not directly utilize any specific functions from the rgdal package.
#In the provided code, there are no functions from the maptools package explicitly called or used. Although the maptools package is included in the suggs vector as a suggestion for required packages, the code does not utilize any specific functions from maptools.


#load in data (usually passed through data param)
data2 = my_result_read_data <- read_data(
  id = "neon.ecocomdp.20120.001.001",
  site= c('COMO','LECO','SUGG'),
  startdate = "2017-06",
  enddate = "2021-03",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)


data <- read_data(
  id = "neon.ecocomdp.10092.001.001",
  site = c("ORNL","OSBS"),
  startdate = "2016-01",
  enddate = "2017-11",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

data_stored = data

data = data_stored

id = NA_character_
alpha = 1
labels = TRUE
  # TODO Rename function using ecocomDP term (e.g. plot_locations())?
  # TODO validate labels
  # Validate inputs

  #What exactly does this do?
  validate_arguments(fun.name = "plot", fun.args = as.list(environment()))

  # check for suggested packages that are required for this function to work
  suggs <- c("ggrepel", "usmap")
  suggsmissing <- !unlist(lapply(suggs, requireNamespace, quietly = TRUE))
  if (any(suggsmissing)) {
    stop("Packages ", paste(suggs, collapse = ", "), " are required for ",
         "running plot_sites(). Packages ",
         paste(suggs[suggsmissing], collapse = ", "), " are not installed.",
         call. = FALSE)
  }

  # TODO: Convert this unreadable block of code to a function like get_observation_table()
  # TODO: Call get_id()
  # required col names in flat data
  req_col_names <- c("observation_id","event_id","package_id","location_id",
                     "datetime","taxon_id","variable_name","value",
                     "location_id","location_name","longitude","latitude")

  # detect data type, extract observation table
  data_type <- detect_data_type(data)
  if(data_type == "table" && all(req_col_names %in% names(data))){
    flat_data <- data %>% dplyr::distinct()
    if(is.na(id)) id <- paste(unique(data$package_id), collapse = " | ")
  }else if(data_type == "table" && !all(req_col_names %in% names(data))){
    stop("please provide a valid ecocomDP dataset or a table that includes the columns present in the ecocomDP 'observation' and 'taxon' tables")
  }else if(data_type == "dataset_old"){
    flat_data <- flatten_data(data)
    if(is.na(id)) id <- names(data)
  }else if(data_type == "dataset"){
    flat_data <- flatten_data(data)
    if(is.na(id)) id <- data$id
  }else{
    stop("No plotting method currently implemented for this data format")
  }





  #This part causes warning messages -------------------------------------------------------
  cleaned <- flat_data %>%
    dplyr::select(
      "longitude",
      "latitude",
      "location_name",
      "package_id") %>%
    dplyr::distinct()


  # 1: Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
  # ℹ Please use `"longitude"` instead of `.data$longitude`
  # This warning is displayed once every 8 hours.
  # Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
  # 2: Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
  # ℹ Please use `"latitude"` instead of `.data$latitude`
  # This warning is displayed once every 8 hours.
  # Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
  # 3: Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
  # ℹ Please use `"location_name"` instead of `.data$location_name`
  # This warning is displayed once every 8 hours.
  # Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
  # 4: Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
  # ℹ Please use `"package_id"` instead of `.data$package_id`
  # This warning is displayed once every 8 hours.
  # Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.


  #------------------------------------------------------------------------------------------










  # Load the map data
  #us_map <- rast(system.file("ex/elev.tif", package="terra"))

  # Transform the coordinates
  transformed_cleaned <- suppressWarnings(usmap::usmap_transform(cleaned,
                                                                 input_names = c("longitude", "latitude")))

  #transformed_cleaned <- transformed_cleaned %>% as.data.frame()

  #----------------------------------------------------------------------------------------------------------------------------------

  # Plot the map (terra first attempt)
  # ggplot() +
  #   geom_raster(data = as.data.frame(us_map), aes(x = V1, y = V2, fill = value), interpolate = TRUE) +
  #   geom_point(data = transformed_cleaned, aes(x = V1, y = V2), color = "red", size = 3, alpha = alpha) #+
    # geom_text_repel(data = transformed_cleaned, aes(x = X1, y = X2, label = cleaned$location_name), size = 3, max.overlaps = Inf) +
    # xlab("Longitude") +
    # ylab("Latitude") +
    # labs(title = "Map of sites", subtitle = id) +
    # theme(legend.position = "none")
    #

  #----------------------------------------------------------------------------------------------------------------------------------





  # us <- gadm(country = "USA", level = 1, resolution = 2, path = getwd())
  # plot(us, lwd = 2, xlim = c(-125,-65), ylim = c(25,50))
  # points(c(-100), c(40), pch = 16,
  #        col = alpha("green", 0.2))


  #-----------------------------------------------------------------------------------------------------------------------------------


  #MY (potentially) working version wiithout deprecated packages

  us_map <- map_data("state")

  ggplot() +
    geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "grey") +
    geom_point(data = cleaned, aes(x = longitude, y = latitude, color = location_name), size = 3) +
    coord_map() +
    labs(x = "Longitude", y = "Latitude") +
    ggtitle("US Map with Coordinates") +
    theme_bw() +
    ggrepel::geom_text_repel(
      data = cleaned,
      aes(x = longitude, y = latitude, label = location_name),
      size = 3, max.overlaps = Inf) +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab(paste0("Latitude")) +
    ggplot2::labs(title = "Map of sites", subtitle = id) +
    ggplot2::theme(legend.position = "none")



  #working version (using map tools)
  # transformed_cleaned <- usmap::usmap_transform(cleaned, input_names = c("longitude", "latitude"))
  #
  #
  #
  usmap::plot_usmap(color = "grey") +
    ggplot2::geom_point(
      data = transformed_cleaned,
      ggplot2::aes(x = .data$x, y = .data$y, size = 20),
      color = "red", alpha = alpha) +
    ggrepel::geom_text_repel(
      data = transformed_cleaned,
      aes(x = .data$x, y = .data$y, label = .data$location_name),
      size = 3, max.overlaps = Inf) +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab(paste0("Latitude")) +
    ggplot2::labs(title = "Map of sites", subtitle = id) +
    ggplot2::theme(legend.position = "none")



#--------------------------------------------------------------------------------------------------------------------------------------------------------

































#Test Function --------------------------------------------------------------------------------------------------------


plot_sites_original <- function(data,
                        id = NA_character_,
                        alpha = 1,
                        labels = TRUE){validate_arguments(fun.name = "plot", fun.args = as.list(environment()))
  suggs <- c("ggrepel", "usmap", "maptools", "rgdal")
  suggsmissing <- !unlist(lapply(suggs, requireNamespace, quietly = TRUE))
  if (any(suggsmissing)) {
    stop("Packages ", paste(suggs, collapse = ", "), " are required for ",
         "running plot_sites(). Packages ", paste(suggs[suggsmissing],
                                                  collapse = ", "), " are not installed.", call. = FALSE)
  }
  req_col_names <- c("observation_id", "event_id", "package_id",
                     "location_id", "datetime", "taxon_id", "variable_name",
                     "value", "location_id", "location_name", "longitude",
                     "latitude")
  data_type <- detect_data_type(data)
  if (data_type == "table" && all(req_col_names %in% names(data))) {
    flat_data <- data %>% dplyr::distinct()
    if (is.na(id))
      id <- paste(unique(data$package_id), collapse = " | ")
  }
  else if (data_type == "table" && !all(req_col_names %in%
                                        names(data))) {
    stop("please provide a valid ecocomDP dataset or a table that includes the columns present in the ecocomDP 'observation' and 'taxon' tables")
  }
  else if (data_type == "dataset_old") {
    flat_data <- flatten_data(data)
    if (is.na(id))
      id <- names(data)
  }
  else if (data_type == "dataset") {
    flat_data <- flatten_data(data)
    if (is.na(id))
      id <- data$id
  }
  else {
    stop("No plotting method currently implemented for this data format")
  }
  cleaned <- flat_data %>% dplyr::select(.data$longitude, .data$latitude,
                                         .data$location_name, .data$package_id) %>% dplyr::distinct()
  transformed_cleaned <- suppressWarnings(usmap::usmap_transform(cleaned,
                                                                 input_names = c("longitude", "latitude")))
  usmap::plot_usmap(color = "grey") + ggplot2::geom_point(data = transformed_cleaned,
                                                          ggplot2::aes(x = .data$x, y = .data$y, size = 20), color = "red",
                                                          alpha = alpha) + ggrepel::geom_text_repel(data = transformed_cleaned,
                                                                                                    aes(x = .data$x, y = .data$y, label = .data$location_name),
                                                                                                    size = 3, max.overlaps = Inf) + ggplot2::xlab("Longitude") +
    ggplot2::ylab(paste0("Latitude")) + ggplot2::labs(title = "Map of sites",
                                                      subtitle = id) + ggplot2::theme(legend.position = "none")

}
plot_sites_original(data)




my_plot_sites <- function(data,
              id = NA_character_,
              alpha = 1,
              labels = TRUE){
  validate_arguments(fun.name = "plot", fun.args = as.list(environment()))

  # check for suggested packages that are required for this function to work
  suggs <- c("ggrepel", "usmap")
  suggsmissing <- !unlist(lapply(suggs, requireNamespace, quietly = TRUE))
  if (any(suggsmissing)) {
    stop("Packages ", paste(suggs, collapse = ", "), " are required for ",
         "running plot_sites(). Packages ",
         paste(suggs[suggsmissing], collapse = ", "), " are not installed.",
         call. = FALSE)
  }

  # TODO: Convert this unreadable block of code to a function like get_observation_table()
  # TODO: Call get_id()
  # required col names in flat data
  req_col_names <- c("observation_id","event_id","package_id","location_id",
                     "datetime","taxon_id","variable_name","value",
                     "location_id","location_name","longitude","latitude")

  # detect data type, extract observation table
  data_type <- detect_data_type(data)
  if(data_type == "table" && all(req_col_names %in% names(data))){
    flat_data <- data %>% dplyr::distinct()
    if(is.na(id)) id <- paste(unique(data$package_id), collapse = " | ")
  }else if(data_type == "table" && !all(req_col_names %in% names(data))){
    stop("please provide a valid ecocomDP dataset or a table that includes the columns present in the ecocomDP 'observation' and 'taxon' tables")
  }else if(data_type == "dataset_old"){
    flat_data <- flatten_data(data)
    if(is.na(id)) id <- names(data)
  }else if(data_type == "dataset"){
    flat_data <- flatten_data(data)
    if(is.na(id)) id <- data$id
  }else{
    stop("No plotting method currently implemented for this data format")
  }


  #This part causes warning messages -------------------------------------------------------
  cleaned <- flat_data %>%
    dplyr::select(
      "longitude",
      "latitude",
      "location_name",
      "package_id") %>%
    dplyr::distinct()


  us_map <- map_data("state")

  ggplot() +
    geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "grey") +
    geom_point(data = cleaned, aes(x = longitude, y = latitude, color = location_name), size = 3) +
    coord_map() +
    labs(x = "Longitude", y = "Latitude") +
    ggtitle("US Map with Coordinates") +
    theme_bw() +
    ggrepel::geom_text_repel(
      data = cleaned,
      aes(x = longitude, y = latitude, label = location_name),
      size = 3, max.overlaps = Inf) +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab(paste0("Latitude")) +
    ggplot2::labs(title = "Map of sites", subtitle = id) +
    ggplot2::theme(legend.position = "none")

}
my_plot_sites(data)


#my_plot_sites2 --------------------------------------------------------------------------------------------------------------------------------

 datasets = c(data,data2)

  validate_arguments(fun.name = "plot", fun.args = as.list(environment()))

  # check for suggested packages that are required for this function to work
  suggs <- c("ggrepel", "usmap")
  suggsmissing <- !unlist(lapply(suggs, requireNamespace, quietly = TRUE))
  if (any(suggsmissing)) {
    stop("Packages ", paste(suggs, collapse = ", "), " are required for ",
         "running plot_sites(). Packages ",
         paste(suggs[suggsmissing], collapse = ", "), " are not installed.",
         call. = FALSE)
  }

  req_col_names <- c("observation_id", "event_id", "package_id",
                     "location_id", "datetime", "taxon_id", "variable_name",
                     "value", "location_id", "location_name", "longitude",
                     "latitude")

  result <- vector("list", length(datasets))
  result <- append(result, data)


 # detect data type, extract observation table
 data_type <- detect_data_type(datasets)
 if(data_type == "table" && all(req_col_names %in% names(data))){
   flat_data <- data %>% dplyr::distinct()
   if(is.na(id)) id <- paste(unique(data$package_id), collapse = " | ")
 }else if(data_type == "table" && !all(req_col_names %in% names(data))){
   stop("please provide a valid ecocomDP dataset or a table that includes the columns present in the ecocomDP 'observation' and 'taxon' tables")
 }else if(data_type == "dataset_old"){
   flat_data <- flatten_data(data)
   if(is.na(id)) id <- names(data)
 }else if(data_type == "dataset"){
   flat_data <- flatten_data(data)
   if(is.na(id)) id <- data$id
 }else{
   stop("No plotting method currently implemented for this data format")
 }


ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "grey") +
  geom_point(data = flatten_data, aes(x = longitude, y = latitude, color = location_name), size = 3) +
  coord_map() +
  labs(x = "Longitude", y = "Latitude") +
  ggtitle("US Map with Coordinates") +
  theme_bw()




#-----------------------------------------------------------------------------------
my_plot_sites3 <- function(data,
                          id = NA_character_,
                          alpha = 1,
                          labels = TRUE){
  validate_arguments(fun.name = "plot", fun.args = as.list(environment()))

  # check for suggested packages that are required for this function to work
  suggs <- c("ggrepel", "usmap")
  suggsmissing <- !unlist(lapply(suggs, requireNamespace, quietly = TRUE))
  if (any(suggsmissing)) {
    stop("Packages ", paste(suggs, collapse = ", "), " are required for ",
         "running plot_sites(). Packages ",
         paste(suggs[suggsmissing], collapse = ", "), " are not installed.",
         call. = FALSE)
  }

  # TODO: Convert this unreadable block of code to a function like get_observation_table()
  # TODO: Call get_id()
  # required col names in flat data
  req_col_names <- c("observation_id","event_id","package_id","location_id",
                     "datetime","taxon_id","variable_name","value",
                     "location_id","location_name","longitude","latitude")

  # detect data type, extract observation table
  data_type <- detect_data_type(data)
  if(data_type == "table" && all(req_col_names %in% names(data))){
    flat_data <- data %>% dplyr::distinct()
    if(is.na(id)) id <- paste(unique(data$package_id), collapse = " | ")
  }else if(data_type == "table" && !all(req_col_names %in% names(data))){
    stop("please provide a valid ecocomDP dataset or a table that includes the columns present in the ecocomDP 'observation' and 'taxon' tables")
  }else if(data_type == "dataset_old"){
    flat_data <- flatten_data(data)
    if(is.na(id)) id <- names(data)
  }else if(data_type == "dataset"){
    flat_data <- flatten_data(data)
    if(is.na(id)) id <- data$id
  }else{
    stop("No plotting method currently implemented for this data format")
  }


  #This part causes warning messages -------------------------------------------------------
  cleaned <- flat_data %>%
    dplyr::select(
      "longitude",
      "latitude")

  us_map <- map_data("state")

  ggplot() +
    geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "grey") +
    geom_point(data = cleaned, aes(x = longitude, y = latitude, color = location_name), size = 3) +
    coord_map() +
    labs(x = "Longitude", y = "Latitude") +
    ggtitle("US Map with Coordinates") +
    theme_bw() +
    ggrepel::geom_text_repel(
      data = cleaned,
      aes(x = longitude, y = latitude, label = location_name),
      size = 3, max.overlaps = Inf) +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab(paste0("Latitude")) +
    ggplot2::labs(title = "Map of sites", subtitle = id) +
    ggplot2::theme(legend.position = "none")

}


my_plot_sites3(data)
