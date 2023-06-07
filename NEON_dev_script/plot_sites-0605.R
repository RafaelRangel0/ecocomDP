library(ecocomDP)
library(tidyverse)
library(ggplot2)
library(devtools)
library(terra)
library(rnaturalearth)
library(maps)
library(geodata)

#Where can I find the column names that all




rm(list = ls())


load_all()

#DONT TOUCH
my_plot_sites <- function(data,
                          id = NA_character_,
                          alpha = 1,
                          labels = TRUE,
                          color_by = "location_abbrv",
                          shape_by = "location_abbrv"){
  validate_arguments(fun.name = "plot", fun.args = as.list(environment()))

  # check for suggested packages that are required for this function to work
  suggs <- c("ggrepel", "usmap","maps")
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


  #Idea have vector possible columns (columns that don't ruin the distinct observations) and select the ones that are part of the input datasets







  cleaned <- flat_data %>%
    dplyr::select(
      "longitude",
      "latitude",
      "location_name",
      "package_id",
      # "aquaticSiteType",
      # "habitatType",
      # "samplerType",
      # "aquaticSiteType"
      ) %>%
    dplyr::distinct()

  cleaned$location_abbrv = substr(cleaned$location_name, 1, 4)


  world <- map_data("world")



  ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey") +
    geom_point(data = cleaned, aes(x = longitude, y = latitude, color = .data[[color_by]], size = 3, shape = .data[[shape_by]])) +
    labs(x = "Longitude", y = "Latitude", color = color_by) +
    ggtitle("US Map with Coordinates") +
    theme_bw() +
    ggrepel::geom_text_repel(
      data = cleaned,
      aes(x = longitude, y = latitude, label = location_name),
      size = 3, max.overlaps = Inf) +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude") +
    ggplot2::labs(title = "Map of sites", subtitle = id) +
    ggplot2::theme(legend.position = "right") +
    coord_cartesian(xlim = c(-165, -40), ylim = c(15, 75))

}
my_plot_sites(data2)

#-------------------------------------------------

my_plot_sites2 <- function(data,
                          id = NA_character_,
                          alpha = 1,
                          labels = TRUE,
                          color_by = "location_abbrv") {
  # ... existing code ...



  cleaned <- flat_data %>%
    # dplyr::select(
    #   "longitude",
    #   "latitude",
    #   "location_name",
    #   "package_id"
    # ) %>%
    dplyr::distinct()

  # desired_letters <-
  # matching_columns <- grep(paste0(".*", desired_letters, "$"), names(flat_data2), value = TRUE, ignore.case = TRUE)

  # flat_data$type =

  cleaned$location_abbrv <- substr(cleaned$location_name, 1, 4)

  world <- map_data("world")

  #alaska_map <- map_data("state", region = "alaska")

  # Combine map data for the United States and Alaska
  #combined_map <- rbind(us_map, alaska_map)

  ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey") +
    geom_point(data = cleaned, aes(x = longitude, y = latitude, color = , size = 3)) +
    labs(x = "Longitude", y = "Latitude", color = color_by) +
    ggtitle("US Map with Coordinates") +
    theme_bw() +
    ggrepel::geom_text_repel(
      data = cleaned,
      aes(x = longitude, y = latitude, label = location_name),
      size = 3, max.overlaps = Inf) +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude") +
    ggplot2::labs(title = "Map of sites", subtitle = id) +
    ggplot2::theme(legend.position = "right") +
    coord_cartesian(xlim = c(-165, -40), ylim = c(15, 75))
}

my_plot_sites2(data)

#----------------------------------------------------------------------------------------------

my_plot_sites3 <- function(data,
                          id = NA_character_,
                          alpha = 1,
                          labels = TRUE,
                          color_var = "package_id",
                          shape_var = "package_id"){
  validate_arguments(fun.name = "plot", fun.args = as.list(environment()))

  # check for suggested packages that are required for this function to work
  suggs <- c("ggrepel", "usmap","maps")
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
      "package_id"
    ) %>%
    dplyr::distinct()

  #cleaned$location_abbrv = substr(cleaned$location_name, 1, 4)


  us_map <- map_data("state")


  ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey") +
    geom_point(data = cleaned, aes(x = longitude, y = latitude, color = .data[[color_by]], size = 3)) +
    labs(x = "Longitude", y = "Latitude", color = color_by) +
    ggtitle("US Map with Coordinates") +
    theme_bw() +
    ggrepel::geom_text_repel(
      data = cleaned,
      aes(x = longitude, y = latitude, label = location_name),
      size = 3, max.overlaps = Inf) +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude") +
    ggplot2::labs(title = "Map of sites", subtitle = id) +
    ggplot2::theme(legend.position = "right") +
    coord_cartesian(xlim = c(-165, -40), ylim = c(15, 75))

}

#--------------------------------------------------------------------------------------------

my_plot_sites3 <- function(data,
                          id = NA_character_,
                          alpha = 1,
                          color_var = "Red",
                          shape_var = "location_name",
                          size_var = 3){
  validate_arguments(fun.name = "plot", fun.args = as.list(environment()))

  # check for suggested packages that are required for this function to work
  suggs <- c("ggrepel", "usmap","maps")
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

  cleaned <- flat_data %>%
    dplyr::select(
      "longitude",
      "latitude",
      "location_name",
      "package_id"
    ) %>%
    dplyr::distinct()

  cleaned$location_abbrv = substr(cleaned$location_name, 1, 4)

  world <- map_data("world")

  plot <- ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey") +
    geom_point(data = flat_data, aes(x = longitude, y = latitude, color = .data[[color_by]], shape = .data[[shape_by]], size = 3)) +
    labs(x = "Longitude", y = "Latitude", color = color_by) +
    ggtitle("US Map with Coordinates") +
    theme_bw() +
    ggrepel::geom_text_repel(
      data = cleaned,
      aes(x = longitude, y = latitude, label = location_name),
      size = 3, max.overlaps = Inf) +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude") +
    ggplot2::labs(title = "Map of sites", subtitle = id) +
    ggplot2::theme(legend.position = "right") +
    coord_cartesian(xlim = c(-165, -40), ylim = c(15, 75))

  return(plot)

}





ggplot1 = my_plot_sites3(d, shape_by = "plotType", color = "siteID", size_by = "siteID")
ggplot3 = my_plot_sites3(both_datasets, shape_by = "package_id", color = "siteID", size_by = "siteID")

plotly1 = ggplotly(ggplot1)




