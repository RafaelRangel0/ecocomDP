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

# make table vec
# inside loop:
# #make temp
# grab first 4
# put them in temp
# flatten temp vect
# append flattened_temp vect to table vect
# clear temp vect

new_df <- data.frame(
  Column1 = c(1, 2, 3),
  Column2 = c("A", "B", "C"),
  Column3 = c(TRUE, FALSE, TRUE)
)



tables = array()
j = 4
for(i in 1:(length(datasets)/4)){
  temp_vector = datasets[i:j]
  flattened_data = flatten_data(temp_vector)
  tables <- append(tables, flattened_vector)
  i= i + 4
  j= j+4

  if(i == 1){

  }

}


table_df = tables %>% as.data.frame()

#
#
#
#
#
#
#
#
#
#
# # detect data type, extract observation table
# data_type <- detect_data_type(datasets)
# if(data_type == "table" && all(req_col_names %in% names(data))){
#   flat_data <- data %>% dplyr::distinct()
#   if(is.na(id)) id <- paste(unique(data$package_id), collapse = " | ")
# }else if(data_type == "table" && !all(req_col_names %in% names(data))){
#   stop("please provide a valid ecocomDP dataset or a table that includes the columns present in the ecocomDP 'observation' and 'taxon' tables")
# }else if(data_type == "dataset_old"){
#   flat_data <- flatten_data(data)
#   if(is.na(id)) id <- names(data)
# }else if(data_type == "dataset"){
#   flat_data <- flatten_data(data)
#   if(is.na(id)) id <- data$id
# }else{
#   stop("No plotting method currently implemented for this data format")
# }
#
#
# ggplot() +
#   geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "grey") +
#   geom_point(data = flatten_data, aes(x = longitude, y = latitude, color = location_name), size = 3) +
#   coord_map() +
#   labs(x = "Longitude", y = "Latitude") +
#   ggtitle("US Map with Coordinates") +
#   theme_bw()


# my_plot_sites1000 <- function(datasets, id = NA_character_, alpha = 1, labels = TRUE) {
#   validate_arguments(fun.name = "plot", fun.args = as.list(environment()))
#
#   # check for suggested packages that are required for this function to work
#   suggs <- c("ggrepel", "usmap")
#   suggsmissing <- !unlist(lapply(suggs, requireNamespace, quietly = TRUE))
#   if (any(suggsmissing)) {
#     stop("Packages ", paste(suggs, collapse = ", "), " are required for ",
#          "running plot_sites(). Packages ",
#          paste(suggs[suggsmissing], collapse = ", "), " are not installed.",
#          call. = FALSE)
#   }
#
#   # List to store cleaned datasets
#   cleaned_datasets <- list()
#
#   for (d in datasets) {
#     # TODO: Convert this unreadable block of code to a function like get_observation_table()
#     # TODO: Call get_id()
#     # required col names in flat data
#     req_col_names <- c("observation_id", "event_id", "package_id", "location_id",
#                        "datetime", "taxon_id", "variable_name", "value",
#                        "location_id", "location_name", "longitude", "latitude")
#
#     # detect data type, extract observation table
#     # data_type <- detect_data_type(data)
#     # if (data_type == "table" && all(req_col_names %in% names(data))) {
#         #flat_data <- d %>% dplyr::distinct()
#     #   if (is.na(id)) id <- paste(unique(data$package_id), collapse = " | ")
#     # } else if (data_type == "table" && !all(req_col_names %in% names(data))) {
#     #   stop("please provide a valid ecocomDP dataset or a table that includes the columns present in the ecocomDP 'observation' and 'taxon' tables")
#     # } else if (data_type == "dataset_old") {
#     #   flat_data <- flatten_data(data)
#     #   if (is.na(id)) id <- names(data)
#     # } else if (data_type == "dataset") {
#     #   flat_data <- flatten_data(data)
#     #   if (is.na(id)) id <- data$id
#     # } else {
#     #   stop("No plotting method currently implemented for this data format")
#     # }
#
#     # Cleaned dataset
#     cleaned <- flat_data %>%
#       dplyr::select("longitude", "latitude", "location_name", "package_id") %>%
#       dplyr::distinct()
#
#     cleaned_datasets[[length(cleaned_datasets) + 1]] <- cleaned
#   }
#
#   # Combine all cleaned datasets
#   combined_cleaned <- do.call(rbind, cleaned_datasets)
#
#   # Transform the coordinates
#   transformed_cleaned <- suppressWarnings(usmap::usmap_transform(combined_cleaned,
#                                                                  input_names = c("longitude", "latitude")))
#
#   us_map <- map_data("state")
#
#   ggplot() +
#     geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "grey") +
#     geom_point(data = combined_cleaned, aes(x = longitude, y = latitude, color = location_name), size = 3) +
#     coord_map() +
#     labs(x = "Longitude", y = "Latitude") +
#     ggtitle("US Map with Coordinates") +
#     theme_bw()
# }
#
# my_plot_sites1000(data2,data)
