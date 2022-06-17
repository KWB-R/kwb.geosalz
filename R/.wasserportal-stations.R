if(FALSE) {
  library(kwb.geosalz)

  ### Set target CRS
  crs_target <- 4326
  
  ### Load SVM Boundaries
  shp_svm_path <- system.file("extdata/gis/shapefiles/svm.shp", package = "kwb.geosalz")
 

  ### Remove duplicated points otherwise sf transformations e.g. sf::st_filter 
  ### result an error: https://github.com/r-spatial/sf/issues/1762
  #  library(shapefiles)
  #shp_fri <- shapefiles::read.shapefile(stringr::str_remove(shp_svm_path, "\\.shp"))
  # 
  # is_duplicated <- duplicated(shp_fri$shp$shp[[1]]$points)
  # if(length(duplicated(shp_fri$shp$shp[[1]]$points)) > 0) {
  #   kwb.utils::catAndRun(messageText = "Removing duplicated data points", 
  #                        expr = { 
  #   shp_fri$shp$shp[[1]]$points <- shp_fri$shp$shp[[1]]$points[!is_duplicated,]})
  #   kwb.utils::catAndRun(messageText = "Writting cleaned shapefile", 
  #                        expr = { 
  #  #shapefiles::write.shapefile(shapefile = shp_fri, out.name = "inst/extdata/gis/shapefiles/svm") 
  #                          }
  #   )} else {
  #   message("no duplicates!")
  # }
  
  svm <- sf::st_read(shp_svm_path) %>% 
    sf::st_transform(crs = crs_target)
  sf::st_crs(svm)
  
  sf::st_area(polygon_svm_fri)
  polygon_svm_fri <- svm$geometry[svm$name == "Friedrichshagen"]
  
  add_label <- function(df, 
                        columns = c("Nummer", "start",
                                 "end", "n", "interval")
                        ) {
    
  df %>% 
  dplyr::mutate(label = wasserportal::columns_to_labels(data = df, 
                                  columns = columns,
                                  fmt = "<p>%s: %s</p>",
                                  sep = ""
  ))
}
  
  stations <- wasserportal::get_stations()
  

 swl_master <- wasserportal::get_wasserportal_masters_data(
   station_ids = stations$overview_list$surface_water.water_level %>%  
     dplyr::filter(.data$Betreiber == "SenUVK") %>%  
     dplyr::pull(.data$Messstellennummer)
 )  %>% 
   kwb.geosalz::convert_to_sf(crs_target = crs_target) %>% 
   sf::st_filter(y = polygon_svm_fri) %>% 
   add_label(columns = c("Nummer", "Gewaesser", "Auspraegung", "Betreiber"))
 
  
 gwl_master <- jsonlite::fromJSON("https://kwb-r.github.io/wasserportal/stations_gwl_master.json") %>% 
    kwb.geosalz::convert_to_sf(crs_target = crs_target) %>% 
    sf::st_filter(y = polygon_svm_fri) 
 
 
 gwl_data <- data.table::rbindlist(
   lapply(gwl_master$Nummer, 
          function(id) {
            wasserportal::read_wasserportal_raw_gw(
              station = id, stype = "gwl")
          }))
 
 
 get_data_stats <- function(df, group_col = "Messstellennummer") {
   df %>% 
   dplyr::group_by(.data[[group_col]]) %>% 
   dplyr::summarise(start = min(as.Date(.data$Datum)),
                    end = max(as.Date(.data$Datum)),
                    period = diff(c(.data$start, .data$end)),
                    n = dplyr::n(), 
                    interval = round(.data$period/.data$n, 0))
 
 }
 
 gwl_data_stats <- get_data_stats(gwl_data)
 
 readr::write_csv2(gwl_data_stats, 
                   file = "gwl_data_stats.csv")
 
 
 gwl_master_stats <- gwl_master %>% 
   dplyr::left_join(gwl_data_stats, by = c("Nummer" = "Messstellennummer")) %>% 
   add_label()
 
  
  
  gwq_master <- jsonlite::fromJSON("https://kwb-r.github.io/wasserportal/stations_gwq_master.json") %>% 
    kwb.geosalz::convert_to_sf(crs_target = crs_target) %>% 
    sf::st_filter(y = polygon_svm_fri)
  
  gwq_data <- jsonlite::fromJSON("https://kwb-r.github.io/wasserportal/stations_gwq_data.json") 
    
  gwq_data_stats <- gwq_data %>% 
    dplyr::filter(.data$Parameter == "Chlorid") %>% 
    get_data_stats()
  
  readr::write_csv2(gwq_data_stats, 
                    file = "gwq_data_stats.csv")
  
  gwq_master_stats <- gwq_master %>% 
    dplyr::mutate(Nummer = as.integer(.data$Nummer)) %>% 
    dplyr::left_join(gwq_data_stats, by = c("Nummer" = "Messstellennummer")) %>%
    add_label()
  

    
  # Print Map
  basemap <- svm %>% 
    leaflet::leaflet() %>%
    leaflet::addTiles() %>% 
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
    leaflet::addPolygons(color = "red", fill = FALSE)

  
  swl_map <-  basemap %>% 
    leaflet::addCircles(data = swl_master,
                        color = "blue", 
                        opacity = 0.4, 
                        label = lapply(swl_master$label, htmltools::HTML)) %>% 
    leaflet::addLegend(
      position = "topright",
      colors = c("red", "darkblue"),
      labels = c("SVM Modellgrenze", "OW-Stand (Wasserportal)"),
      title = "Legende")
  
  swl_map
    
  gwl_map <-  basemap %>% 
    leaflet::addCircles(data = gwl_master_stats,
                        color = "blue", 
                        opacity = 0.4, 
                        label = lapply(gwl_master_stats$label, htmltools::HTML)) %>% 
    leaflet::addLegend(
      position = "topright",
      colors = c("red", "blue"),
      labels = c("SVM Modellgrenze", "GW-Stand (Wasserportal)"),
      title = "Legende")
  
  
  gwq_map <-  basemap %>% 
    leaflet::addCircles(data = gwq_master_stats,
                        color = "orange", 
                        opacity = 0.4, 
                        label = lapply(gwq_master_stats$label, htmltools::HTML)) %>% 
    leaflet::addLegend(
      position = "topright",
      colors = c("red", "orange"),
      
      labels = c("SVM Modellgrenze", "GW-G\u00FCte (Wasserportal)"),
      title = "Legende")
  
gwl_map 
gwq_map
}
