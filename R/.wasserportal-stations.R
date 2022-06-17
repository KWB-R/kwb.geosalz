if(FALSE) {
  library(kwb.geosalz)
  library(shapefiles)
  
  ### Set target CRS
  crs_target <- 4326
  
  ### Load SVM Boundaries
  shp_svm_path <- system.file("extdata/gis/shapefiles/svm.shp", package = "kwb.geosalz")
  
  
  shp_fri <- shapefiles::read.shapefile(stringr::str_remove(shp_svm_path, "\\.shp"))
  shapefiles::read.dbf(stringr::str_remove(shp_svm_path, "\\.shp"))
  ### Remove duplicated points otherwise sf transformations e.g. sf::st_filter 
  ### result an error: https://github.com/r-spatial/sf/issues/1762
  
  is_duplicated <- duplicated(shp_fri$shp$shp[[1]]$points)
  if(any(is_duplicated)) {
    kwb.utils::catAndRun(messageText = "Removing duplicated data points", 
                         expr = { 
    shp_fri$shp$shp[[1]]$points <- shp_fri$shp$shp[[1]]$points[!is_duplicated,]})
    kwb.utils::catAndRun(messageText = "Writting cleaned shapefile", 
                         expr = { 
   shapefiles::write.shapefile(shapefile = shp_fri, out.name = "inst/extdata/gis/shapefiles/svm") }
                         )
  } else {
    message("no duplicates!")
  }
  
  svm <- sf::st_read(shp_svm_path) %>% 
    sf::st_transform(crs = crs_target)
  sf::st_crs(svm)
  
  sf::st_area(polygon_svm_fri)
  polygon_svm_fri <- svm$geometry[svm$name == "Friedrichshagen"]
  
  gwl_master <- jsonlite::fromJSON("https://kwb-r.github.io/wasserportal/stations_gwl_master.json") %>% 
    kwb.geosalz::convert_to_sf(crs_target = crs_target) %>% 
    sf::st_filter(y = polygon_svm_fri)
  
  gwq_master <- jsonlite::fromJSON("https://kwb-r.github.io/wasserportal/stations_gwq_master.json") %>% 
    kwb.geosalz::convert_to_sf(crs_target = crs_target) %>% 
    sf::st_filter(y = polygon_svm_fri)
  
  
}
