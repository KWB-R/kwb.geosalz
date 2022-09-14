#' Convert to SF
#'
#' @param df data frame or tibble with spatial data
#' @param crs_source origingal CRS (default: 25833)
#' @param crs_target target CRS (default: 4326)
#' @param col_coord_x column name of latitude (default: "Rechtswert_UTM_33_N")
#' @param col_cood_y column name of longitude (default: "Hochwert_UTM_33_N")
#'
#' @return data frame or tibble converted to sf  
#' @export
#' @importFrom sf st_as_sf st_transform
#' @importFrom jsonlite fromJSON
#' @examples
#' gwl_master <- jsonlite::fromJSON("https://kwb-r.github.io/wasserportal/stations_gwl_master.json")
#' convert_to_sf(gwl_master)
#' 
convert_to_sf <- function(
    df, 
    crs_source = 25833, 
    crs_target = 4326,
    col_coord_x = "Rechtswert_UTM_33_N",
    col_cood_y = "Hochwert_UTM_33_N"
)
{
  df %>% 
    sf::st_as_sf(
      coords = c(col_coord_x, col_cood_y), 
      crs =  crs_source
    ) %>%
    sf::st_transform(
      crs = crs_target
    )
}
