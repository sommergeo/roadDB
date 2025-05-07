#' Get localities from ROAD Database
#'
#' `road_get_localities` fetches data of archaeological sites (localities) from ROAD database.
#'
#' Use parameters to spatially delimit search results or omit them to have a broader radius.
#' All parameters are optional and should be omitted or set to NULL when not used.
#'
#' @param continents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param subcontinents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param countries string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param locality_types string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param cultural_periods string (one item) or vector of strings (one or more items); defaults to NULL.
#'
#' @return Database search result as list of localities.
#' @export
#'
#' @examples road_get_localities(continents = c("Europe"), countries = c("Germany", "France"))
#' @examples road_get_localities(continents = "Europe", countries = c("Germany", "France"))
#' @examples road_get_localities(countries = c("Germany", "France"), locality_type = "cave")
#' @examples road_get_localities(countries = c("Germany", "France"), cultural_periods = "Middle Paleolithic")
road_get_localities <- function(
    continents = NULL,
    subcontinents = NULL,
    countries = NULL,
    locality_types = NULL,
    cultural_periods = NULL
)
{
  # select fields
  select_fields <- c(
    paste0("locality.idlocality AS ", cm_locality_idlocality),
    paste0("geopolitical_units.continent AS ", cm_geopolitical_units_continent),
    paste0("geopolitical_units.continent_region AS ", cm_geopolitical_units_continent_region),
    paste0("locality.country AS ", cm_locality_country),
    paste0("locality.type AS ", cm_locality_types),
    paste0("locality.x AS ", cm_locality_x),
    paste0("locality.y AS ", cm_locality_y),
    paste0("STRING_AGG(DISTINCT archaeological_stratigraphy.cultural_period, ', ') AS ", cm_cultural_periods)
  )
  
  # order by
  query_order_by <- ""
  if (!is.null(countries))
  {
    query_order_by <- paste("ORDER BY", cm_locality_idlocality)
  }
  
  # combine query parts
  query <- paste(
    # SELECT
    "SELECT DISTINCT",
    paste(select_fields, collapse = ", "),
    # FROM
    "FROM locality",
    "INNER JOIN geopolitical_units ON
      locality.country = geopolitical_units.geopolitical_name",
    "LEFT JOIN archaeological_layer ON
      locality.idlocality = archaeological_layer.locality_idlocality",
    "LEFT JOIN archaeological_stratigraphy ON
      archaeological_layer.archstratigraphy_idarchstrat = archaeological_stratigraphy.idarchstrat",
    # WHERE
    "WHERE NOT locality.no_data_entry AND geopolitical_units.rank = 1",
    parameter_to_query("AND geopolitical_units.continent IN (", continents, ")"),
    parameter_to_query("AND geopolitical_units.continent_region IN (", subcontinents, ")"),
    parameter_to_query("AND locality.country IN (", countries, ")"),
    parameter_to_query("AND string_to_array(locality.type, ', ') && array[", locality_types, "]"),
    parameter_to_query(
      "AND locality.idlocality IN
        (SELECT DISTINCT locality_idlocality FROM archaeological_layer 
        LEFT JOIN archaeological_stratigraphy ON archaeological_layer.archstratigraphy_idarchstrat = archaeological_stratigraphy.idarchstrat 
        WHERE archaeological_stratigraphy.cultural_period IN (", cultural_periods, "))"
    ),
    "GROUP BY locality.idlocality, geopolitical_units.continent, geopolitical_units.continent_region, locality.country, locality.type, locality.x, locality.y",
    query_order_by
  )
  
  data <- road_run_query(query)
  
  return(data)
}