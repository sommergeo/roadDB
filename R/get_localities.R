#' Get localities from ROAD Database
#'
#' The \strong{\code{road_get_localities}} fetches data of archaeological sites (localities) from ROAD database.
#' The ROAD table locality provides basic information about each place where an assemblage of
#' archaeological, paleoanthropological, paleontological, paleobotanical or other relevant materials
#' was described, recorded, sampled or collected. Every locality (site) is situated in a specific
#' country within a given geographic region. The name of every locality is unique.
#'
#' Use parameters to filter search results by location, type, or culture or omit them to have a broader result set.
#' All parameters are optional and should be omitted or set to NULL when not used.
#'
#' @param continents specifies the continent(s) of the country/countries, e.g. Africa, Europe, Asia. 
#' The parameter continents is a string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param subcontinents specifies the continental region(s) of the country , e.g. Southern Europe. 
#' The parameter subcontinents is a string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param countries specifies the name of the country where a locality is situated (e.g.
#' Germany, Kenya, Saudi Arabia, China). The parameter countries is a string (one item) 
#' or vector of strings (one or more items); defaults to NULL.
#' @param locality_types specifies the type of locality (e.g. cave, rockshelter, open air, profile, outcrop,
#' mine, quarry, boring). The parameter locality_types is a string (one item) or 
#' vector of strings (one or more items); defaults to NULL.
#' @param cultural_periods specifies the main cultural epoch(s) and includes the Eurasian
#' Paleolithic (Lower, Middle, Upper, Epi) and the African Stone Age (Earlier, Middle, Later). 
#' The parameter cultural_periods is a string (one item) or vector of strings (one or more items); defaults to NULL.
#' 
#' @return Database search result as a data frame with the information about localities 
#' like their geographic information, cultural period and type.
#' @export
#'
#' @examples
#' df <- road_get_localities(continents = "Europe", countries = c("Germany"), 
#'                     locality_type = c("basin", "quarry"))
#' df <- road_get_localities(countries = c("Germany", "France"), cultural_periods = "Epipaleolithic")
road_get_localities <- function(
    continents = NULL,
    subcontinents = NULL,
    countries = NULL,
    locality_types = NULL,
    cultural_periods = NULL,
    technocomplexes = NULL
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
    paste0("STRING_AGG(DISTINCT archaeological_stratigraphy.cultural_period, ', ') AS ", cm_cultural_periods),
    paste0("STRING_AGG(DISTINCT archaeological_stratigraphy.technocomplex, ', ') AS ", cm_technocomplexes)
  )
  
  # order by
  query_order_by <- ""
  #if (!is.null(countries))
  #{
    query_order_by <- paste("ORDER BY", cm_locality_idlocality)
  #}
  
  # combine query parts
  query <- paste(
    "SELECT DISTINCT",
    paste(select_fields, collapse = ", "),
    "FROM locality",
    "INNER JOIN geopolitical_units ON
      locality.country = geopolitical_units.geopolitical_name",
    "LEFT JOIN archaeological_layer ON
      locality.idlocality = archaeological_layer.locality_idlocality",
    "LEFT JOIN archaeological_stratigraphy ON
      archaeological_layer.archstratigraphy_idarchstrat = archaeological_stratigraphy.idarchstrat",
    "WHERE NOT locality.no_data_entry AND geopolitical_units.rank = 1",
    parameter_to_query("AND geopolitical_units.continent IN (", continents, ")"),
    parameter_to_query("AND geopolitical_units.continent_region IN (", subcontinents, ")"),
    parameter_to_query("AND locality.country IN (", countries, ")"),
    parameter_to_query("AND string_to_array(locality.type, ', ') && array[", locality_types, "]"),
    parameter_to_query("AND archaeological_stratigraphy.cultural_period IN (", cultural_periods, ")"),
    parameter_to_query("AND archaeological_stratigraphy.technocomplex IN (", technocomplexes, ")"),
    # parameter_to_query(
    #   "AND locality.idlocality IN
    #     (SELECT DISTINCT locality_idlocality FROM archaeological_layer 
    #     LEFT JOIN archaeological_stratigraphy ON archaeological_layer.archstratigraphy_idarchstrat = archaeological_stratigraphy.idarchstrat 
    #     WHERE archaeological_stratigraphy.cultural_period IN (", cultural_periods, "))"
    # ),
    "GROUP BY locality.idlocality, geopolitical_units.continent, geopolitical_units.continent_region, locality.country, locality.type, locality.x, locality.y",
    query_order_by
  )
  
  data <- road_run_query(query)

  if (nrow(data) == 0) 
    print_null_result_message(  continents,
                                subcontinents,
                                countries,
                                locality_types,
                                cultural_periods,
                                technocomplexes
    )

  return(data)
}


#' Get localities from ROAD Database
#'
#' The \strong{\code{road_get_localities_ext}} fetches data of archaeological sites (localities) from ROAD database.
#' The ROAD table locality provides basic information about each place where an assemblage of
#' archaeological, paleoanthropological, paleontological, paleobotanical or other relevant materials
#' was described, recorded, sampled or collected. Every locality (site) is situated in a specific
#' country within a given geographic region. The name of every locality is unique.
#'
#' Use parameters to filter search results by location, type, or culture or omit them to have a broader result set.
#' All parameters are optional and should be omitted or set to NULL when not used.
#'
#' @param continents specifies the continent(s) of the country/countries, e.g. Africa, Europe, Asia. 
#' The parameter continents is a string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param subcontinents specifies the continental region(s) of the country , e.g. Southern Europe. 
#' The parameter subcontinents is a string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param countries specifies the name of the country where a locality is situated (e.g.
#' Germany, Kenya, Saudi Arabia, China). The parameter countries is a string (one item) 
#' or vector of strings (one or more items); defaults to NULL.
#' @param locality_types specifies the type of locality (e.g. cave, rockshelter, open air, profile, outcrop,
#' mine, quarry, boring). The parameter locality_types is a string (one item) or 
#' vector of strings (one or more items); defaults to NULL.
#' @param cultural_periods specifies the main cultural epoch(s) and includes the Eurasian
#' Paleolithic (Lower, Middle, Upper, Epi) and the African Stone Age (Earlier, Middle, Later). 
#' The parameter cultural_periods is a string (one item) or vector of strings (one or more items); defaults to NULL.
#' 
#' @return Database search result as a data frame with the information about localities 
#' like their geographic information, cultural period and type.
#' @export
#'
#' @examples
#' df <- road_get_localities_ext(continents = "Europe", countries = c("Germany"), 
#'                     locality_type = c("basin", "quarry"))
#' df <- road_get_localities_ext(countries = c("Germany", "France"), cultural_periods = "Epipaleolithic")
road_get_localities_ext <- function(
    continents = NULL,
    subcontinents = NULL,
    countries = NULL,
    locality_types = NULL,
    cultural_periods = NULL,
    technocomplexes = NULL,
    categories = NULL,
    age_min = NULL,
    age_max = NULL
)
{
  assemblages_all_info <- road_get_assemblages(continents, subcontinents, countries, locality_types)
  assemblages_ages <- assemblages_all_info %>% select(c(cm_locality_idlocality,
                                                  cm_geological_stratigraphy_age_min,
                                                  cm_geological_stratigraphy_age_max))
  ages_min_max <- assemblages_ages %>% group_by(locality_id) %>% summarise(locality_min_age = min(age_min), 
                                                             locality_max_age = max(age_max))
  
  assemblages <- road_get_assemblages(continents, subcontinents, countries, 
                                      locality_types, cultural_periods, technocomplexes, 
                                      categories, age_min, age_max)
  if (!is.null(assemblages) && nrow(assemblages) != 0)
  {
    assemblages_selected <- assemblages %>% select(c(cm_locality_idlocality,
                                                   cm_geopolitical_units_continent, 
                                                   cm_geopolitical_units_continent_region, 
                                                   cm_locality_country,
                                                   cm_locality_types, cm_locality_x, 
                                                   cm_locality_y, cm_cultural_periods,
                                                   cm_technocomplexes, cm_assemblages_categories,
                                                   cm_geological_stratigraphy_age_min, 
                                                   cm_geological_stratigraphy_age_max))
  
    data_tmp <- assemblages_selected %>% group_by(locality_id, continent, subcontinent,
                                            country, locality_types, coord_x, 
                                            coord_y) %>% 
                                            summarise(
                                            categories = paste0(categories, collapse = ", "),
                                            cultural_periods = paste0(cultural_periods, collapse = ", "),
                                            technocomplexes = paste0(technocomplexes, collapse = ", "),
                                            subset_min_age = min(age_min), 
                                            subset_max_age = max(age_max)
                                            )
    data <- full_join(data_tmp, ages_min_max, by = c("locality_id"),
                      copy = FALSE, na_matches = "na")
                           
    return(data)
  }
  else return(assemblages)
  
}