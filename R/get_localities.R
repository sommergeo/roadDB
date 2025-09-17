# Get localities from ROAD Database
#
# The \strong{\code{road_get_localities_internal}} fetches data of archaeological sites
# (localities) from ROAD database. The ROAD table locality provides basic
# information about each place where an assemblage of archaeological,
# paleoanthropological, paleontological, paleobotanical or other relevant materials
# was described, recorded, sampled or collected. Every locality (site) is situated in a specific
# country within a given geographic region. The name of every locality is unique.
#
# Use arguments to filter search results by location, type, or culture or omit
# them to have a broader result set. All arguments are optional and should be
# omitted or set to NULL when not used.
#
# @param continents specifies the continent(s) (e.g. Africa, Europe, Asia).
# Run \code{road_list_argument_values("continents")} to display possible values.
# The argument \code{continents} is a string (one item) or vector of strings
# (one or more items); defaults to NULL.
# @param subcontinents specifies the continental region(s) (e.g. Southern Europe).
# Run \code{road_list_argument_values("subcontinents")} to display possible values.
# The argument \code{subcontinents} is a string (one item) or vector of strings
# (one or more items); defaults to NULL.
# @param countries specifies the name of the country where a locality is situated
# (e.g. Germany, Kenya, Saudi Arabia). Run \code{road_list_argument_values("countries")}
# to display possible values.
# The argument \code{countries} is a string (one item) or vector of strings
# (one or more items); defaults to NULL.
# @param locality_types specifies the type of locality (e.g. cave, rockshelter, open air).
# Run \code{road_list_argument_values("locality_types")} to display possible values.
# The argument \code{locality_types} is a string (one item) or vector of strings
# (one or more items); defaults to NULL.
# @param cultural_periods specifies the main cultural epoch(s) and includes the
# Eurasian Paleolithic (Lower, Middle, Upper, Epi-) and the African Stone Age
# (Earlier, Middle, Later). Run \code{road_list_argument_values("cultural_periods")}
# to display possible values. The argument \code{cultural_periods} is a string
# (one item) or vector of strings (one or more items); defaults to NULL.
# @param technocomplexes specifies an archaeological culture or named stone tool
# industry (e.g. Oldowan, Acheulean, Mousterian).
# Run \code{road_list_argument_values("technocomplexes")} to display possible values.
# The argument \code{technocomplexes} is a string (one item) or vector of strings
# (one or more items); defaults to NULL.
#
# @details
# This function in comparison to \code{road_get_localities} does not include further
# assemblage data for each location.
#
# @return A data frame with location information. Rows represent individual locations, columns contain location-related details on:
# @return \code{continent}, \code{subcontinent}, \code{country}: The attributes specify the geopolitical information of the locality.
# @return \code{locality_type}: The attribute specifies the type of locality (e.g. cave, rockshelter, open air).
# @return \code{coord_x}, \code{coord_y}: The attributes specify the geographic coordinates (longitude and latitude) of the locality.
# @return \code{cultural_periods}: The attribute specifies the cultural epoch(s) associated with the locality. If there are multiple, they are returned in a comma separated list.
# @return \code{technocomplexes}: The attribute specifies the archaeological culture or named stone tool industry associated with the locality. If there are multiple, they are returned in a comma separated list.
#
# @examples
# df <- road_get_localities_internal(continents = "Europe", countries = c("Germany"),
#                     locality_type = c("basin", "quarry"))
# df <- road_get_localities_internal(countries =
#                 c("Germany", "France"), cultural_periods = "Epipaleolithic")
road_get_localities_internal <- function(
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
    print_null_result_message(continents = continents,
                              subcontinents = subcontinents,
                              countries = countries,
                              locality_types = locality_types,
                              cultural_periods = cultural_periods,
                              technocomplexes = technocomplexes)

  return(data)
}

#' Get localities from ROAD Database
#'
#' The \strong{\code{road_get_localities}} fetches data of archaeological
#' sites (localities) from ROAD database. The ROAD table locality provides basic
#' information about each place where an assemblage of archaeological,
#' paleoanthropological, paleontological, paleobotanical or other relevant materials
#' was described, recorded, sampled or collected. Every locality (site) is
#' situated in a specific country within a given geographic region. The name of every
#' locality is unique.
#'
#' Use parameters to filter search results by location, type, or culture or omit
#' them to have a broader result set. All parameters are optional and should be
#' omitted or set to NULL when not used.
#'
#' @param continents specifies the continent(s) (e.g. Africa, Europe, Asia).
#' Run \code{road_list_argument_values("continents")} to display possible values.
#' The argument\code{continents} is a string (one item) or vector of strings
#' (one or more items); defaults to NULL.
#' @param subcontinents specifies the continental region(s) (e.g. Southern Europe).
#' Run \code{road_list_argument_values("subcontinents")} to display possible values.
#' The argument \code{subcontinents} is a string (one item) or vector of strings
#' (one or more items); defaults to NULL.
#' @param countries specifies the name of the country where a locality is situated
#' (e.g. Germany, Kenya, Saudi Arabia). Run \code{road_list_argument_values("countries")}
#' to display possible values.
#' The argument \code{countries} is a string (one item) or vector of strings
#' (one or more items); defaults to NULL.
#' @param locality_types specifies the type of locality (e.g. cave, rockshelter, open air).
#' Run \code{road_list_argument_values("locality_types")} to display possible values.
#' The argument \code{locality_types} is a string (one item) or vector of strings
#' (one or more items); defaults to NULL.
#' @param cultural_periods specifies the main cultural epoch(s) and includes the
#' Eurasian Paleolithic (Lower, Middle, Upper, Epi-) and the African Stone Age
#' (Earlier, Middle, Later). Run \code{road_list_argument_values("cultural_periods")}
#' to display possible values. The argument \code{cultural_periods} is a string
#' (one item) or vector of strings (one or more items); defaults to NULL.
#' @param technocomplexes specifies an archaeological culture or named stone tool
#' industry (e.g. Oldowan, Acheulean, Mousterian).
#' Run \code{road_list_argument_values("technocomplexes")} to display possible values.
#' The argument \code{technocomplexes} is a string (one item) or vector of strings
#' (one or more items); defaults to NULL.
#' @param categories specifies the assemblage category/categories with the classes
#' human remains, raw material, typology, technology, function, organic tools,
#' symbolic artifacts, feature, miscellaneous finds, paleofauna, animal remains,
#' plant remains. The argument \code{categories} is a string (one item) or
#' vector of strings (one or more items); defaults to NULL.
#' @param age_min specifies the minimum age in years before present, using 1950 CE
#' as the baseline. The argument \code{age_min} is an integer; defaults to NULL.
#' @param age_max specifies the maximum age in years before present, using 1950 CE
#' as the baseline. The argument \code{age_max} is an integer; defaults to NULL.
#'
#' @return A data frame with location information. Rows represent individual locations, columns contain location-related details on:
#' @return \code{continent}, \code{subcontinent}, \code{country}: The attributes specify the geopolitical information of the locality.
#' @return \code{locality_type}: The attribute specifies the type of locality (e.g. cave, rockshelter, open air).
#' @return \code{coord_x}, \code{coord_y}: The attributes specify the geographic coordinates (longitude and latitude) of the locality.
#' @return \code{categories}: Specifies the categories of the findings associated with the locality. If there are multiple, they are returned in a comma separated list.
#' @return \code{cultural_periods}: Specifies the cultural epoch(s) associated with the locality. If there are multiple, they are returned in a comma separated list.
#' @return \code{technocomplexes}: Specifies the archaeological culture or named stone tool industry associated with the locality. If there are multiple, they are returned in a comma separated list.
#' @return \code{subset_min_age}, \code{subset_max_age}: The attributes specify the minimum and maximum age of all assemblages associated with the locality that match the search criteria.
#' @return \code{locality_min_age}, \code{locality_max_age}: The attributes specify the overall minimum and maximum age of all assemblages associated with this locality.
#'
#' @export
#'
#' @examples
#' df <- road_get_localities(continents = "Europe", countries = c("Germany"),
#'                     locality_type = c("basin", "quarry"))
#' df <- road_get_localities(countries = c("Germany", "France"),
#'                     cultural_periods = "Epipaleolithic")
road_get_localities <- function(
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
  assemblages <- road_get_assemblages(continents = continents,
                                          subcontinents = subcontinents,
                                          countries = countries,
                                          locality_types = locality_types,
                                          cultural_periods = cultural_periods,
                                          technocomplexes = technocomplexes,
                                          categories = categories,
                                          age_min = age_min,
                                          age_max = age_max)
  
  assemblages_selected <- select(assemblages, all_of(c(cm_locality_idlocality,
                                                cm_geopolitical_units_continent, 
                                                cm_geopolitical_units_continent_region, 
                                                cm_locality_country,
                                                cm_locality_types, cm_locality_x, 
                                                cm_locality_y, cm_cultural_periods,
                                                cm_technocomplexes, cm_assemblages_categories,
                                                cm_geological_stratigraphy_age_min, 
                                                cm_geological_stratigraphy_age_max)))
  
  
  if (!is.null(assemblages) && nrow(assemblages) != 0)
  {
    
    assemblages_all_info <- road_get_assemblages(continents = continents, 
                                                 subcontinents = subcontinents, 
                                                 countries = countries, 
                                                 locality_types = locality_types)
    assemblages_ages <- assemblages_all_info %>% select(all_of(c(cm_locality_idlocality,
                                                          cm_geological_stratigraphy_age_min,
                                                          cm_geological_stratigraphy_age_max)))
    
    #assemblages_ages <- select(assemblages_all_info, c(cm_locality_idlocality,
    #                                                      cm_geological_stratigraphy_age_min,
    #                                                      cm_geological_stratigraphy_age_max))
    
    #ages_min_max <- assemblages_ages %>% group_by(locality_id) %>% summarise(locality_min_age = min(age_min), 
    #                                                           locality_max_age = max(age_max))
    
    ages_min_max <- summarise(group_by(assemblages_ages, locality_id), locality_min_age = min(age_min), 
                              locality_max_age = max(age_max))
    
    
    assemblages_selected <- select(assemblages, all_of(c(cm_locality_idlocality,
                                                  cm_geopolitical_units_continent, 
                                                  cm_geopolitical_units_continent_region, 
                                                  cm_locality_country,
                                                  cm_locality_types, cm_locality_x, 
                                                  cm_locality_y, cm_cultural_periods,
                                                  cm_technocomplexes, cm_assemblages_categories,
                                                  cm_geological_stratigraphy_age_min, 
                                                  cm_geological_stratigraphy_age_max)))
    
  
    data_tmp <- assemblages_selected %>% group_by(locality_id, continent, subcontinent,
                                                  country, locality_types, coord_x, coord_y
                                                  ) %>% summarise(categories = well_formed_string_to_string_without_duplicates(paste0(categories, collapse = ", ")),
                                                                  cultural_periods = well_formed_string_to_string_without_duplicates(paste0(cultural_periods, collapse = ", ")),
                                                                  technocomplexes = well_formed_string_to_string_without_duplicates(paste0(technocomplexes, collapse = ", ")),
                                                                  subset_min_age = min(age_min),
                                                                  subset_max_age = max(age_max))
    
    # data_tmp <- summarise(group_by(assemblages_selected, locality_id, continent, 
    #                                subcontinent,
    #                                country, locality_types, coord_x, coord_y
    #                                 ), categories = well_formed_string_to_string_without_duplicates(paste0(categories, collapse = ", ")),
    #                                    cultural_periods = well_formed_string_to_string_without_duplicates(paste0(cultural_periods, collapse = ", ")),
    #                                    technocomplexes = well_formed_string_to_string_without_duplicates(paste0(technocomplexes, collapse = ", ")),
    #                                    subset_min_age = min(age_min),
    #                                    subset_max_age = max(age_max))
    # 
    # categories <- subset(data_tmp, select = categories)
    
    data <- inner_join(data_tmp, ages_min_max, by = c("locality_id"),
                      copy = FALSE, na_matches = "na")

    return(data)
  }
  else return(select(assemblages, all_of(c(cm_locality_idlocality,
                                    cm_geopolitical_units_continent, 
                                    cm_geopolitical_units_continent_region, 
                                    cm_locality_country,
                                    cm_locality_types, cm_locality_x, 
                                    cm_locality_y, cm_cultural_periods,
                                    cm_technocomplexes, cm_assemblages_categories
                                    ))))
}
