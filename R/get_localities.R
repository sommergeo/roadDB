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
# @param continent specifies the continent(s) (e.g. Africa, Europe, Asia).
# Run \code{road_list_argument_values("continent")} to display possible values.
# The argument \code{continent} is a string (one item) or vector of strings
# (one or more items); defaults to NULL.
# @param subcontinent specifies the continental region(s) (e.g. Southern Europe).
# Run \code{road_list_argument_values("subcontinent")} to display possible values.
# The argument \code{subcontinent} is a string (one item) or vector of strings
# (one or more items); defaults to NULL.
# @param country specifies the name of the country where a locality is situated
# (e.g. Germany, Kenya, Saudi Arabia). Run \code{road_list_argument_values("country")}
# to display possible values.
# The argument \code{country} is a string (one item) or vector of strings
# (one or more items); defaults to NULL.
# @param locality_type specifies the type of locality (e.g. cave, rockshelter, open air).
# Run \code{road_list_argument_values("locality_type")} to display possible values.
# The argument \code{locality_type} is a string (one item) or vector of strings
# (one or more items); defaults to NULL.
# @param cultural_period specifies the main cultural epoch(s) and includes the
# Eurasian Paleolithic (Lower, Middle, Upper, Epi-) and the African Stone Age
# (Earlier, Middle, Later). Run \code{road_list_argument_values("cultural_period")}
# to display possible values. The argument \code{cultural_period} is a string
# (one item) or vector of strings (one or more items); defaults to NULL.
# @param technocomplex specifies an archaeological culture or named stone tool
# industry (e.g. Oldowan, Acheulean, Mousterian).
# Run \code{road_list_argument_values("technocomplex")} to display possible values.
# The argument \code{technocomplex} is a string (one item) or vector of strings
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
# @return \code{cultural_period}: The attribute specifies the cultural epoch(s) associated with the locality. If there are multiple, they are returned in a comma separated list.
# @return \code{technocomplex}: The attribute specifies the archaeological culture or named stone tool industry associated with the locality. If there are multiple, they are returned in a comma separated list.
#
# @examples
# df <- road_get_localities_internal(continent = "Europe", country = c("Germany"),
#                     locality_type = c("basin", "quarry"))
# df <- road_get_localities_internal(country =
#                 c("Germany", "France"), cultural_period = "Epipaleolithic")
road_get_localities_internal <- function(
    continent = NULL,
    subcontinent = NULL,
    country = NULL,
    locality_type = NULL,
    cultural_period = NULL,
    technocomplex = NULL
)
{
  # select fields
  select_fields <- c(
    paste0("locality.idlocality AS ", cm_locality_idlocality),
    paste0("geopolitical_units.continent AS ", cm_geopolitical_units_continent),
    paste0("geopolitical_units.continent_region AS ", cm_geopolitical_units_continent_region),
    paste0("locality.country AS ", cm_locality_country),
    paste0("locality.type AS ", cm_locality_type),
    paste0("locality.x AS ", cm_locality_x),
    paste0("locality.y AS ", cm_locality_y),
    paste0("STRING_AGG(DISTINCT archaeological_stratigraphy.cultural_period, ', ') AS ", cm_cultural_period),
    paste0("STRING_AGG(DISTINCT archaeological_stratigraphy.technocomplex, ', ') AS ", cm_technocomplex)
  )
  
  # order by
  query_order_by <- ""
  #if (!is.null(country))
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
    parameter_to_query("AND geopolitical_units.continent IN (", continent, ")"),
    parameter_to_query("AND geopolitical_units.continent_region IN (", subcontinent, ")"),
    parameter_to_query("AND locality.country IN (", country, ")"),
    parameter_to_query("AND string_to_array(locality.type, ', ') && array[", locality_type, "]"),
    parameter_to_query("AND archaeological_stratigraphy.cultural_period IN (", cultural_period, ")"),
    parameter_to_query("AND archaeological_stratigraphy.technocomplex IN (", technocomplex, ")"),
    # parameter_to_query(
    #   "AND locality.idlocality IN
    #     (SELECT DISTINCT locality_idlocality FROM archaeological_layer 
    #     LEFT JOIN archaeological_stratigraphy ON archaeological_layer.archstratigraphy_idarchstrat = archaeological_stratigraphy.idarchstrat 
    #     WHERE archaeological_stratigraphy.cultural_period IN (", cultural_period, "))"
    # ),
    "GROUP BY locality.idlocality, geopolitical_units.continent, geopolitical_units.continent_region, locality.country, locality.type, locality.x, locality.y",
    query_order_by
  )
  
  data <- road_run_query(query)

  if (nrow(data) == 0) 
    print_null_result_message(continent = continent,
                              subcontinent = subcontinent,
                              country = country,
                              locality_type = locality_type,
                              cultural_period = cultural_period,
                              technocomplex = technocomplex)

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
#' @param continent specifies the continent(s) (e.g. Africa, Europe, Asia).
#' Run \code{road_list_argument_values("continent")} to display possible values.
#' The argument\code{continent} is a string (one item) or vector of strings
#' (one or more items); defaults to NULL.
#' @param subcontinent specifies the continental region(s) (e.g. Southern Europe).
#' Run \code{road_list_argument_values("subcontinent")} to display possible values.
#' The argument \code{subcontinent} is a string (one item) or vector of strings
#' (one or more items); defaults to NULL.
#' @param country specifies the name of the country where a locality is situated
#' (e.g. Germany, Kenya, Saudi Arabia). Run \code{road_list_argument_values("country")}
#' to display possible values.
#' The argument \code{country} is a string (one item) or vector of strings
#' (one or more items); defaults to NULL.
#' @param locality_type specifies the type of locality (e.g. cave, rockshelter, open air).
#' Run \code{road_list_argument_values("locality_type")} to display possible values.
#' The argument \code{locality_type} is a string (one item) or vector of strings
#' (one or more items); defaults to NULL.
#' @param cultural_period specifies the main cultural epoch(s) and includes the
#' Eurasian Paleolithic (Lower, Middle, Upper, Epi-) and the African Stone Age
#' (Earlier, Middle, Later). Run \code{road_list_argument_values("cultural_period")}
#' to display possible values. The argument \code{cultural_period} is a string
#' (one item) or vector of strings (one or more items); defaults to NULL.
#' @param technocomplex specifies an archaeological culture or named stone tool
#' industry (e.g. Oldowan, Acheulean, Mousterian).
#' Run \code{road_list_argument_values("technocomplex")} to display possible values.
#' The argument \code{technocomplex} is a string (one item) or vector of strings
#' (one or more items); defaults to NULL.
#' @param category specifies the assemblage category with the classes
#' human remains, raw material, typology, technology, function, organic tools,
#' symbolic artifacts, feature, miscellaneous finds, paleofauna, animal remains,
#' plant remains. The argument \code{category} is a string (one item) or
#' vector of strings (one or more items); defaults to NULL.
#' @param age_min specifies the minimum age in years before present, using 1950 CE
#' as the baseline. If possible the argument \code{age_min} will be converted to an integer; defaults to NULL.
#' @param age_max specifies the maximum age in years before present, using 1950 CE
#' as the baseline. If possible the argument \code{age_max} will be converted to an integer; defaults to NULL.
#'
#' @return A data frame with location information. Rows represent individual locations, columns contain location-related details on:
#' @return \code{continent}, \code{subcontinent}, \code{country}: The attributes specify the geopolitical information of the locality.
#' @return \code{locality_type}: The attribute specifies the type of locality (e.g. cave, rockshelter, open air).
#' @return \code{coord_x}, \code{coord_y}: The attributes specify the geographic coordinates (longitude and latitude) of the locality.
#' @return \code{category}: Specifies the category of the findings associated with the locality. If there are multiple, they are returned in a comma separated list.
#' @return \code{cultural_period}: Specifies the cultural epoch(s) associated with the locality. If there are multiple, they are returned in a comma separated list.
#' @return \code{technocomplex}: Specifies the archaeological culture or named stone tool industry associated with the locality. If there are multiple, they are returned in a comma separated list.
#' @return \code{subset_age_min}, \code{subset_age_max}: The attributes specify the minimum and maximum age of all assemblages associated with the locality that match the search criteria.
#' @return \code{locality_age_min}, \code{locality_age_max}: The attributes specify the overall minimum and maximum age of all assemblages associated with this locality.
#'
#' @export
#'
#' @importFrom dplyr all_of
#' @importFrom dplyr select
#'
#' @examples
#' df <- road_get_localities(continent = "Europe", country = c("Germany"),
#'                     locality_type = c("basin", "quarry"))
#' df <- road_get_localities(country = c("Germany", "France"),
#'                     cultural_period = "Epipaleolithic")
road_get_localities <- function(
    continent = NULL,
    subcontinent = NULL,
    country = NULL,
    locality_type = NULL,
    cultural_period = NULL,
    technocomplex = NULL,
    category = NULL,
    age_min = NULL,
    age_max = NULL
)
{
  assemblages <- road_get_assemblages(continent = continent,
                                          subcontinent = subcontinent,
                                          country = country,
                                          locality_type = locality_type,
                                          cultural_period = cultural_period,
                                          technocomplex = technocomplex,
                                          category = category,
                                          age_min = age_min,
                                          age_max = age_max)

  assemblages_selected <- select(assemblages, dplyr::all_of(c(cm_locality_idlocality,
                                                cm_geopolitical_units_continent,
                                                cm_geopolitical_units_continent_region,
                                                cm_locality_country,
                                                cm_locality_type, cm_locality_x,
                                                cm_locality_y, cm_cultural_period,
                                                cm_technocomplex, cm_assemblages_category,
                                                cm_geological_stratigraphy_age_min,
                                                cm_geological_stratigraphy_age_max)))


  if (!is.null(assemblages) && nrow(assemblages) != 0)
  {

    assemblages_all_info <- road_get_assemblages(continent = continent,
                                                 subcontinent = subcontinent,
                                                 country = country,
                                                 locality_type = locality_type)
    assemblages_ages <- assemblages_all_info %>% dplyr::select(dplyr::all_of(c(cm_locality_idlocality,
                                                          cm_geological_stratigraphy_age_min,
                                                          cm_geological_stratigraphy_age_max)))

    #assemblages_ages <- select(assemblages_all_info, c(cm_locality_idlocality,
    #                                                      cm_geological_stratigraphy_age_min,
    #                                                      cm_geological_stratigraphy_age_max))

    #ages_min_max <- assemblages_ages %>% group_by(locality_id) %>% summarise(locality_age_min = min(age_min),
    #                                                           locality_age_max = max(age_max))

    ages_min_max <- dplyr::summarise(dplyr::group_by(assemblages_ages, .data$locality_id), locality_age_min = min(age_min),
                              locality_age_max = max(age_max))


    assemblages_selected <- dplyr::select(assemblages, dplyr::all_of(c(cm_locality_idlocality,
                                                  cm_geopolitical_units_continent,
                                                  cm_geopolitical_units_continent_region,
                                                  cm_locality_country,
                                                  cm_locality_type, cm_locality_x,
                                                  cm_locality_y, cm_cultural_period,
                                                  cm_technocomplex, cm_assemblages_category,
                                                  cm_geological_stratigraphy_age_min,
                                                  cm_geological_stratigraphy_age_max)))

    # data_tmp <- assemblages_selected %>% group_by(c(cm_locality_idlocality,
    #                                                 cm_geopolitical_units_continent,
    #                                                 cm_geopolitical_units_continent_region,
    #                                                 cm_locality_country,
    #                                                 cm_locality_type, cm_locality_x,
    #                                                 cm_locality_y)) %>%
    #                 summarise(category = well_formed_string_to_string_without_duplicates(paste0(category, collapse = ", ")),
    #                 cultural_period = well_formed_string_to_string_without_duplicates(paste0(cultural_period, collapse = ", ")),
    #                 technocomplex = well_formed_string_to_string_without_duplicates(paste0(technocomplex, collapse = ", ")),
    #                 subset_age_min = min(age_min),
    #                 subset_age_max = max(age_max))

    .data <- c()
    data_tmp <- assemblages_selected %>% dplyr::group_by(.data$locality_id, .data$continent, .data$subcontinent,
                                                  .data$country, .data$locality_type, .data$coord_x, .data$coord_y,
                                                  ) %>% dplyr::summarise(category = well_formed_string_to_string_without_duplicates(paste0(category, collapse = ", ")),
                                                                  cultural_period = well_formed_string_to_string_without_duplicates(paste0(cultural_period, collapse = ", ")),
                                                                  technocomplex = well_formed_string_to_string_without_duplicates(paste0(technocomplex, collapse = ", ")),
                                                                  subset_age_min = min(age_min),
                                                                  subset_age_max = max(age_max))

    # data_tmp <- dplyr::summarise(dplyr::group_by(assemblages_selected, .data$locality_id, .data$continent,
    #                                .data$subcontinent,
    #                                .data$country, .data$locality_type, .data$coord_x, .data$coord_y
    #                                 ), category = well_formed_string_to_string_without_duplicates(paste0(.data$category, collapse = ", ")),
    #                                    cultural_period = well_formed_string_to_string_without_duplicates(paste0(.data$cultural_period, collapse = ", ")),
    #                                    technocomplex = well_formed_string_to_string_without_duplicates(paste0(.data$technocomplex, collapse = ", ")),
    #                                    subset_age_min = min(.data$age_min),
    #                                    subset_age_max = max(.data$age_max))
    #
    #category <- subset(data_tmp, select = category)

    data <- dplyr::inner_join(data_tmp, ages_min_max, by = c(cm_locality_idlocality), #"locality_id"),
                      copy = FALSE, na_matches = "na")

    return(data)
  }
  else return(dplyr::select(assemblages,
                              dplyr::all_of(c(cm_locality_idlocality,
                                    cm_geopolitical_units_continent, 
                                    cm_geopolitical_units_continent_region, 
                                    cm_locality_country,
                                    cm_locality_type, cm_locality_x, 
                                    cm_locality_y, cm_cultural_period,
                                    cm_technocomplex, cm_assemblages_category
                                    ))))
}
