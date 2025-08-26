#' Get lithic typology from ROAD database
#'
#' The \strong{\code{road_get_lithic_typology}} fetches data of lithic finds 
#' from ROAD databaseLithic typology refers to the classification of stone tools 
#' based on their shape, technology, and function.This function enables you to 
#' query lithic typology data from the ROAD database using various parameters such as
#' geographical location, cultural periods, tool types, and assemblages. Use the 
#' parameters to filter the results according to your research needs, or omit them 
#' to retrieve a broader dataset.
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
#' @param technocomplex specifies an archaeological culture or named stone tool 
#' industry (e.g. Oldowan, Acheulean, Mousterian).
#' Run \code{road_list_argument_values("technocomplex")} to display possible values.
#' The argument \code{technocomplex} is a string (one item) or vector of strings 
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
#' @param assemblages list of assemblages; return value from function `road_get_assemblages`.
#' Can be used instead of the other locality and assemblage parameters to filter the results.
#' @param tool_list specifies values that can be entered for various tool types. 
#' Tool types can contain 1) chipped tool types like scraper end, scraper side, 
#' scraper, carinated, burin, handaxe, chopper, cleaver, point, point unifacial, 
#' segment, unknown; 2) non-chipped tool types like grindstone upper, hammerstone, 
#' anvil, retoucher; 3) non-tools like core, debitage, flake, point; 4) unknown 
#' like cobble, block, manuport. Run \code{road_list_argument_values("tool_list")} 
#' to display possible values. The argument \code{tool_list} is a string (one item) 
#' or vector of strings (one or more items); defaults to NULL.
#'
#' @return Database search result as a data frame with the information about 
#' lithic finds like their geographic information, cultural period, locality type, 
#' assemblage category, dating, typology, percentage, tool list.
#' @export
#'
#' @examples
#' road_get_lithic_typology(continents = c("Asia"), tool_list = "adze")
#' road_get_lithic_typology(subcontinents = "Eastern Europe", 
#'                          tool_list = c("bladelet burin spall"))
road_get_lithic_typology <- function(
    continents = NULL,
    subcontinents = NULL,
    countries = NULL,
    locality_types = NULL,
    cultural_periods = NULL,
    categories = NULL,
    age_min = NULL,
    age_max = NULL,
    assemblages = NULL, 
    tool_list = NULL
)
{
  # calculate assemblage_condition
  # To do: !is.null(categories) AND !is.null(assemblages)  ---> Warnung an den Benutzer
  if (is.null(assemblages)) assemblages <- road_get_assemblages(continents, subcontinents, countries, locality_types, cultural_periods, categories, age_min, age_max)
  assemblage_condition <- get_assemblage_condition(assemblages = assemblages, locality_id_column_name = "typology.assemblage_idlocality", assemblage_id_column_name = "typology.assemblage_idassemblage")

  # select fields
  select_fields <- c(
    paste0("assemblage_idlocality AS ", cm_locality_idlocality),
    paste0("assemblage_idassemblage AS ", cm_assemblages_idassemblage),
    paste0("tool_list AS ", cm_tool_list),
    paste0("typology AS ", cm_typology),
    "percentage",
    "comments"
  )

  query <- paste(
    "SELECT DISTINCT",
    paste(select_fields, collapse = ", "),
    "FROM typology",
    "WHERE",
    assemblage_condition,
    query_values_in_string("AND ", tool_list, "tool_list")
  )

  data <- road_run_query(query)
  
  if (nrow(data) == 0 && nrow(assemblages) > 0)
  {
    
    print_null_result_message(  continents,
                                subcontinents,
                                countries,
                                locality_types,
                                cultural_periods,
                                categories,
                                age_min,
                                age_max,
                                tool_list
    )
  }
  
  data <- add_locality_columns(data, assemblages = assemblages)

  return(data)
}


#' Get lithic raw material from ROAD database
#'
#' The \strong{\code{road_get_lithic_raw_material}} fetches data of lithic finds 
#' from ROAD database. Lithic raw material refers to the types of stone used for 
#' tool production in archaeological contexts. This function allows you to query 
#' lithic raw material data from the ROAD database using parameters such as 
#' geographical location, cultural periods, raw material types, and assemblages. 
#' Use the parameters to filter the results or omit them to retrieve a broader 
#' dataset.
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
#' @param technocomplex specifies an archaeological culture or named stone tool 
#' industry (e.g. Oldowan, Acheulean, Mousterian).
#' Run \code{road_list_argument_values("technocomplex")} to display possible values.
#' The argument \code{technocomplex} is a string (one item) or vector of strings 
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
#' @param assemblages list of assemblages; return value from function `road_get_assemblages`.
#' Can be used instead of the other locality and assemblage parameters to filter the results.
#' @param raw_material_list specifies lithic raw materials (e.g. quartz, chert, flint). 
#' Consider the function \code{road_get_organic_tools()} for non-lithic raw materials.
#' Run \code{road_list_argument_values("raw_material_list")} to display possible values.
#' The argument \code{raw_material_list} is a string (one item) or vector of strings 
#' (one or more items); defaults to NULL.
#' @param transport_distance specifies one or more of the five categories, each 
#' distinguished by specific intervals of transport for the raw materials present 
#' in an assemblage. When more than one type of transport distance is present, 
#' then each of the transport distances must be entered as a unique dataset. 
#' The five classes of transport distance are:
#' \itemize{
#'   \item local (0-5 km) 
#'   \item regional (6-20 km)
#'   \item supra-regional (21-100 km)
#'   \item distant (>100 km)
#'   \item unknown
#' }
#' Run \code{road_list_argument_values("transport_distance")} to display possible values.
#' The argument \code{transport_distance} is a string (one item) or vector of strings; 
#' defaults to NULL.
#'
#' @return Database search result as a data frame with the information about 
#' lithic finds like their geographic information,
#' cultural period, locality type, assemblage category, dating and info about raw material like
#' transport distance, percentage, raw material list.
#' @export
#'
#' @examples
#'
#' road_get_lithic_raw_material(subcontinents = "South Asia", raw_material_list = c("limestone"))
#' road_get_lithic_raw_material(subcontinents = c("Caucasus"), locality_types = "cave",
#'                              raw_material_list = c("chalcedony", "limestone"))
road_get_lithic_raw_material <- function(
    continents = NULL,
    subcontinents = NULL,
    countries = NULL,
    locality_types = NULL,
    cultural_periods = NULL,
    categories = NULL,
    age_min = NULL,
    age_max = NULL,
    raw_material_list = NULL,
    transport_distance = NULL,
    assemblages = NULL
)
{
  # calculate assemblage_condition
  # To do: !is.null(categories) AND !is.null(assemblages)  ---> Warnung an den Benutzer
  if (is.null(assemblages)) assemblages <- road_get_assemblages(continents, subcontinents, countries, locality_types, cultural_periods, categories, age_min, age_max)
  assemblage_condition <- get_assemblage_condition(assemblages = assemblages, locality_id_column_name = "raw_material.assemblage_idlocality", assemblage_id_column_name = "raw_material.assemblage_idassemblage")

  # select fields
  select_fields <- c(
    paste0("assemblage_idlocality AS ", cm_locality_idlocality),
    paste0("assemblage_idassemblage AS ", cm_assemblages_idassemblage),
    paste0("raw_material_list AS ", cm_raw_material_list),
    paste0("transport_distance AS ", cm_transport_distance),
    "percentage",
    "comments"
  )

  query <- paste(
    "SELECT DISTINCT",
    paste(select_fields, collapse = ", "),
    "FROM raw_material",
    "WHERE",
    assemblage_condition,
    query_values_in_string("AND ", raw_material_list, "raw_material_list"),
    parameter_to_query("AND transport_distance IN (", transport_distance, ")")
    # query_values_in_string("AND ", transport_distance, "transport_distance")
  )

  data <- road_run_query(query)
  
  if (nrow(data) == 0 && nrow(assemblages) > 0)
  {
    
    print_null_result_message(  continents,
                                subcontinents,
                                countries,
                                locality_types,
                                cultural_periods,
                                categories,
                                age_min,
                                age_max,
                                raw_material_list,
                                transport_distance
    )
  }
  
  data <- add_locality_columns(data, assemblages = assemblages)

  return(data)
}


#' Get organic tools from ROAD database
#'
#' The \strong{\code{road_get_organic_tools}} fetches data of organic tools from 
#' ROAD database.Organic tools are artifacts made from organic materials such as 
#' bone, antler, or wood, found in archaeological contexts. This function enables 
#' you to query organic tool data from the ROAD database based on parameters like 
#' geographical location, cultural periods, tool interpretation, and assemblages. 
#' Use the parameters to filter the results or omit them for broader results.
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
#' @param technocomplex specifies an archaeological culture or named stone tool 
#' industry (e.g. Oldowan, Acheulean, Mousterian).
#' Run \code{road_list_argument_values("technocomplex")} to display possible values.
#' The argument \code{technocomplex} is a string (one item) or vector of strings 
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
#' @param assemblages list of assemblages; return value from function `road_get_assemblages`.
#' Can be used instead of the other locality and assemblage parameters to filter the results.
#' @param organic_tools_interpretation specifies interpreted organic tool types 
#' (e.g. lance/spear, point, retoucher). 
#' Run \code{road_list_argument_values("organic_tools_interpretation")} 
#' to display possible values. The argument \code{organic_tools_interpretation} 
#' is a string (one item) or vector of strings; defaults to NULL.
#'
#' @return Database search result as a data frame with the information about 
#' organic tools like their geographic information,
#' cultural period, locality type, category, dating, information about
#' organic tools interpretation, organic raw material, organic tools technology and number.
#' @export
#'
#' @examples
#' road_get_organic_tools(continents = c("Europe"), organic_tools_interpretation = "fishhook")
#' road_get_organic_tools(continents = "Africa", organic_tools_interpretation = c("fishhook"))
road_get_organic_tools <- function(
    continents = NULL,
    subcontinents = NULL,
    countries = NULL,
    locality_types = NULL,
    cultural_periods = NULL,
    categories = NULL,
    age_min = NULL,
    age_max = NULL,
    organic_tools_interpretation = NULL,
    assemblages = NULL
)
{

  # calculate assemblage_condition
  # To do: !is.null(categories) AND !is.null(assemblages)  ---> Warnung an den Benutzer
  if (is.null(assemblages)) assemblages <- road_get_assemblages(continents, subcontinents, countries, locality_types, cultural_periods, categories, age_min, age_max)
  assemblage_condition <- get_assemblage_condition(assemblages = assemblages, locality_id_column_name = "organic_tools.assemblage_idlocality", assemblage_id_column_name = "organic_tools.assemblage_idassemblage")

  # select fields
  select_fields <- c(
    paste0("assemblage_idlocality AS ", cm_locality_idlocality),
    paste0("assemblage_idassemblage AS ", cm_assemblages_idassemblage),
    paste0("interpretation AS ", cm_organic_tools_interpretation),
    paste0("organic_raw_material AS ", cm_organic_raw_material),
    paste0("technology AS ", cm_organic_tools_technology),
    "number",
    "comments"
  )

  query <- paste(
    "SELECT DISTINCT",
    paste(select_fields, collapse = ", "),
    "FROM organic_tools",
    "WHERE",
    assemblage_condition,
    query_values_in_string("AND ", organic_tools_interpretation, "interpretation")
  )

  data <- road_run_query(query)
  
  if (nrow(data) == 0 && nrow(assemblages) > 0)
  {
    
    print_null_result_message(  continents,
                                subcontinents,
                                countries,
                                locality_types,
                                cultural_periods,
                                categories,
                                age_min,
                                age_max,
                                organic_tools_interpretation
    )
  }

  data <- add_locality_columns(data, assemblages = assemblages)

  return(data)
}


#' Get symbolic artifacts from ROAD database
#'
#' The \strong{\code{road_get_symbolic_artifacts}} fetches data of symbolic artifacts from ROAD database.
#' Symbolic artifacts are objects interpreted as having symbolic or cultural significance
#' in archaeological contexts. This function allows you to query symbolic artifact
#' data from the ROAD database using parameters such as geographical location,
#' cultural periods, artifact interpretation, and assemblages. Use the parameters
#' to filter the results or omit them to retrieve a broader dataset.
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
#' @param technocomplex specifies an archaeological culture or named stone tool 
#' industry (e.g. Oldowan, Acheulean, Mousterian).
#' Run \code{road_list_argument_values("technocomplex")} to display possible values.
#' The argument \code{technocomplex} is a string (one item) or vector of strings 
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
#' @param assemblages list of assemblages; return value from function `road_get_assemblages`.
#' Can be used instead of the other locality and assemblage parameters to filter the results.
#' @param symbolic_artifacts_interpretation specifies the interpretation of 
#' symbolic artifacts (e.g. abstract, anthropomorphic, zoomorphic, instrument, 
#' ornament). Run \code{road_list_argument_values("symbolic_artifacts_interpretation")} 
#' to display possible values. The argument \code{symbolic_artifacts_interpretation} 
#' is a string (one item) or vector of strings; defaults to NULL.
#'
#' @return Database search result as a data frame with the information about 
#' symbolic artifacts like their geographic information, cultural period, 
#' locality type, assemblage category, dating and info about symbolic artifacts 
#' interpretation, symbolic artifacts category, symbolic artifacts technology, 
#' symbolic artifacts material, symbolic artifacts raw material source.
#' @export
#'
#' @examples
#' road_get_symbolic_artifacts(symbolic_artifacts_interpretation = c("instrument"))
#' road_get_symbolic_artifacts(continents = "Africa", symbolic_artifacts_interpretation = "zoomorphic")
road_get_symbolic_artifacts <- function(
    continents = NULL,
    subcontinents = NULL,
    countries = NULL,
    locality_types = NULL,
    cultural_periods = NULL,
    categories = NULL,
    age_min = NULL,
    age_max = NULL,
    symbolic_artifacts_interpretation = NULL,
    assemblages = NULL
) {
  # calculate assemblage_condition
  # To do: !is.null(categories) AND !is.null(assemblages)  ---> Warnung an den Benutzer
  if (is.null(assemblages)) assemblages <- road_get_assemblages(continents, subcontinents, 
                                                                countries, locality_types, 
                                                                cultural_periods, categories, age_min, age_max)
  assemblage_condition <- get_assemblage_condition(assemblages = assemblages, 
                                                   locality_id_column_name = "symbolic_artifacts.assemblage_idlocality", 
                                                   assemblage_id_column_name = "symbolic_artifacts.assemblage_idassemblage")

  # select fields
  select_fields <- c(
    paste0("assemblage_idlocality AS ", cm_locality_idlocality),
    paste0("assemblage_idassemblage AS ", cm_assemblages_idassemblage),
    paste0("interpretation AS ", cm_symbolic_artifacts_interpretation),
    paste0("category AS ", cm_symbolic_artifacts_category),
    paste0("material AS ", cm_symbolic_artifacts_material),
    paste0("technology AS ", cm_symbolic_artifacts_technology),
    paste0("raw_material_source AS ", cm_symbolic_artifacts_raw_material_source),
    "comments"
  )

  query <- paste(
    "SELECT DISTINCT",
    paste(select_fields, collapse = ", "),
    "FROM symbolic_artifacts",
    "WHERE",
    assemblage_condition,
    query_values_in_string("AND ", symbolic_artifacts_interpretation, "interpretation")
  )

  data <- road_run_query(query)
  
  if (nrow(data) == 0 && nrow(assemblages) > 0)
  {
    
    print_null_result_message(  continents,
                                subcontinents,
                                countries,
                                locality_types,
                                cultural_periods,
                                categories,
                                age_min,
                                age_max,
                                symbolic_artifacts_interpretation
    )
  }

  data <- add_locality_columns(data, assemblages = assemblages)

  return(data)
}


#' Get feature assemblages from ROAD database
#'
#' The \strong{\code{road_get_feature}} fetches data of feature finds from ROAD database. Feature assemblages refer to archaeological features such as hearths, pits, or structures found at a site.
#' This function enables you to query feature data from the ROAD database using parameters like geographical location,
#' cultural periods, feature interpretation, and assemblages. Use the parameters to filter the results or omit them for broader results.
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
#' @param technocomplex specifies an archaeological culture or named stone tool 
#' industry (e.g. Oldowan, Acheulean, Mousterian).
#' Run \code{road_list_argument_values("technocomplex")} to display possible values.
#' The argument \code{technocomplex} is a string (one item) or vector of strings 
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
#' @param assemblages list of assemblages; return value from function `road_get_assemblages`.
#' Can be used instead of the other locality and assemblage parameters to filter the results.
#' @param feature_interpretation specifies archaeological features present in 
#' the archaeological assemblage (e.g. bedding, burial, butchering event).
#' Run \code{road_list_argument_values("feature_interpretation")} to display 
#' possible values. The argument \code{feature_interpretation} is a string 
#' (one item) or vector of strings; defaults to NULL.
#'
#' @return Database search result as a data frame with the information about feature finds 
#' like their geographic information, cultural period, locality type, assemblage category, 
#' dating and interpretations.
#' @export
#'
#' @examples
#' road_get_feature(continents = c("Europe", "Africa"), feature_interpretation = "textile imprints")
#' road_get_feature(continents = "Africa", locality_types = c("cave"),
#'                  feature_interpretation = "bedding")
road_get_feature <- function(
    continents = NULL,
    subcontinents = NULL,
    countries = NULL,
    locality_types = NULL,
    cultural_periods = NULL,
    categories = NULL,
    age_min = NULL,
    age_max = NULL,
    feature_interpretation = NULL,
    assemblages = NULL
) {
  # calculate assemblage_condition
  # To do: !is.null(categories) AND !is.null(assemblages)  ---> Warnung an den Benutzer
  if (is.null(assemblages)) assemblages <- road_get_assemblages(continents, subcontinents, countries, locality_types, cultural_periods, categories, age_min, age_max)
  assemblage_condition <- get_assemblage_condition(assemblages = assemblages, locality_id_column_name = "feature.assemblage_idlocality", assemblage_id_column_name = "feature.assemblage_idassemblage")

  # select fields
  select_fields <- c(
    paste0("assemblage_idlocality AS ", cm_locality_idlocality),
    paste0("assemblage_idassemblage AS ", cm_assemblages_idassemblage),
    paste0("interpretation AS ", cm_feature_interpretation),
    "comments"
  )

  query <- paste(
    "SELECT DISTINCT",
    paste(select_fields, collapse = ", "),
    "FROM feature",
    "WHERE",
    assemblage_condition,
    parameter_to_query("AND interpretation IN (", feature_interpretation, ")")
  )

  data <- road_run_query(query)
  
  if (nrow(data) == 0 && nrow(assemblages) > 0)
  {
    
    print_null_result_message(  continents,
                                subcontinents,
                                countries,
                                locality_types,
                                cultural_periods,
                                categories,
                                age_min,
                                age_max,
                                feature_interpretation
    )
  }
  
  data <- add_locality_columns(data, assemblages = assemblages)

  return(data)
}

#' Get miscellaneous finds from ROAD database
#'
#' The \strong{\code{road_get_miscellaneous_finds}} fetches data of feature finds from ROAD database.
#' Miscellaneous finds are archaeological objects that do not fit into other specific categories.
#' Miscellaneous finds are classified by their material.
#' This function allows you to query miscellaneous finds data from the ROAD database
#' using parameters such as geographical location,
#' cultural periods, material types, and assemblages. Use the parameters to filter the results
#' or omit them to retrieve a broader dataset.
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
#' @param technocomplex specifies an archaeological culture or named stone tool 
#' industry (e.g. Oldowan, Acheulean, Mousterian).
#' Run \code{road_list_argument_values("technocomplex")} to display possible values.
#' The argument \code{technocomplex} is a string (one item) or vector of strings 
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
#' @param assemblages list of assemblages; return value from function `road_get_assemblages`.
#' Can be used instead of the other locality and assemblage parameters to filter the results.
#' @param miscellaneous_finds_material specifies material of the miscellaneous 
#' finds (e.g. shell, ochre, ostrich eggshell).
#' Run \code{road_list_argument_values("miscellaneous_finds_material")} to 
#' display possible values. The argument \code{miscellaneous_finds_material} is 
#' a string (one item) or vector of strings; defaults to NULL.
#'
#' @return Database search result as a data frame with the information about 
#' miscellaneous finds like their geographic information,
#' cultural period, locality type, category, dating, miscellaneous finds material,
#' miscellaneous finds material source (local, regional, supra-regional, unknown) and number.
#' @export
#'
#' @examples
#' road_get_miscellaneous_finds(miscellaneous_finds_material = "wood fossil")
#' road_get_miscellaneous_finds(continents = c("Africa"), locality_types = "open air",
#'                              miscellaneous_finds_material = "shell")
road_get_miscellaneous_finds <- function(
    continents = NULL,
    subcontinents = NULL,
    countries = NULL,
    locality_types = NULL,
    cultural_periods = NULL,
    categories = NULL,
    age_min = NULL,
    age_max = NULL,
    miscellaneous_finds_material = NULL,
    assemblages = NULL
) {
  # calculate assemblage_condition
  # To do: !is.null(categories) AND !is.null(assemblages)  ---> Warnung an den Benutzer
  if (is.null(assemblages)) assemblages <- road_get_assemblages(continents, subcontinents, countries, locality_types, cultural_periods, categories, age_min, age_max)
  assemblage_condition <- get_assemblage_condition(assemblages = assemblages, locality_id_column_name = "miscellaneous_finds.assemblage_idlocality", assemblage_id_column_name = "miscellaneous_finds.assemblage_idassemblage")

  # select fields
  select_fields <- c(
    paste0("assemblage_idlocality AS ", cm_locality_idlocality),
    paste0("assemblage_idassemblage AS ", cm_assemblages_idassemblage),
    paste0("material AS ", cm_miscellaneous_finds_material),
    paste0("raw_material_source AS ", cm_miscellaneous_finds_raw_material_source),
    "number",
    "comments"
  )

  query <- paste(
    "SELECT DISTINCT",
    paste(select_fields, collapse = ", "),
    "FROM miscellaneous_finds",
    "WHERE",
    assemblage_condition,
    parameter_to_query("AND material IN (", miscellaneous_finds_material, ")")
  )

  data <- road_run_query(query)
  
  if (nrow(data) == 0 && nrow(assemblages) > 0)
  {
    
    print_null_result_message(  continents,
                                subcontinents,
                                countries,
                                locality_types,
                                cultural_periods,
                                categories,
                                age_min,
                                age_max,
                                miscellaneous_finds_material
    )
  }
  
  data <- add_locality_columns(data, assemblages = assemblages)

  return(data)
}
