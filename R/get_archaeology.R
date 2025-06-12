#' Get lithic typology from ROAD database
#'
#' `road_get_lithic_typology` fetches data of lithic finds from ROAD database.
#'
#' Lithic typology refers to the classification of stone tools based on their shape, technology, and function.
#' This function enables you to query lithic typology data from the ROAD database using various parameters such as
#' geographical location, cultural periods, tool types, and assemblages. Use the parameters to filter the results
#' according to your research needs, or omit them to retrieve a broader dataset.
#'
#' @param continents specifies the continent(s) of the country/countries, e.g. Africa, Europe, Asia. The parameter continents is a string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param subcontinents specifies the continental region(s) of the country , e.g. Southern Europe. The parameter subcontinents is a string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param countries specifies the name of the country where a locality is situated (e.g.
#' Germany, Kenya, Saudi Arabia, China). The parameter countries is a string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param locality_types specifies the type of locality (e.g. cave, rockshelter, open air, profile, outcrop,
#' mine, quarry, boring). The parameter locality_types is a string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param cultural_periods specifies the main cultural epoch(s) and includes the Eurasian
#' Paleolithic (Lower, Middle, Upper, Epi) and the African Stone Age (Earlier, Middle, Later). The parameter cultural_periods is a string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param categories specifies the assemblage category/categories. For an assemblage consisting of human remains category includes the entry “human
#' remains”. In the case of archaeological assemblages, multiple categories are the norm and may
#' include “raw material, typology, technology, function, organic tools, symbolic artifacts, feature,
#' miscellaneous finds”. A faunal assemblage can also contain multiple entries including
#' “paleofauna, animal remains”, while a botanical assemblage can only include the entry “plant
#' remains”.The parameter categories is a string (one item) or vector of strings (one or more items).
#' @param age_min specifies the minimum age of assemblage. The parameter age_min is an integer.
#' @param age_max specifies the maximum age of assemblage. The parameter age_max is an integer.
#' @param assemblages list of assemblages; return value from function `road_get_assemblages`.
#' @param tool_list string (one item) or vector of strings
#'
#' @return Database search result as list of lithic finds with info about typology.
#' @export
#'
#' @examples
#' road_get_lithic_typology(continents = "Europe")
#' road_get_lithic_typology(continents = "Europe", tool_list = "flake")
road_get_lithic_typology <- function(
    continents = NULL,
    subcontinents = NULL,
    countries = NULL,
    locality_types = NULL,
    cultural_periods = NULL,
    categories = NULL,
    age_min = NULL,
    age_max = NULL,
    tool_list = NULL,
    assemblages = NULL
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

  data <- add_locality_columns(data, assemblages = assemblages)

  return(data)
}


#' Get lithic raw material from ROAD database
#'
#' `road_get_lithic_raw_material` fetches data of lithic finds from ROAD database.
#'
#' Lithic raw material refers to the types of stone used for tool production in archaeological contexts.
#' This function allows you to query lithic raw material data from the ROAD database using parameters such as
#' geographical location, cultural periods, raw material types, and assemblages. Use the parameters to filter
#' the results or omit them to retrieve a broader dataset.
#'
#' @param continents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param subcontinents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param countries string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param locality_types string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param cultural_periods string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param categories string (one item) or vector of strings (one or more items).
#' @param age_min integer; minimum age of assemblage.
#' @param age_max integer; maximum age of assemblage.
#' @param assemblages list of assemblages; return value from function `road_get_assemblages`.
#' @param raw_material_list string (one item) or vector of strings
#'
#' @return Database search result as list of lithic finds with info about raw material.
#' @export
#'
#' @examples
#' road_get_lithic_raw_material(continents = "Europe")
#' road_get_lithic_raw_material(continents = "Europe", raw_material_list = "flint")
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
    query_values_in_string("AND ", raw_material_list, "raw_material_list")
  )

  data <- road_run_query(query)

  data <- add_locality_columns(data, assemblages = assemblages)

  return(data)
}


#' Get organic tools from ROAD database
#'
#' `road_get_organic_tools` fetches data of organic tools from ROAD database.
#'
#' Organic tools are artifacts made from organic materials such as bone, antler, or wood, found in archaeological contexts.
#' This function enables you to query organic tool data from the ROAD database based on parameters like geographical location,
#' cultural periods, tool interpretation, and assemblages. Use the parameters to filter the results or omit them for broader results.
#'
#' @param continents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param subcontinents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param countries string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param locality_types string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param cultural_periods string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param categories string (one item) or vector of strings (one or more items).
#' @param age_min integer; minimum age of assemblage.
#' @param age_max integer; maximum age of assemblage.
#' @param assemblages list of assemblages; return value from function `road_get_assemblages`.
#' @param organic_tools_interpretation string (one item) or vector of strings
#'
#' @return Database search result as list of organic tools.
#' @export
#'
#' @examples
#' road_get_organic_tools(continents = "Europe")
#' road_get_organic_tools(continents = "Europe", organic_tools_interpretation = "abrader/polisher")
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

  data <- add_locality_columns(data, assemblages = assemblages)

  return(data)
}


#' Get symbolic artifacts from ROAD database
#'
#' `road_get_symbolic_artifacts` fetches data of symbolic artifacts from ROAD database.
#'
#' Symbolic artifacts are objects interpreted as having symbolic or cultural significance in archaeological contexts.
#' This function allows you to query symbolic artifact data from the ROAD database using parameters such as geographical location,
#' cultural periods, artifact interpretation, and assemblages. Use the parameters to filter the results or omit them to retrieve a broader dataset.
#'
#' @param continents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param subcontinents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param countries string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param locality_types string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param cultural_periods string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param categories string (one item) or vector of strings (one or more items).
#' @param age_min integer; minimum age of assemblage.
#' @param age_max integer; maximum age of assemblage.
#' @param assemblages list of assemblages; return value from function `road_get_assemblages`.
#' @param symbolic_artifacts_interpretation string (one item) or vector of strings
#'
#' @return Database search result as list of symbolic artifacts with info about symbolic.
#' @export
#'
#' @examples
#' road_get_symbolic_artifacts(continents = "Europe")
#' road_get_symbolic_artifacts(continents = "Europe", symbolic_artifacts_interpretation = "abstract")
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
  if (is.null(assemblages)) assemblages <- road_get_assemblages(continents, subcontinents, countries, locality_types, cultural_periods, categories, age_min, age_max)
  assemblage_condition <- get_assemblage_condition(assemblages = assemblages, locality_id_column_name = "symbolic_artifacts.assemblage_idlocality", assemblage_id_column_name = "symbolic_artifacts.assemblage_idassemblage")

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

  data <- add_locality_columns(data, assemblages = assemblages)

  return(data)
}


#' Get feature assemblages from ROAD database
#'
#' `road_get_feature` fetches data of feature finds from ROAD database.
#'
#' Feature assemblages refer to archaeological features such as hearths, pits, or structures found at a site.
#' This function enables you to query feature data from the ROAD database using parameters like geographical location,
#' cultural periods, feature interpretation, and assemblages. Use the parameters to filter the results or omit them for broader results.
#'
#' @param continents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param subcontinents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param countries string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param locality_types string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param cultural_periods string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param categories string (one item) or vector of strings (one or more items).
#' @param age_min integer; minimum age of assemblage.
#' @param age_max integer; maximum age of assemblage.
#' @param assemblages list of assemblages; return value from function `road_get_assemblages`.
#' @param feature_interpretation string (one item) or vector of strings
#'
#' @return Database search result as list of feature finds.
#' @export
#'
#' @examples
#' road_get_feature(continents = "Europe")
#' road_get_feature(continents = "Europe", feature_interpretation = "bedding")
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

  data <- add_locality_columns(data, assemblages = assemblages)

  return(data)
}


#' Get miscellaneous finds from ROAD database
#'
#' `road_get_miscellaneous_finds` fetches data of feature finds from ROAD database.
#'
#' Miscellaneous finds are archaeological objects that do not fit into other specific categories.
#' This function allows you to query miscellaneous finds data from the ROAD database using parameters such as geographical location,
#' cultural periods, material types, and assemblages. Use the parameters to filter the results or omit them to retrieve a broader dataset.
#'
#' @param continents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param subcontinents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param countries string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param locality_types string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param cultural_periods string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param categories string (one item) or vector of strings (one or more items).
#' @param age_min integer; minimum age of assemblage.
#' @param age_max integer; maximum age of assemblage.
#' @param assemblages list of assemblages; return value from function `road_get_assemblages`.
#' @param miscellaneous_finds_material string (one item) or vector of strings
#'
#' @return Database search result as list of miscellaneous finds.
#' @export
#'
#' @examples
#' road_get_miscellaneous_finds(continents = "Europe")
#' road_get_miscellaneous_finds(continents = "Europe", miscellaneous_finds_material = "wood fossil")
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

  data <- add_locality_columns(data, assemblages = assemblages)

  return(data)
}
