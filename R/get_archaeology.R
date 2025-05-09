#' Get lithic typology from ROAD database
#'
#' `road_get_lithic_typology` fetches data of lithic finds from ROAD database.
#'
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
#' @param tool_list string (one item) or vector of strings
#'
#' @return Database search result as list of lithic finds with info about typology.
#' @export
#'
#' @examples 
#' road_get_lithic_typology(continents = "Europe")
#' road_get_lithic_typology(continents = "Europe", tool_list = "biface")
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
