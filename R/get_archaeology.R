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
# @examples road_get_lithic_typology(continents = "Europe")
# @examples road_get_lithic_typology(continents = "Europe", tool_list = "biface")
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
  if (is.null(assemblages)) assemblages <- road_get_assemblages(continents = continents,
                                                                subcontinents = subcontinents,
                                                                countries = countries,
                                                                categories = categories,
                                                                locality_types = locality_types,
                                                                cultural_periods = cultural_periods,
                                                                age_min = age_min, age_max = age_max)

  assemblage_condition <- get_assemblage_condition(query_start = "AND ", assemblages = assemblages)

  # select fields
  select_fields <- c(
    paste0(" assemblage_idlocality AS ", cm_locality_idlocality),
    paste0(" assemblage_idassemblage AS ", cm_assemblages_idassemblage),
    paste0(" tool_list AS ", cm_tool_list),
    paste0(" typology AS ", cm_typology),
    "percentage",
    "comments"
  )

  if (!is.null(tool_list))
    tool_list_condition <- query_check_intersection("AND ", tool_list, cm_tool_list)
  else
    tool_list_condition <- ""

  query <- paste(
    "SELECT DISTINCT * FROM (",
    "SELECT DISTINCT ",
    paste(select_fields, collapse = ", "),
    " FROM typology) as foo WHERE true ",
    assemblage_condition,
    tool_list_condition
  )

  data <- road_run_query(query)

  data <- add_locality_columns(data, assemblages = assemblages)

  return(data)
}


#' Get lithic raw material from ROAD database
#'
#' `road_get_lithic_raw_material` fetches data of lithic finds from ROAD database.
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
#' @param raw_material_list string (one item) or vector of strings
#'
#' @return Database search result as list of lithic finds with info about raw material.
#' @export
#'
# @examples road_get_lithic_raw_material(continents = "Europe")
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
  if (is.null(assemblages)) assemblages <- road_get_assemblages(continents = continents,
                                                                subcontinents = subcontinents,
                                                                countries = countries,
                                                                categories = categories,
                                                                locality_types = locality_types,
                                                                cultural_periods = cultural_periods,
                                                                age_min = age_min, age_max = age_max)
  #localities = localities)
  assemblage_condition <- get_assemblage_condition(query_start = "AND ", assemblages = assemblages)

  # select fields
  select_fields <- c(
    paste0(" assemblage_idlocality AS ", cm_locality_idlocality),
    paste0(" assemblage_idassemblage AS ", cm_assemblages_idassemblage),
    paste0(" raw_material_list AS ", cm_raw_material_list),
    paste0(" transport_distance AS ", cm_transport_distance),
    "percentage",
    "comments"
  )

  if (!is.null(raw_material_list))
    raw_material_condition <- query_check_intersection("AND ", raw_material_list, cm_raw_material_list)
  else
    raw_material_condition <- ""

  query <- paste(
    "SELECT DISTINCT * FROM (",
    "SELECT DISTINCT ",
    paste(select_fields, collapse = ", "),
    " FROM raw_material) as foo WHERE true ",
    assemblage_condition,
    raw_material_condition
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
# @examples road_get_organic_tools(continents = "Europe")
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
  if (is.null(assemblages)) assemblages <- road_get_assemblages(continents = continents,
                                                                subcontinents = subcontinents,
                                                                countries = countries,
                                                                categories = categories,
                                                                locality_types = locality_types,
                                                                cultural_periods = cultural_periods,
                                                                age_min = age_min,
                                                                age_max = age_max)

  assemblage_condition <- get_assemblage_condition(query_start = "AND ", assemblages = assemblages)

  # select fields
  select_fields <- c(
    paste0(" assemblage_idlocality AS ", cm_locality_idlocality),
    paste0(" assemblage_idassemblage AS ", cm_assemblages_idassemblage),
    paste0(" interpretation AS ", cm_organic_tools_interpretation),
    paste0(" organic_raw_material AS ", cm_organic_raw_material),
    paste0(" technology AS ", cm_organic_tools_technology),
    "number",
    "comments"
  )

  if (!is.null(organic_tools_interpretation))
    organic_tools_interpretation_condition <- query_check_intersection("AND ",
                                                                       organic_tools_interpretation,
                                                                       cm_organic_tools_interpretation)
  else
    organic_tools_interpretation_condition <- ""

  query <- paste(
    "SELECT DISTINCT * FROM (",
    "SELECT DISTINCT ",
    paste(select_fields, collapse = ", "),
    " FROM organic_tools) as foo WHERE true ",
    assemblage_condition,
    organic_tools_interpretation_condition
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
# @examples road_get_symbolic_artifacts(continents = "Europe")
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
  if (is.null(assemblages)) assemblages <- road_get_assemblages(continents = continents,
                                                                subcontinents = subcontinents,
                                                                countries = countries,
                                                                categories = categories,
                                                                locality_types = locality_types,
                                                                cultural_periods = cultural_periods,
                                                                age_min = age_min,
                                                                age_max = age_max)

  assemblage_condition <- get_assemblage_condition(query_start = "AND ", assemblages = assemblages)

  # select fields
  select_fields <- c(
    paste0(" assemblage_idlocality AS ", cm_locality_idlocality),
    paste0(" assemblage_idassemblage AS ", cm_assemblages_idassemblage),
    paste0(" interpretation AS ", cm_symbolic_artifacts_interpretation),
    paste0(" category AS ", cm_symbolic_artifacts_category),
    paste0(" material AS ", cm_symbolic_artifacts_material),
    paste0(" technology AS ", cm_symbolic_artifacts_technology),
    paste0(" raw_material_source AS ", cm_symbolic_artifacts_raw_material_source),
    "comments"
  )

  if (!is.null(symbolic_artifacts_interpretation))
    symbolic_artifacts_interpretation_condition <- query_check_intersection("AND ",
                                                                            symbolic_artifacts_interpretation,
                                                                            cm_symbolic_artifacts_interpretation)
  else
    symbolic_artifacts_interpretation_condition <- ""

  query <- paste(
    "SELECT DISTINCT * FROM (",
    "SELECT DISTINCT ",
    paste(select_fields, collapse = ", "),
    " FROM symbolic_artifacts) as foo WHERE true ",
    assemblage_condition,
    symbolic_artifacts_interpretation_condition
  )

  # message(query)
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
# @examples road_get_feature(continents = "Europe")
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
  if (is.null(assemblages)) assemblages <- road_get_assemblages(continents = continents,
                                                                subcontinents = subcontinents,
                                                                countries = countries,
                                                                categories = categories,
                                                                locality_types = locality_types,
                                                                cultural_periods = cultural_periods,
                                                                age_min = age_min, age_max = age_max)

  assemblage_condition <- get_assemblage_condition(query_start = " AND ", assemblages = assemblages)

  # select fields
  select_fields <- c(
    paste0(" assemblage_idlocality AS ", cm_locality_idlocality),
    paste0(" assemblage_idassemblage AS ", cm_assemblages_idassemblage),
    paste0(" interpretation AS ", cm_feature_interpretation),
    "comments"
  )

  if (!is.null(feature_interpretation))
    feature_interpretation_condition <- query_check_intersection("AND ",
                                                                 feature_interpretation,
                                                                 cm_feature_interpretation)
  else
    feature_interpretation_condition <- ""

  query <- paste(
    "SELECT DISTINCT * FROM (",
    "SELECT DISTINCT ",
    paste(select_fields, collapse = ", "),
    " FROM feature) as foo WHERE true ",
    assemblage_condition,
    feature_interpretation_condition
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
# @examples road_get_miscellaneous_finds(continents = "Europe")
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
  if (is.null(assemblages)) assemblages <- road_get_assemblages(continents = continents,
                                                                subcontinents = subcontinents,
                                                                countries = countries,
                                                                categories = categories,
                                                                locality_types = locality_types,
                                                                cultural_periods = cultural_periods,
                                                                age_min = age_min, age_max = age_max)

  assemblage_condition <- get_assemblage_condition(query_start = "AND ", assemblages = assemblages)

  # select fields
  select_fields <- c(
    paste0(" assemblage_idlocality AS ", cm_locality_idlocality),
    paste0(" assemblage_idassemblage AS ", cm_assemblages_idassemblage),
    paste0(" material AS ", cm_miscellaneous_finds_material),
    paste0(" raw_material_source AS ", cm_miscellaneous_finds_raw_material_source),
    "number",
    "comments"
  )

  if (!is.null(miscellaneous_finds_material))
    miscellaneous_finds_material_condition <- query_check_intersection("AND ",
                                                                       miscellaneous_finds_material,
                                                                       cm_miscellaneous_finds_material)
  else
    miscellaneous_finds_material_condition <- ""

  query <- paste(
    "SELECT DISTINCT * FROM (",
    "SELECT DISTINCT ",
    paste(select_fields, collapse = ", "),
    " FROM miscellaneous_finds) as foo WHERE true ",
    assemblage_condition,
    miscellaneous_finds_material_condition
  )

  data <- road_run_query(query)

  data <- add_locality_columns(data, assemblages = assemblages)

  return(data)
}
