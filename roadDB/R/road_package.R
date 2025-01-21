#source("./roadDB/R/login.R")
library(assertthat)
library(RPostgres)

# column names
cm_locality_idlocality <- "locality_id"
cm_locality_types <- "locality_types"
cm_geopolitical_units_continent <- "continent"
cm_geopolitical_units_continent_region <- "subcontinent"
cm_locality_country <- "country"
cm_locality_x <- "coord_x"
cm_locality_y <- "coord_y"
cm_assemblages_locality_idlocality <- "locality_id"
cm_assemblages_idassemblage <- "assemblage_id"
cm_assemblages_name <- "name"
cm_assemblages_categories <- "categories"
cm_geological_stratigraphy_age_min <- "age_min"
cm_geological_stratigraphy_age_max <- "age_max"
cm_assemblage_in_geolayer_geolayer_name <- "geolayers"
cm_age <- "age"
cm_negative_standard_deviation <- "negative_standard_deviation"
cm_positive_standard_deviation <- "positive_standard_deviation"
cm_material_dated <- "material_dated"
cm_dating_method <- "dating_method"
cm_laboratory_idlaboratory <- "laboratory_idlaboratory"

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
#' @examples road_get_localities()
#' @examples road_get_localities(continents = c("Europe"), countries = c("Germany", "France"))
#' @examples road_get_localities(continents = "Europe", countries = c("Germany", "France"))
#' @examples road_get_localities(countries = c("Germany", "France"), locality_type = "cave")
#' @examples road_get_localities(NULL, NULL, "Germany")
#' @examples road_get_localities(countries = c("Germany", "France"), cultural_periods = "Middle Paleolithic")
road_get_localities <- function(continents = NULL, subcontinents = NULL, countries = NULL, locality_types = NULL, cultural_periods = NULL)
{
  # select fields
  select_fields <- c(
    paste0("locality.idlocality AS \"", cm_locality_idlocality, "\""),
    paste0("locality.type AS \"", cm_locality_types, "\""),
    paste0("geopolitical_units.continent AS \"", cm_geopolitical_units_continent, "\""),
    paste0("geopolitical_units.continent_region AS \"", cm_geopolitical_units_continent_region, "\""),
    paste0("locality.country AS \"", cm_locality_country, "\""),
    paste0("locality.x AS \"", cm_locality_x, "\""),
    paste0("locality.y AS \"", cm_locality_y, "\"")
  )

  # cultural periods
  query_additional_joins <- ""
  query_additional_where_clauses <- ""
  if (!is.null(cultural_periods))
  {
    query_additional_joins <- paste(
      "INNER JOIN archaeological_layer ON locality.idlocality = archaeological_layer.locality_idlocality",
      "INNER JOIN archaeological_stratigraphy ON archaeological_layer.archstratigraphy_idarchstrat = archaeological_stratigraphy.idarchstrat"
    )
    query_additional_where_clauses <- parameter_to_query("AND archaeological_stratigraphy.cultural_period IN (", cultural_periods, ")")
  }

  # order by
  query_order_by <- ""
  if (!is.null(countries))
  {
    query_order_by <- "ORDER BY locality.idlocality"
  }

  # combine query parts
  query <- paste(
    "SELECT DISTINCT",
    paste(select_fields, collapse = ", "),
    "FROM locality",
    "INNER JOIN geopolitical_units ON locality.country = geopolitical_units.geopolitical_name",
    query_additional_joins,
    "WHERE NOT locality.no_data_entry AND geopolitical_units.rank = 1",
    parameter_to_query("AND geopolitical_units.continent IN (", continents, ")"),
    parameter_to_query("AND geopolitical_units.continent_region IN (", subcontinents, ")"),
    parameter_to_query("AND locality.country IN (", countries, ")"),
    parameter_to_query("AND string_to_array(locality.type, ', ') && array[", locality_types, "]"),
    query_additional_where_clauses,
    query_order_by
  )
  
  #message(query)

  data <- road_run_query(query)

  return(data)
}



#' Get assemblages from ROAD database
#'
#' `road_get_assemblages` fetches data of archeological assembalges from ROAD database.
#'
#' Assembalges are articulated archeological finds inside in a locality. One locality
#' can host multiple assemblages which can for example be associated with certain
#' geological layers or historical time periods.
#' This function uses a list of localities to get assemblages that were found in these localities.
#' To preselect these localities the same parameters as in `road_get_localities` can be used.
#' Alternatively, if you run `road_get_localities` independently, you can pass its return value
#' (list of localities) to this function. This will overwrite any argumnets passed to the localities
#' parameters in this function (continents, subcontinents, countries, locality_types, cultural_periods).
#' Use parameters to further narrow down the assemblages you are searching for.
#' Excluding `localities` all parameters are optional and should be omitted or
#' set to NULL when not used.
#'
#' @param continents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param subcontinents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param countries string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param locality_types string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param cultural_periods string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param categories string (one item) or vector of strings (one or more items).
#' @param age_min integer; minimum age of assemblage.
#' @param age_max integer; maximum age of assemblage.
#' @param localities list of localities; return value from function `road_get_localities`.
#'
#' @return Database search result as list of assemblages.
#' @export
#'
#' @examples road_get_assemblages(localities = road_get_localities())
#' @examples road_get_assemblages(localities, NULL, 80000L, 120000L)
#' @examples road_get_assemblages(localities = localities, categories = "human remains", age_max = 100000L)
road_get_assemblages <- function(continents = NULL, subcontinents = NULL, countries = NULL, locality_types = NULL, cultural_periods = NULL, categories = NULL, age_min = NULL, age_max = NULL, localities = NULL)
{
  if ((!is.null(age_min) && !is.integer(age_min)) || (!is.null(age_max) && !is.integer(age_max)))
    stop("Parameters 'min_age' and 'max_age' have to be integers.")

  if (!is.null(age_min) && !is.null(age_max) && age_min > age_max)
    stop("Parameter 'min_age' can not be bigger than 'max_age'.")

  if (is.null(localities))
  {
    # run `road_get_localities` else preselected list of localities is used
     localities <- road_get_localities(continents, subcontinents, countries, locality_types, cultural_periods)
  }
  localities <- localities[cm_locality_idlocality]
  query_localities <- paste(
    sapply(localities, function(x) paste0("'", x, "'")),
    collapse = ", "
  )

  # select fields
  select_fields <- c(
    paste0("assemblage.locality_idlocality AS \"", cm_assemblages_locality_idlocality, "\""),
    paste0("assemblage.idassemblage AS \"", cm_assemblages_idassemblage, "\""),
    paste0("assemblage.name AS \"", cm_assemblages_name, "\""),
    paste0("assemblage.category AS \"", cm_assemblages_categories, "\""),
    paste0("MIN(geological_stratigraphy.age_min) AS \"", cm_geological_stratigraphy_age_min, "\""),
    paste0("MAX(geological_stratigraphy.age_max) AS \"", cm_geological_stratigraphy_age_max, "\""),
    paste0("STRING_AGG(DISTINCT assemblage_in_geolayer.geolayer_name, ', ') AS \"", cm_assemblage_in_geolayer_geolayer_name, "\""),
    "CASE
      WHEN (assemblage.locality_idlocality, assemblage.idassemblage) in (select assemblage_idlocality, assemblage_idassemblage from humanremains) THEN true
      ELSE false
    END as humanremains,
    CASE
      WHEN category LIKE '%paleofauna%' THEN true
      ELSE false
    END as paleofauna,
    CASE
      WHEN category ~ 'raw material|symbolic artifacts|technology|typology|miscellaneous finds|feature|organic tools|function' THEN true
      ELSE false
    END as archaeology,
    CASE
      WHEN category LIKE '%plant remains%' THEN true
      ELSE false
    END as plantremains"
  )

  # combine query parts
  query <- paste(
    "SELECT DISTINCT",
    paste(select_fields, collapse = ", "),
    "FROM assemblage",
    "LEFT JOIN assemblage_in_geolayer ON",
      "assemblage_in_geolayer.assemblage_idlocality = assemblage.locality_idlocality",
      "AND assemblage_in_geolayer.assemblage_idassemblage = assemblage.idassemblage",
    "LEFT JOIN geostrat_desc_geolayer ON geostrat_desc_geolayer.geolayer_idlocality = assemblage.locality_idlocality",
      "AND assemblage_in_geolayer.geolayer_name = geostrat_desc_geolayer.geolayer_name",
    "LEFT JOIN geological_stratigraphy ON geological_stratigraphy.idgeostrat = geostrat_desc_geolayer.geostrat_idgeostrat",
    "WHERE assemblage.locality_idlocality IN (",
    query_localities,
    ")",
    query_check_intersection("AND ", categories, "assemblage.category"),
    parameter_to_query("AND ", age_min, " <= age_max"),
    parameter_to_query("AND ", age_max, " >= age_min"),
    "GROUP BY assemblage.locality_idlocality, assemblage.idassemblage, assemblage.name, assemblage.category",
    "ORDER BY assemblage.locality_idlocality ASC"
  )

  data <- road_run_query(query)

  return(data)
}



#' Get human remains from ROAD database
#'
#' `road_get_human_remains` fetches data of human remains from ROAD database.
#'
#' Human remains are always part of an assemblage which means the function needs a list of
#' assemblages (return value of function `road_get_assemblages`) as its first parameter.
#' The parameter `genus_species` can't be used in combination with `genus' or `species`. Use this function
#' in one of the two modes depending on which parameters you use:
#' Mode 1: either one or both of `genus` and `species` is used (not NULL), then `genus_species` can't be used and has to be set to NULL.
#' Mode 2: `genus_species` is used (not NULL), then `genus` and `species` can't be used and have to be set to NULL.
#'
#' @param assemblages list of assemblages; return value from function `road_get_assemblages`.
#' @param genus string (one item) or vector of strings (one or more items); can not be used in combination with `genus_species`.
#' @param species string (one item) or vector of strings (one or more items); can not be used in combination with `genus_species`.
#' @param genus_species string (one item) or vector of strings (one or more items); can not be used in combination with `genus` or `species`.
#'
#' @return Database search result as list of human remains.
#' @export
#'
#' @examples road_get_human_remains(assemblages = assemblages, genus = 'Homo', species = 'neanderthalensis')
#' @examples road_get_human_remains(assemblages = assemblages, genus = 'Homo')
#' @examples road_get_human_remains(assemblages = assemblages, genus_species = 'Homo neanderthalensis')
road_get_human_remains <- function(assemblages, genus = NULL, species = NULL, genus_species = NULL)
{
  if (!is.null(genus_species) && (!is.null(genus) || !is.null(species)))
    stop("Parameter 'genus_species' can't be used in combination with 'genus' or 'species'.")

  # get preselected list of localities and list of locality/assemblage strings
  locality_list <- paste(
    sapply(assemblages["locality_idlocality"], function(x) paste0("'", x, "'")),
    collapse = ", "
  )
  locality_assemblage_list <- paste(
    sapply(assemblages["locality_idlocality"], function(x) paste0("'", x, ", ")),
    sapply(assemblages["idassemblage"], function(x) paste0(x, "'")),
    collapse = ", "
  )

  # build genus/species selection
  selection_query = ""
  if (!is.null(genus_species))
  {
    selection_query <- parameter_to_query("AND genus_species_str IN (", genus_species, ")")
  }
  else
  {
    species_conjucton <- "AND"
    if (!is.null(genus))
    {
      selection_query <- parameter_to_query("AND genus IN (", genus, ")")
      species_conjucton <- "OR"
    }
    if (!is.null(species))
    {
      selection_query <- paste(
        selection_query,
        species_conjucton,
        parameter_to_query("species IN (", species, ")")
      )
    }
  }

  # combine query parts
  query <- paste(
    "SELECT DISTINCT * FROM (SELECT humanremains_idlocality || ', ' || humanremains_idassemblage as locality_assemblage_str, genus || ' ' || species as genus_species_str, genus, species, age, sex, humanremains_idhumanremains FROM publication_desc_humanremains WHERE humanremains_idlocality IN (",
    locality_list,
    ") ) as foo WHERE locality_assemblage_str IN (",
    locality_assemblage_list,
    ")",
    selection_query,
    "ORDER BY locality_assemblage_str"
  )

  data <- road_run_query(query)

  return(data)
}



# run query in ROAD db
road_run_query <- function(query)
{
  query <- trimws(query)

  if (query == "") {
    stop("Query can not be empty.")
  }

  #con <- dbConnect(RPostgres::Postgres(), dbname = "roceeh", host="134.2.216.14", port=5432, user=rstudioapi::askForPassword("Database username"), password=rstudioapi::askForPassword("Database password"))
  con <- dbConnect(RPostgres::Postgres(), dbname = "roceeh", host = "134.2.216.14", port = 5432, user = user_name, password = user_password)

  # run query
  result <- dbGetQuery(con, query)

  return(result)
}



# convert string parameter to vector
parameter_to_query <- function(query_start, parameter, query_end)
{
  query <- ""
  if (!is.null(parameter))
  {
    parameter <- parameter_to_vector(parameter)

    if (is.vector(parameter))
    {
      query <- paste0(
        query_start,
        paste(
          sapply(parameter, function(x) paste0("'", x, "'")),
          collapse = ", "
        ),
        query_end
      )
    }
    else
      stop(paste("Wrong input for '", deparse(substitute(parameter)), "'."))
  }

  return(query)
}

# build query to check if parameters intersect with comma seperated database values
query_check_intersection <- function(query_start, parameter, column)
{
  query <- ""
  if (!is.null(parameter))
  {
    parameter <- parameter_to_vector(parameter)

    if (is.vector(parameter))
    {
      query <- paste(
        sapply(parameter, function(x) paste0("OR '", x, "' = ANY(STRING_TO_ARRAY(", column, ", ', '))")),
        collapse = " "
      )
      query <- paste0(
        query_start,
        "(",
        sub("OR ", "", query),
        ")"
      )
    }
    else
      stop(paste("Wrong input for '", deparse(substitute(parameter)), "'."))
  }

  return(query)
}

# convert non-vector parameter to vector
parameter_to_vector <- function(parameter)
{
  # convert string to vector
  if (is.string(parameter) && parameter != "")
    parameter <- c(parameter)

  # convert integer to vector
  if (is.integer(parameter) && parameter != 0)
    parameter <- c(parameter)

  return(parameter)
}
