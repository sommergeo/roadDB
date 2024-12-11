source("roadDB/R/login.R")
library(assertthat)
library(RPostgres)

# column names
cm_locality_idlocality <- "locality_id"
cm_locality_type <- "type"
cm_geopolitical_units_continent <- "continent"
cm_geopolitical_units_continent_region <- "subcontinent"
cm_locality_country <- "country"
cm_locality_x <- "coord_x"
cm_locality_y <- "coord_y"
cm_assemblages_locality_idlocality <- "locality_id"
cm_assemblages_idassemblage <- "assemblage_id"
cm_assemblages_name <- "name"
cm_assemblages_category <- "category"
cm_geological_stratigraphy_age_min <- "age_min"
cm_geological_stratigraphy_age_max <- "age_max"
cm_assemblage_in_geolayer_geolayer_name <- "geolayer"



#' Get localities from ROAD Database
#'
#' `road_get_localities` fetches data of archeological sites (localities) from ROAD database.
#'
#' Use parameters to spatially delimit search results or oimit them to have a broader radius.
#' All parameters are optional and should be omitted or set to NULL when not used.
#'
#' @param continent string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param subcontinent string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param country string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param locality_type string (one item) or vector of strings (one or more items); defaults to NULL.
#'
#' @return Database search result as list of localities.
#' @export
#'
#' @examples road_get_localities()
#' @examples road_get_localities(continent = c("Europe"), country = c("Germany", "France"))
#' @examples road_get_localities(continent = "Europe", country = c("Germany", "France"))
#' @examples road_get_localities(country = c("Germany", "France"), locality_type = "cave")
#' @examples road_get_localities(NULL, NULL, "Germany")
road_get_localities <- function(continent = NULL, subcontinent = NULL, country = NULL, locality_type = NULL)
{
  # select fields
  select_fields <- c(
    paste0("locality.idlocality AS \"", cm_locality_idlocality, "\""),
    paste0("locality.type AS \"", cm_locality_type, "\""),
    paste0("geopolitical_units.continent AS \"", cm_geopolitical_units_continent, "\""),
    paste0("geopolitical_units.continent_region AS \"", cm_geopolitical_units_continent_region, "\""),
    paste0("locality.country AS \"", cm_locality_country, "\""),
    paste0("locality.x AS \"", cm_locality_x, "\""),
    paste0("locality.y AS \"", cm_locality_y, "\"")
  )

  # order by
  query_order_by <- ""
  if (!is.null(country))
  {
    query_order_by <- "ORDER BY locality.idlocality"
  }

  # combine query parts
  query <- paste(
    "SELECT DISTINCT",
    paste(select_fields, collapse = ", "),
    "FROM locality LEFT JOIN geopolitical_units ON locality.country = geopolitical_units.geopolitical_name WHERE NOT locality.no_data_entry AND geopolitical_units.rank = 1",
    parameter_to_query("AND geopolitical_units.continent IN (", continent, ")"),
    parameter_to_query("AND geopolitical_units.continent_region IN (", subcontinent, ")"),
    parameter_to_query("AND locality.country IN (", country, ")"),
    parameter_to_query("AND string_to_array(locality.type, ', ') && array[", locality_type, "]"),
    query_order_by
  )

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
#' This frunction uses the return value of `road_get_localities` (list of localities)
#' to get assemblages that were found in these localities.
#' Use parameters to further narrow down the assemblages you are searching for.
#' Excluding `localities` all parameters are optional and should be omitted or
#' set to NULL when not used.
#'
#' @param localities list of localities; return value from function `road_get_localities`.
#' @param category string (one item) or vector of strings (one or more items).
#' @param age_min integer; minimum age of assemblage.
#' @param age_max integer; maximum age of assemblage.
#'
#' @return Database search result as list of assembalges.
#' @export
#'
#' @examples road_get_assemblages(localities = road_get_localities())
#' @examples road_get_assemblages(localities, NULL, 80000L, 120000L)
#' @examples road_get_assemblages(localities = localities, category = "human remains", age_max = 100000L)
road_get_assemblages <- function(localities, category = NULL, age_min = NULL, age_max = NULL)
{
  if ((!is.null(age_min) && !is.integer(age_min)) || (!is.null(age_max) && !is.integer(age_max)))
    stop("Parameters 'min_age' and 'max_age' have to be integers.")

  if (!is.null(age_min) && !is.null(age_max) && age_min > age_max)
    stop("Parameter 'min_age' can not be bigger than 'max_age'.")

  # get preselected list of localities
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
    paste0("assemblage.category AS \"", cm_assemblages_category, "\""),
    paste0("geological_stratigraphy.age_min AS \"", cm_geological_stratigraphy_age_min, "\""),
    paste0("geological_stratigraphy.age_max AS \"", cm_geological_stratigraphy_age_max, "\""),
    paste0("assemblage_in_geolayer.geolayer_name AS \"", cm_assemblage_in_geolayer_geolayer_name, "\"")
  )

  # combine query parts
  query <- paste(
    "SELECT DISTINCT",
    paste(select_fields, collapse = ", "),
    "FROM assemblage",
    "JOIN assemblage_in_geolayer ON assemblage_in_geolayer.assemblage_idassemblage = assemblage.idassemblage",
    "JOIN geostrat_desc_geolayer ON geostrat_desc_geolayer.geolayer_idlocality = assemblage.locality_idlocality",
    "JOIN geological_stratigraphy ON geological_stratigraphy.idgeostrat = geostrat_desc_geolayer.geostrat_idgeostrat",
    "WHERE assemblage_in_geolayer.geolayer_idlocality = geostrat_desc_geolayer.geolayer_idlocality AND assemblage_in_geolayer.geolayer_name = geostrat_desc_geolayer.geolayer_name AND geological_stratigraphy.idgeostrat = geostrat_desc_geolayer.geostrat_idgeostrat AND assemblage_in_geolayer.assemblage_idlocality = assemblage.locality_idlocality AND assemblage_in_geolayer.assemblage_idassemblage = assemblage.idassemblage AND assemblage_in_geolayer.geolayer_idlocality IN (",
    #"FROM geological_stratigraphy, geostrat_desc_geolayer, assemblage_in_geolayer, assemblage WHERE assemblage_in_geolayer.geolayer_idlocality = geostrat_desc_geolayer.geolayer_idlocality AND assemblage_in_geolayer.geolayer_name = geostrat_desc_geolayer.geolayer_name AND geological_stratigraphy.idgeostrat = geostrat_desc_geolayer.geostrat_idgeostrat AND assemblage_in_geolayer.assemblage_idlocality = assemblage.locality_idlocality AND assemblage_in_geolayer.assemblage_idassemblage = assemblage.idassemblage AND assemblage_in_geolayer.geolayer_idlocality IN (",
    query_localities,
    ")",
    parameter_to_query("AND cardinality(array(select unnest(string_to_array(", category, ", ', ')) intersect select unnest(string_to_array(category, ', ')))) != 0"),
    parameter_to_query("AND ", age_min, " <= age_max"),
    parameter_to_query("AND ", age_max, " >= age_min"),
    "ORDER BY locality_idlocality"
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
    # convert string to vector
    if (is.string(parameter) && parameter != "")
      parameter <- c(parameter)

    # convert integer to vector
    if (is.integer(parameter) && parameter != 0)
      parameter <- c(parameter)

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
