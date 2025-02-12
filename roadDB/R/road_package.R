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
cm_geolayer_geolayer_name <- "geolayer"
cm_assemblage_in_archlayer_archlayer_name <- "archlayers"
cm_archlayer_archlayer_name <- "archlayer"
cm_age <- "age"
cm_negative_standard_deviation <- "negative_standard_deviation"
cm_positive_standard_deviation <- "positive_standard_deviation"
cm_material_dated <- "material_dated"
cm_dating_method <- "dating_method"
cm_laboratory_idlaboratory <- "laboratory_idlaboratory"
cm_humanremains_genus_species_str <- "genus_species_str"
cm_humanremains_genus <- "genus"
cm_humanremains_species <- "species"
cm_humanremains_age <- "age"
cm_humanremains_sex <- "sex"
cm_humanremains_idhumanremains <- "humanremains_id"

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
  queryAux <- paste(
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
  
  query <-  paste("SELECT DISTINCT locality_id, assemblage_id, name, categories, age_min, age_max, geolayers",
                  paste0(", STRING_AGG(DISTINCT assemblage_in_archlayer.archlayer_name, ', ') AS \" ", 
                         cm_assemblage_in_archlayer_archlayer_name, "\", "),
                  "humanremains, paleofauna, archaeology, plantremains ",
                  "FROM (", queryAux, ") as foo ",
                  "LEFT JOIN assemblage_in_archlayer ON ",
                  "assemblage_in_archlayer.assemblage_idlocality = ", cm_assemblages_locality_idlocality, 
                  "AND assemblage_in_archlayer.assemblage_idassemblage = ", cm_assemblages_idassemblage,
                  "GROUP BY ", cm_assemblages_locality_idlocality, ", ", cm_assemblages_idassemblage, ", ",
                  cm_assemblages_name, ", ",cm_assemblages_categories, ", ", 
                  cm_geological_stratigraphy_age_min, ", ", cm_geological_stratigraphy_age_max, ", ", 
                  "humanremains, paleofauna, archaeology, plantremains", ", ", 
                  cm_assemblage_in_geolayer_geolayer_name, 
                  "ORDER BY ", cm_assemblages_locality_idlocality, " ASC"
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
#' @param continents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param subcontinents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param countries string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param locality_types string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param cultural_periods string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param localities list of localities; return value from function `road_get_localities`.
#' @param categories string (one item) or vector of strings (one or more items).
#' @param age_min integer; minimum age of assemblage.
#' @param age_max integer; maximum age of assemblage.
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
road_get_human_remains <- function(continents = NULL, subcontinents = NULL, countries = NULL, 
                                   locality_types = NULL, cultural_periods = NULL, 
                                   categories = NULL, age_min = NULL, age_max = NULL, 
                                   genus = NULL, species = NULL, genus_species = NULL, 
                                   assemblages = NULL, localities = NULL)
{
  # calculate locality_condition
  # To do: !is.null(one of localities parameters) AND !is.null(localities)  ---> Warnung an den Benutzer
  if (is.null(localities)) localities <- road_get_localities(continents = continents, 
                                                             subcontinents = subcontinents, 
                                                             countries = countries, 
                                                             locality_types = locality_types, 
                                                             cultural_periods = cultural_periods)
  # locality_condition <- get_locality_condition(localities = localities)
  query_localities <- paste(
   sapply(localities$locality_id, function(x) paste0("'", x, "'")),
    collapse = ", "
  )
  # calculate output extention
  locality_info_for_output <- get_output_extention_locality(localities)

  # calculate assemblage_condition
  # To do: !is.null(categories) AND !is.null(assemblages)  ---> Warnung an den Benutzer
  if (is.null(assemblages)) assemblages <- road_get_assemblages(categories = categories, 
                                                                age_min = age_min, age_max = age_max, 
                                                                localities = localities)
  assemblage_condition <- get_assemblage_condition(assemblages = assemblages)
  # calculate output extention
  assemblage_info_for_output <- get_output_extention_assemblage(assemblages)

  if (!is.null(genus_species) && (!is.null(genus) || !is.null(species)))
    stop("Parameter 'genus_species' can't be used in combination with 'genus' or 'species'.")

   # build genus/species selection
  genus_species_condition = ""
  if (!is.null(genus_species))
  {
    genus_species_condition <- parameter_to_query("AND genus_species_str IN (", genus_species, ")")
  }
  else
  {
    species_conjucton <- "AND"
    if (!is.null(genus))
    {
      genus_species_condition <- parameter_to_query("AND ( genus IN (", genus, ")")
      species_conjucton <- "OR"
    }
    if (!is.null(species))
    {
      genus_species_condition <- paste(
        genus_species_condition,
        species_conjucton,
        parameter_to_query("species IN (", species, ") )")
      )
    }
    else genus_species_condition <- paste(genus_species_condition," )")
  }

  # select fields
  select_fields <- c(
    paste0("humanremains_idlocality AS \"", cm_locality_idlocality, "\""),
    paste0("humanremains_idassemblage AS \"", cm_assemblages_idassemblage, "\""),
    paste0("genus || ' ' || species AS \"", cm_humanremains_genus_species_str, "\""),
    paste0("genus AS \"", cm_humanremains_genus, "\""),
    paste0("species AS \"", cm_humanremains_species, "\""),
    paste0("age AS \"", cm_humanremains_age, "\""),
    paste0("sex AS \"", cm_humanremains_sex, "\""),
    paste0("humanremains_idhumanremains AS \"", cm_humanremains_idhumanremains, "\"")
  )
  
  # combine query parts
  query <- paste(
    "SELECT DISTINCT * FROM ( SELECT ",
    paste(select_fields, collapse = ", "), 
    " FROM publication_desc_humanremains) as foo  
    WHERE ", cm_locality_idlocality," IN (",
    query_localities, ")",
    assemblage_condition,
    genus_species_condition,
    "ORDER BY ", cm_locality_idlocality, ", ", cm_assemblages_idassemblage 
  )
  
  message(query)
  
  data <- road_run_query(query)
  
  data$genus[data$genus == ""] <- NA
  data$species[data$species == ""] <- NA
  data$age[data$age == ""] <- NA
  data$sex[data$sex == ""] <- NA
  data$genus_species_str[data$genus_species_str == ""] <- NA

  data_plus_assemblage_info <- merge(x = data, y = assemblage_info_for_output, by = c(cm_locality_idlocality, cm_assemblages_idassemblage))

  return(merge(x = data_plus_assemblage_info, y = locality_info_for_output, by = cm_locality_idlocality))

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

# build query to check if parameters intersect with comma separated database values
query_check_intersection <- function(query_start, parameter, column)
{
  query <- ""
  if (!is.null(parameter))
  {
    parameter <- parameter_to_vector(parameter)

    if (is.vector(parameter))
    {
      query <- paste(
        #sapply(parameter, function(x) paste0("OR '", x, "' = ANY(STRING_TO_ARRAY(", column, ", ', '))")),
        sapply(parameter, function(x) paste0("OR '", x, "' = ANY(regexp_split_to_array(", column, ", ',\\s*'))")),
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

# calculate assemblage_condition
get_assemblage_condition <- function(assemblages = NULL)
{
  assemblage_condition <- ""
  # To do: !is.null(categories) AND !is.null(assemblages)  ---> Warnung an den Benutzer
  #if (is.null(assemblages)) assemblages <- road_get_assemblages(categories = categories, 
   #                                                             age_min = age_min, age_max = age_max, localities = localities)
  
  assemblages$locality_assemblage_list <- paste(assemblages$locality_id, assemblages$assemblage_id, sep = ", ")
  
  query_locality_assemblage_list <- ""
  query_locality_assemblage_list <- paste(
    sapply(assemblages$locality_assemblage_list, function(x) paste0("'", x, "'")),
    collapse = ", "
  )
  
  if (!is.null(query_locality_assemblage_list) && query_locality_assemblage_list != '')
    assemblage_condition <- paste0(" AND ", cm_locality_idlocality, " || ', ' || ", 
                                   cm_assemblages_idassemblage," IN (", query_locality_assemblage_list, ")")
  
  return(assemblage_condition)
}

get_output_extention_locality <- function(localities = NULL)
{
  if (is.null(localities)) return(NULL)
  
  locality_info_for_output <- list()
  locality_info_for_output$locality_id <- localities$locality_id
  locality_info_for_output$assemblage_id <- localities$assemblage_id
  locality_info_for_output$continent <- localities$continent
  locality_info_for_output$subcontinent <- localities$subcontinent
  locality_info_for_output$country <- localities$country
  locality_info_for_output$locality_types <- localities$locality_types
  locality_info_for_output$cultural_periods <- localities$cultural_periods
  
  return(locality_info_for_output)
}

get_output_extention_assemblage <- function(assemblages = NULL)
{
  if (is.null(assemblages)) return(NULL)
  
  assemblage_info_for_output <- list()
  assemblage_info_for_output$locality_id <- assemblages$locality_id
  assemblage_info_for_output$assemblage_id <- assemblages$assemblage_id
  assemblage_info_for_output$categories <- assemblages$categories
  assemblage_info_for_output$age_min <- assemblages$age_min
  assemblage_info_for_output$age_max <- assemblages$age_max
  
  return(assemblage_info_for_output)
}