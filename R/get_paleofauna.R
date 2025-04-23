#' Get paleofauna finds from ROAD database
#'
#' `road_get_paleofauna` fetches data of paleofauna finds from ROAD database.
#'
#' Paleofauna finds are often part of an assemblage which means the function needs a list of
#' assemblages (return value of function `road_get_assemblages`) as its parameter.
#'
#' @param continents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param subcontinents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param countries string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param locality_types string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param cultural_periods string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param categories string (one item) or vector of strings (one or more items).
#' @param age_min integer; minimum age of assemblage.
#' @param age_max integer; maximum age of assemblage.
#' @param fauna_genus string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param fauna_species string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param assemblages list of assemblages; return value from function `road_get_assemblages`.
#' 
#' @return Database search result as list of archaeological finds.
#' @export
#'
# @examples road_get_paleofauna(continents = "Europe")
# @examples road_get_paleofauna(continents = "Europe", archaeological_category = "feature")
# @examples road_get_paleofauna(continents = "Europe", archaeological_category = c("feature", "symbolic artefacts"))
road_get_paleofauna <- function(
    continents = NULL, 
    subcontinents = NULL, 
    countries = NULL, 
    locality_types = NULL,
    cultural_periods = NULL, 
    categories = NULL, 
    age_min = NULL, 
    age_max = NULL,
    fauna_genus = NULL,
    fauna_species = NULL,
    assemblages = NULL
) {
  # calculate assemblage_condition
  # To do: !is.null(categories) AND !is.null(assemblages)  ---> Warnung an den Benutzer
  if (is.null(assemblages)) assemblages <- road_get_assemblages(continents = continents, 
                                                                subcontinents = subcontinents, 
                                                                countries = countries, 
                                                                locality_types = locality_types, 
                                                                cultural_periods = cultural_periods, 
                                                                categories = categories, 
                                                                age_min = age_min, 
                                                                age_max = age_max)
  
  assemblage_condition <- get_assemblage_condition(query_start = "AND ", assemblages = assemblages)
  
  
  if (!is.null(fauna_genus)) 
    fauna_genus_condition <- query_check_intersection("AND ", 
                                                      fauna_genus, 
                                                      cm_fauna_genus)
  else 
    fauna_genus_condition <- ""
  
  if (!is.null(fauna_species)) 
    fauna_species_condition <- query_check_intersection("AND ", 
                                                        fauna_species, 
                                                        cm_fauna_species)
  else 
    fauna_species_condition <- ""
  
  # select fields
  select_fields <- c(
    paste0(" assemblage_idlocality AS ", cm_locality_idlocality),
    paste0(" assemblage_idassemblage AS ", cm_assemblages_idassemblage),
    paste0("genus AS ", cm_fauna_genus),
    paste0("species AS ", cm_fauna_species)
  )
  
  # combine query parts
  query <- paste(
    "SELECT DISTINCT * FROM ( SELECT ",
    paste(select_fields, collapse = ", "),
    " FROM paleofauna",
    "LEFT JOIN taxonomical_classification ON",
    "taxonomical_classification_id_t_c = idtaxonomical_classification",
    ") as foo WHERE TRUE ",
    assemblage_condition,
    fauna_genus_condition,
    fauna_species_condition,
    "ORDER BY ", cm_locality_idlocality, ", ", cm_assemblages_idassemblage 
  )
  
  data <- road_run_query(query)
  
  data <- add_locality_columns(data, assemblages = assemblages)
  
  return(data)
}