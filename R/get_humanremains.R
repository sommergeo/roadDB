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
#' @examples road_get_human_remains(genus = 'Homo', species = 'neanderthalensis')
#' @examples road_get_human_remains(genus = 'Homo')
#' @examples road_get_human_remains(species = 'neanderthalensis')
#' @examples road_get_human_remains(genus_species = 'Homo neanderthalensis')
road_get_human_remains <- function(
    continents = NULL, 
    subcontinents = NULL, 
    countries = NULL, 
    locality_types = NULL, 
    cultural_periods = NULL, 
    categories = NULL, 
    age_min = NULL, 
    age_max = NULL, 
    genus = NULL, 
    species = NULL, 
    genus_species = NULL, 
    assemblages = NULL
)
{
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
        parameter_to_query("species IN (", species, ")")
      )
  
    }
    else genus_species_condition <- paste(genus_species_condition, ")")
  }
  
  # select fields
  select_fields <- c(
    paste0("humanremains_idlocality AS ", cm_locality_idlocality),
    paste0("humanremains_idassemblage AS ", cm_assemblages_idassemblage),
    paste0("genus || ' ' || species AS ", cm_humanremains_genus_species_str),
    paste0("genus AS ", cm_humanremains_genus),
    paste0("species AS ", cm_humanremains_species),
    paste0("age AS ", cm_humanremains_age),
    paste0("sex AS ", cm_humanremains_sex),
    paste0("humanremains_idhumanremains AS ", cm_humanremains_idhumanremains)
  )
  
  # combine query parts
  query <- paste(
    "SELECT DISTINCT * FROM ( SELECT ",
    paste(select_fields, collapse = ", "), 
    " FROM publication_desc_humanremains) as foo  
    WHERE TRUE ",
    assemblage_condition,
    genus_species_condition,
    "ORDER BY ", cm_locality_idlocality, ", ", cm_assemblages_idassemblage 
  )
  
  data <- road_run_query(query)
  
  data <- add_locality_columns(data, assemblages = assemblages)
  
  return(data)
}
