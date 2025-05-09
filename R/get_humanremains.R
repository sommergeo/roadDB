#' Get human remains from ROAD database
#'
#' `road_get_human_remains` fetches data of human remains from ROAD database.
#'
#' Human remains are always part of an assemblage which means the function needs a list of
#' assemblages (return value of function `road_get_assemblages`) as its first parameter.
#' If you will search for genus AND species use this function with `genus` AND `species` as parameter
#'
#' @param assemblages list of assemblages; return value from function `road_get_assemblages`.
#' @param continents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param subcontinents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param countries string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param locality_types string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param cultural_periods string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param categories string (one item) or vector of strings (one or more items).
#' @param age_min integer; minimum age of assemblage.
#' @param age_max integer; maximum age of assemblage.
#' @param genus string (one item) or vector of strings (one or more items).
#' @param species string (one item) or vector of strings (one or more items).
#' 
#' @return Database search result as list of human remains.
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' 
#' @export
#'
#'
#' @examples
#' road_get_human_remains(genus = 'Homo')
#' road_get_human_remains(continents = "Europe", genus = c('Homo', 'Paranthropus'))
#' road_get_human_remains(species = 'neanderthalensis')
#' road_get_human_remains(species = c('neanderthalensis', 'erectus'))

road_get_human_remains <- function(
    assemblages = NULL,
    continents = NULL, 
    subcontinents = NULL, 
    countries = NULL, 
    locality_types = NULL, 
    cultural_periods = NULL, 
    categories = NULL, 
    age_min = NULL, 
    age_max = NULL, 
    genus = NULL, 
    species = NULL 
)
{
  # calculate assemblage_condition
  if ((!is.null(categories) | !is.null(age_min) | !is.null(age_max)) & !is.null(assemblages)) 
    warning("No assemblage search for categories or age_min/age_max is performed because a non-empty assemblage list was passed")

  if (is.null(assemblages))  assemblages <- road_get_assemblages(continents = continents, 
                                                                subcontinents = subcontinents, 
                                                                countries = countries, 
                                                                locality_types = locality_types, 
                                                                cultural_periods = cultural_periods,
                                                                categories = categories, 
                                                                age_min = age_min, 
                                                                age_max = age_max)

  assemblage_condition <- get_assemblage_condition(query_start = " AND ", assemblages = assemblages)
  
  # build genus/species condition
  if (is.vector(genus) && is.vector(species))
  {
    cp <- expand.grid(genus = genus, species = species)
    
    cp <- cp %>% mutate(genus_species=paste(genus, species, sep=" "))
    s <- paste(cp$genus_species, collapse="; ")
    warning(paste("If none of the following genus and species combinations 
                  ", s, "
                  are in the database, 
                  the search results will be empty"))
  }
  genus_condition <- ""
  species_condition <- ""
  
  if (!is.null(genus))
  {
    genus_condition <- parameter_to_query("AND genus IN (",genus, ")")
  }
  if (!is.null(species))
  {
    species_condition <-parameter_to_query("AND species IN (",species, ")")
  }

  # select fields
  select_fields <- c(
    paste0("humanremains_idlocality AS ", cm_locality_idlocality),
    paste0("humanremains_idassemblage AS ", cm_assemblages_idassemblage),
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
    genus_condition,
    species_condition,
    "ORDER BY ", cm_locality_idlocality, ", ", cm_assemblages_idassemblage 
  )

  data <- road_run_query(query)
  
  data <- add_locality_columns(data, assemblages = assemblages)
  
  return(data)
}
