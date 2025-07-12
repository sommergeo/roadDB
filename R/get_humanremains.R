#' Get human remains from ROAD database
#'
#' The function \code{road_get_human_remains} fetches data of human remains from ROAD database.
#'
#' Human remains are human fossil finds and always associated with an assemblage. A human remain is
#' a direct and substantial piece of evidence for the presence of fossil hominids at a particular locality.
#' Next to the assemblage information the function returns genus, species, age and sex if available
#' and further information regarding the remains.
#' The dataset may also include indirect evidence such as fossil endocasts and footprints.
#' Use the parameters to filter the results or omit them to retrieve a broader dataset.
#' Genus and species parameters can be entered as a vector of strings to search for multiple entries.
#' If genus and species are both specified, most of the time it's more sensible to enter them as
#' single strings and not as vectors with multiple search words to recieve useful results.
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
#' @param age_min specifies the minimum age of assemblage. The parameter \code{age_min} is an integer.
#' @param age_max specifies the maximum age of assemblage. The parameter \code{age_max} is an integer.
#' @param assemblages list of assemblages; return value from function \code{road_get_assemblages}.
#' Can be used instead of the other locality and assemblage parameters to filter the results.
#' @param genus specifies the genus to which the described fossil is attributed to.
#' Possible entries include: "Australopithecus", "Homo", "indet", etc.
#' The parameter genus is a string (one item) or vector of strings (one or more items).
#' @param species specifies the species to which the described fossil is attributed. Possible entries include:
#' "afarensis", "sapiens", "erectus" or "sp." for unidentified species.
#' The parameter species is a string (one item) or vector of strings (one or more items).
#'
#' @return Database search result as list of assemblages with human remains.
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#'
#' @export
#'
#' @examples
#' road_get_human_remains(genus = c('Homo', 'Paranthropus'))
#' road_get_human_remains(continents = 'Europe', genus = 'Homo', species = 'neanderthalensis')
road_get_human_remains <- function(
    continents = NULL,
    subcontinents = NULL,
    countries = NULL,
    locality_types = NULL,
    cultural_periods = NULL,
    categories = NULL,
    age_min = NULL,
    age_max = NULL,
    assemblages = NULL,
    genus = NULL,
    species = NULL
)
{
  # calculate assemblage_condition
  if ((!is.null(categories) || !is.null(age_min) || !is.null(age_max)) && !is.null(assemblages))
    warning("No assemblage search for categories or age_min/age_max is performed because a non-empty assemblage list was passed")

  if (is.null(assemblages))  assemblages <- road_get_assemblages(continents, subcontinents, countries, locality_types, cultural_periods, categories, age_min, age_max)

  assemblage_condition <- get_assemblage_condition(query_start = " AND ", assemblages = assemblages)

  # build genus/species condition
  genus_condition <- ""
  species_condition <- ""

  if (!is.null(genus))
  {
    genus_condition <- parameter_to_query("AND genus IN (", genus, ")")
  }
  if (!is.null(species))
  {
    species_condition <- parameter_to_query("AND species IN (", species, ")")
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
                                genus,
                                species
    )
      
  }

  data <- add_locality_columns(data, assemblages = assemblages)

  return(data)
}
