#' Get paleofauna finds from ROAD database
#'
#' The  \strong{\code{road_get_paleofauna}} function fetches data of paleofauna finds from the ROAD database.
#' Paleofauna finds are animal fossil remains discovered in archaeological contexts and are always associated with an assemblage.
#' These finds provide direct evidence for the presence of animal species at a particular locality and time.
#' The function returns information about the assemblage in which certain faunal remains were found as well as their genus and species.
#' 
#' @details
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
#' @param fauna_genus specifies the genus to which the described faunal remains is attributed to.
#' Possible entries include: "Mammuthus", "Vulpes" etc.
#' The parameter genus is a string (one item) or vector of strings (one or more items).
#' @param fauna_species specifies the species to which the described faunal remains is attributed. Possible entries include:
#' "primigenius", "vulpes" or "sp." for unidentified species.
#' The parameter species is a string (one item) or vector of strings (one or more items).
#'
#' @return Database search result as list of assemblages with paleofauna information.
#' @export
#'
#' @examples
#' road_get_paleofauna(fauna_genus = "Mammuthus")
#' road_get_paleofauna(continents = "Europe", fauna_genus = "Vulpes", fauna_species = "vulpes")
road_get_paleofauna <- function(
  continents = NULL,
  subcontinents = NULL,
  countries = NULL,
  locality_types = NULL,
  cultural_periods = NULL,
  categories = NULL,
  age_min = NULL,
  age_max = NULL,
  assemblages = NULL,
  fauna_genus = NULL,
  fauna_species = NULL
) {
  # calculate assemblage_condition
  if ((!is.null(categories) || !is.null(age_min) || !is.null(age_max)) && !is.null(assemblages))
    warning("No assemblage search for categories or age_min/age_max is performed because a non-empty assemblage list was passed")

  if (is.null(assemblages)) assemblages <- road_get_assemblages(continents, subcontinents, countries, locality_types, cultural_periods, categories, age_min, age_max)

  assemblage_condition <- get_assemblage_condition(query_start = " AND ", assemblages = assemblages)

  #build genus/species condition
  fauna_genus_condition <- ""
  fauna_species_condition <- ""

  if (!is.null(fauna_genus))
    fauna_genus_condition <- parameter_to_query("AND fauna_genus IN (", fauna_genus, ")")

  if (!is.null(fauna_species))
    fauna_species_condition <- parameter_to_query("AND fauna_species IN (", fauna_species, ")")

  # select fields
  select_fields <- c(
    paste0("assemblage_idlocality AS ", cm_locality_idlocality),
    paste0("assemblage_idassemblage AS ", cm_assemblages_idassemblage),
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

  if (nrow(data) == 0 && nrow(assemblages) > 0)
  {
    # continents_str <- ifelse(is.null(continents), "", paste("continents = (", toString(continents), ")"))
    # subcontinents_str <- ifelse(is.null(subcontinents), "", paste("subcontinents = (", toString(subcontinents), ")"))
    # countries_str <- ifelse(is.null(countries), "", paste("countries = (", toString(countries), ")"))
    # locality_types_str <- ifelse(is.null(locality_types), "", paste("locality_types = (", toString(locality_types), ")"))
    # cultural_periods_str <- ifelse(is.null(cultural_periods), "", paste("cultural_periods = (", toString(cultural_periods), ")"))
    # 
    # categories_str <- ifelse(is.null(categories), "", paste("categories = (", toString(categories), ")"))
    # age_min_str <- ifelse(is.null(age_min), "", paste("age_min = (", age_min, ")"))
    # age_max_str <- ifelse(is.null(age_max), "", paste("age_max = (", age_max, ")"))
    # 
    # fauna_genus_str <- ifelse(is.null(fauna_genus), "", paste("fauna_genus = (", toString(fauna_genus), ")"))
    # fauna_species_str <- ifelse(is.null(fauna_species), "", paste("fauna_species = (", toString(fauna_species), ")"))
    # 
    # message(paste("One or more of the following parameters caused the empty result set:
    #               ",
    #               fauna_genus_str,
    #               fauna_species_str,
    #               "
    #   Please keep in mind, the data search needs exact parameter values. To get exact values for a given parameter 'p' you can use the function road_list_parameter_values('p')."))
    # if (is.vector(fauna_genus) && is.vector(fauna_species))
    # {
    #   genus <- ""
    #   cp <- expand.grid(genus = fauna_genus, species = fauna_species)
    #   
    #   cp <- cp %>% mutate(genus_species = paste(genus, species, sep = " "))
    #   s <- paste(cp$genus_species, collapse = "; ")
    #   message(paste("
    #   Please keep in mind at least one of the the following combinations (fauna_genus fauna_species) have to be in the database::
    #               ", s))
    # }
    # 
    print_null_result_message(  continents,
                                subcontinents,
                                countries,
                                locality_types,
                                cultural_periods,
                                categories,
                                age_min,
                                age_max,
                                fauna_genus,
                                fauna_species
    )
  }

  data <- add_locality_columns(data, assemblages = assemblages)

  return(data)
}