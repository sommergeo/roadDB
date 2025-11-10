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
#' @param continents specifies the continent(s) (e.g. Africa, Europe, Asia).
#' Run \code{road_list_argument_values("continents")} to display possible values.
#' The argument \code{continents} is a string (one item) or vector of strings
#' (one or more items); defaults to NULL.
#' @param subcontinents specifies the continental region(s) (e.g. Southern Europe).
#' Run \code{road_list_argument_values("subcontinents")} to display possible values.
#' The argument \code{subcontinents} is a string (one item) or vector of strings
#' (one or more items); defaults to NULL.
#' @param countries specifies the name of the country where a locality is situated
#' (e.g. Germany, Kenya, Saudi Arabia). Run \code{road_list_argument_values("countries")}
#' to display possible values.
#' The argument \code{countries} is a string (one item) or vector of strings
#' (one or more items); defaults to NULL.
#' @param locality_types specifies the type of locality (e.g. cave, rockshelter, open air).
#' Run \code{road_list_argument_values("locality_types")} to display possible values.
#' The argument \code{locality_types} is a string (one item) or vector of strings
#' (one or more items); defaults to NULL.
#' @param cultural_periods specifies the main cultural epoch(s) and includes the
#' Eurasian Paleolithic (Lower, Middle, Upper, Epi-) and the African Stone Age
#' (Earlier, Middle, Later). Run \code{road_list_argument_values("cultural_periods")}
#' to display possible values. The argument \code{cultural_periods} is a string
#' (one item) or vector of strings (one or more items); defaults to NULL.
#' @param technocomplexes specifies an archaeological culture or named stone tool
#' industry (e.g. Oldowan, Acheulean, Mousterian).
#' Run \code{road_list_argument_values("technocomplexes")} to display possible values.
#' The argument \code{technocomplexes} is a string (one item) or vector of strings
#' (one or more items); defaults to NULL.
#' @param categories specifies the assemblage category/categories with the classes
#' human remains, raw material, typology, technology, function, organic tools,
#' symbolic artifacts, feature, miscellaneous finds, paleofauna, animal remains,
#' plant remains. The argument \code{categories} is a string (one item) or
#' vector of strings (one or more items); defaults to NULL.
#' @param age_min specifies the minimum age in years before present, using 1950 CE
#' as the baseline. If possible the argument \code{age_min} will be converted to an integer; defaults to NULL.
#' @param age_max specifies the maximum age in years before present, using 1950 CE
#' as the baseline. If possible the argument \code{age_max} will be converted to an integer; defaults to NULL.
#' @param assemblages specifies a data frame necessarily containing columns 
#' locality_id, assemblage_id. It can be  generated as return value of the 
#' function 'road_get_assemblages'. It can be used instead of the locality 
#' and assemblage search parameters to filter the results.
#' @param fauna_genus specifies the genus to which the described faunal remains
#' is attributed to. Possible entries include: "Mammuthus", "Vulpes" etc.
#' Run \code{road_list_argument_values("fauna_genus")} to
#' display possible values. The argument \code{fauna_genus} is a string
#' (one item) or vector of strings; defaults to NULL.
#' @param fauna_species specifies the species to which the
#' described faunal remains is attributed. Possible entries include:
#' "primigenius", "vulpes" or "sp." for unidentified species. Run
#' \code{road_list_argument_values("fauna_species")} to display possible values.
#' The argument \code{fauna_species} is a string (one item) or vector of strings;
#' defaults to NULL.
#'
#' @return A data frame with paleofauna information. Rows represent individual
#' faunal remains finds, columns contain standard outputs and faunal remains-related details on:
#' @return \code{fauna_genus}, \code{fauna_species}: The attributes specify the taxonomic classification of the paleofaunal remains.
#'
#' @export
#'
#' @examples
#' road_get_paleofauna(countries = c("Spain", "Portugal"), fauna_genus = "Mammuthus")
#' road_get_paleofauna(continents = "Arfica", fauna_genus = "Vulpes",
#'                     fauna_species = "vulpes")
road_get_paleofauna <- function(
  continents = NULL,
  subcontinents = NULL,
  countries = NULL,
  locality_types = NULL,
  cultural_periods = NULL,
  technocomplexes = NULL,
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

  if (is.null(assemblages)) assemblages <- road_get_assemblages(continents = continents, 
                                                                subcontinents = subcontinents, 
                                                                countries = countries, 
                                                                locality_types = locality_types, 
                                                                cultural_periods = cultural_periods, 
                                                                technocomplexes = technocomplexes, 
                                                                categories = categories, 
                                                                age_min = age_min, 
                                                                age_max = age_max)

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
    print_null_result_message(continents = continents,
                              subcontinents = subcontinents,
                              countries = countries,
                              locality_types = locality_types,
                              cultural_periods = cultural_periods,
                              technocomplexes = technocomplexes,
                              categories = categories,
                              age_min = age_min,
                              age_max = age_max,
                              fauna_genus = fauna_genus,
                              fauna_species = fauna_species
    )
  }

  data <- add_locality_columns(data, assemblages = assemblages)

  return(data)
}