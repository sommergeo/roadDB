#' Get human remains from ROAD database
#'
#' The \strong{\code{road_get_human_remains}} function fetches data of human
#' remains from ROAD database. Human remains are human fossil finds and always
#' associated with an assemblage. A human remain is a direct and substantial
#' piece of evidence for the presence of fossil hominids at a particular locality.
#' Next to the assemblage information the function returns genus, species,
#' age and sex if available and further information regarding the remains.
#' The dataset may also include indirect evidence such as fossil endocasts and
#' footprints.
#'
#' @details
#' Use the parameters to filter the results or omit them to retrieve a broader
#' dataset. Genus and species parameters can be entered as a vector of strings
#' to search for multiple entries. If genus and species are both specified,
#' most of the time it's more sensible to enter them as single strings and not
#' as vectors with multiple search words to recieve useful results.
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
#' as the baseline. The argument \code{age_min} is an integer; defaults to NULL.
#' @param age_max specifies the maximum age in years before present, using 1950 CE
#' as the baseline. The argument \code{age_max} is an integer; defaults to NULL.
#' @param assemblages list of assemblages; return value from function
#' \code{road_get_assemblages}. Can be used instead of the other locality and
#' assemblage parameters to filter the results.
#' @param human_genus specifies the genus to which the described fossil is
#' attributed to. Possible entries include: "Australopithecus", "Homo", "indet",
#' etc. Run \code{road_list_argument_values("human_genus")} to display possible
#' values. The argument \code{human_genus} is a string (one item) or vector of
#' strings; defaults to NULL.
#' @param human_species specifies the species to which the described fossil is
#' attributed. Possible entries include: "afarensis", "sapiens", "erectus" or
#' "sp." for unidentified species.
#' Run \code{road_list_argument_values("human_species")} to display
#' possible values.The parameter \code{human_species} is a string (one item) or
#' vector of strings; defaults to NULL.
#'
#' @return A data frame with human remains information. Rows represent individual
#' human remains finds, columns contain standard outputs and human remains-related details on:
#' @return \code{humanremains_id}: Unique identifier for each human remains entry per assemblage.
#' @return \code{genus}: The genus to which the described fossil is attributed to.
#' @return \code{species}: The species to which the described fossil is attributed to.
#' @return \code{age}: The age class of the individual at death (e.g. infant, juvenile, adult, subadult, mature).
#' @return \code{sex}: The sex of the individual (e.g. F for female or M for male).
#' @return \code{skeletal_element}: The attribute skeletal_element is an anatomical designation and contains a brief description of
#' the fossil described in the dataset. Cranial and postcranial bones are named by their anatomical
#' appellation in Latin, for example: humerus, (os) capitatum, (os) zygomaticum, os coxae, etc. Left
#' and right are denoted as "L" and "R". Teeth are abbreviated by letters (M = molars, P = premolars,
#' I = incisors, C = canines) combined with Arabic numerals ([automatically generated]) to definitively
#' assign their position in the dental arch. The appellations "upper" and "lower" are not expressed
#' through a subscript or superscript number but written as words. Possible examples include: upper
#' L M1, R humeral epiphysis, natural endocast, etc.
#' @return \code{humanremains_category}: The attribute category indicates the element group to which the fossil belongs.
#' C for cranial bones, D for dental remains, P for postcranial bones, E for natural endocasts,
#' I for imprints that are negative molds of body parts, including footprints, S for sediment (containing human aDNA).
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#'
#' @export
#'
#' @examples
#' road_get_human_remains(subcontinents = "East Africa", human_genus = c('Paranthropus'))
#' road_get_human_remains(subcontinents = 'Caucasus', human_genus = 'Homo', 
#'                        human_species = 'neanderthalensis')
road_get_human_remains <- function(
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
    human_genus = NULL,
    human_species = NULL
)
{
  # calculate assemblage_condition
  if ((!is.null(categories) || !is.null(age_min) || !is.null(age_max)) && !is.null(assemblages))
    warning("No assemblage search for categories or age_min/age_max is performed because a non-empty assemblage list was passed")

  if (is.null(assemblages))  assemblages <- road_get_assemblages(continents = continents, 
                                                                 subcontinents = subcontinents, 
                                                                 countries = countries, 
                                                                 locality_types = locality_types, 
                                                                 cultural_periods = cultural_periods, 
                                                                 technocomplexes = technocomplexes, 
                                                                 categories = categories, 
                                                                 age_min = age_min, 
                                                                 age_max = age_max)

  assemblage_condition <- get_assemblage_condition(query_start = " AND ", assemblages = assemblages)

  # build genus/species condition
  genus_condition <- ""
  species_condition <- ""

  if (!is.null(human_genus))
  {
    genus_condition <- parameter_to_query("AND genus IN (", human_genus, ")")
  }
  if (!is.null(human_species))
  {
    species_condition <- parameter_to_query("AND species IN (", human_species, ")")
  }

  # select fields
  select_fields <- c(
    paste0("humanremains_idlocality AS ", cm_locality_idlocality),
    paste0("humanremains_idassemblage AS ", cm_assemblages_idassemblage),
    paste0("humanremains_idhumanremains AS ", cm_humanremains_idhumanremains),
    paste0("genus AS ", cm_humanremains_genus),
    paste0("species AS ", cm_humanremains_species),
    paste0("age AS ", cm_humanremains_age),
    paste0("sex AS ", cm_humanremains_sex),
    paste0("skeletal_element AS ", cm_humanremains_skeletal_element),
    paste0("category AS ", cm_humanremains_category))

  # combine query parts
  query <- paste(
    "SELECT DISTINCT * FROM ( SELECT ",
    paste(select_fields, collapse = ", "),
    " FROM humanremains ",
    "LEFT JOIN publication_desc_humanremains ON 
      humanremains.assemblage_idlocality = publication_desc_humanremains.humanremains_idlocality AND 
      humanremains.assemblage_idassemblage = publication_desc_humanremains.humanremains_idassemblage AND 
      humanremains.idhumanremains = publication_desc_humanremains.humanremains_idhumanremains ",
    ") as foo ",
    " WHERE TRUE ",
    assemblage_condition,
    genus_condition,
    species_condition,
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
                              genus = human_genus,
                              species = human_species
    )
      
  }

  data <- add_locality_columns(data, assemblages = assemblages)

  return(data)
}
