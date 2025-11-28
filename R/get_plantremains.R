#' Get paleobotany data from ROAD database
#'
#' The function  \strong{\code{road_get_plantremains}} fetches data of paleobotanical remains from the ROAD database.
#' Paleobotanical remains are plant remains found in archaeological contexts and are associated with
#' assemblages.
#'
#' @details
#' This function allows you to query paleobotanical data based on
#' various parameters such as geographical location, cultural periods, plant taxonomy,
#' and assemblages. Use the parameters to filter the results or omit them to retrieve a broader dataset.
#' Genus and species parameters can be entered as a vector of strings to search for multiple entries.
#' If genus and species are both specified, most of the time it's more sensible to enter them as
#' single strings and not as vectors with multiple search words to recieve useful results.
#'
#' @param continent specifies the continent(s) (e.g. Africa, Europe, Asia).
#' Run \code{road_list_argument_values("continent")} to display possible values.
#' The argument \code{continent} is a string (one item) or vector of strings
#' (one or more items); defaults to NULL.
#' @param subcontinent specifies the continental region(s) (e.g. Southern Europe).
#' Run \code{road_list_argument_values("subcontinent")} to display possible values.
#' The argument \code{subcontinent} is a string (one item) or vector of strings
#' (one or more items); defaults to NULL.
#' @param country specifies the name of the country where a locality is situated
#' (e.g. Germany, Kenya, Saudi Arabia). Run \code{road_list_argument_values("country")}
#' to display possible values.
#' The argument \code{country} is a string (one item) or vector of strings
#' (one or more items); defaults to NULL.
#' @param locality_type specifies the type of locality (e.g. cave, rockshelter, open air).
#' Run \code{road_list_argument_values("locality_type")} to display possible values.
#' The argument \code{locality_type} is a string (one item) or vector of strings
#' (one or more items); defaults to NULL.
#' @param cultural_period specifies the main cultural epoch(s) and includes the
#' Eurasian Paleolithic (Lower, Middle, Upper, Epi-) and the African Stone Age
#' (Earlier, Middle, Later). Run \code{road_list_argument_values("cultural_period")}
#' to display possible values. The argument \code{cultural_period} is a string
#' (one item) or vector of strings (one or more items); defaults to NULL.
#' @param technocomplex specifies an archaeological culture or named stone tool
#' industry (e.g. Oldowan, Acheulean, Mousterian).
#' Run \code{road_list_argument_values("technocomplex")} to display possible values.
#' The argument \code{technocomplex} is a string (one item) or vector of strings
#' (one or more items); defaults to NULL.
#' @param category specifies the assemblage category with the classes
#' human remains, raw material, typology, technology, function, organic tools,
#' symbolic artifacts, feature, miscellaneous finds, paleofauna, animal remains,
#' plant remains. The argument \code{category} is a string (one item) or
#' vector of strings (one or more items); defaults to NULL.
#' @param age_min specifies the minimum age in years before present, using 1950 CE
#' as the baseline. If possible the argument \code{age_min} will be converted to an integer; defaults to NULL.
#' @param age_max specifies the maximum age in years before present, using 1950 CE
#' as the baseline. If possible the argument \code{age_max} will be converted to an integer; defaults to NULL.
#' @param assemblages specifies a data frame necessarily containing columns 
#' locality_id, assemblage_id. It can be  generated as return value of the 
#' function 'road_get_assemblages'. It can be used instead of the locality 
#' and assemblage search parameters to filter the results.
#' @param plant_remains specifies the type of plant remains. Possible entries
#' include: "pollen", "plant macroremains" etc. Run
#' \code{road_list_argument_values("plant_remains")} to display
#' possible values. The argument \code{plant_remains} is a string (one item) or vector of
#' strings; defaults to NULL.
#' @param plant_family specifies the family to which the described plant remains
#' is attributed to. Possible entries include: "Poaceae", "Typhaceae" etc.
#' Run \code{road_list_argument_values("plant_family")} to display
#' possible values. The argument \code{plant_family} is a string (one item) or
#' vector of strings; defaults to NULL.
#' @param plant_genus specifies the genus name of the taxon.
#' Run \code{road_list_argument_values("plant_genus")} to display
#' possible values. The argument \code{plant_genus} is a string (one item) or
#' vector of strings; defaults to NULL.
#' @param plant_species specifies the full and valid species name including
#' author’s name (e.g. Quercus ilex L., Sciadopitys verticillata (Thunb.)
#' Siebold & Zucc). Run \code{road_list_argument_values("plant_species")}
#' to display possible values. The argument \code{plant_species} is a string
#' (one item) or vector of strings; defaults to NULL.
#'
#' @return A data frame with plant remains information. Rows represent individual
#' plant remains finds, columns contain standard outputs and plant remains-related details on:
#' @return \code{plant_remains}: The attribute specifies the type of plant remains (e.g. pollen, plant macroremains).
#' @return \code{plant_family}, \code{plant_genus}, \code{plant_species}: The attributes specify the taxonomic classification of the plant remains.
#'
#' @export
#'
#' @examples
#' road_get_plantremains(plant_family = "Poaceae", plant_genus = "Setaria")
#' road_get_plantremains(country = c("Israel"), plant_remains = "starch")
road_get_plantremains <- function(
    continent = NULL,
    subcontinent = NULL,
    country = NULL,
    locality_type = NULL,
    cultural_period = NULL,
    technocomplex = NULL,
    category = NULL,
    age_min = NULL,
    age_max = NULL,
    assemblages = NULL,
    plant_remains = NULL,
    plant_family = NULL,
    plant_genus = NULL,
    plant_species = NULL
)
{
  # calculate assemblage_condition
  if ((!is.null(category) || !is.null(age_min) || !is.null(age_max)) && !is.null(assemblages))
    warning("No assemblage search for category or age_min/age_max is performed because a non-empty assemblage list was passed")

  if (is.null(assemblages)) assemblages <- road_get_assemblages(continent = continent, 
                                                                subcontinent = subcontinent, 
                                                                country = country, 
                                                                locality_type = locality_type, 
                                                                cultural_period = cultural_period, 
                                                                technocomplex = technocomplex, 
                                                                category = category, 
                                                                age_min = age_min, 
                                                                age_max = age_max)

  assemblage_condition <- get_assemblage_condition(query_start = " AND ", assemblages = assemblages, 
                                                   locality_id_column_name = cm_locality_idlocality, assemblage_id_column_name = cm_assemblages_idassemblage)

  # build remains/family/genus/species conditions
  #plant_genus_conjuction <- ""
  #plant_species_conjuction <- ""

  plant_remains_condition <- ""
  plant_family_condition <- ""
  plant_genus_condition <- ""
  plant_species_condition <- ""

  if (!is.null(plant_remains) && length(plant_remains) != 0)
  {
    plant_remains_condition <- parameter_to_query("AND plant_remains IN (", plant_remains, ")")
  }

  if (!is.null(plant_family) && length(plant_family) != 0)
  {
    plant_family_condition <- parameter_to_query("AND plant_family IN (", plant_family, ")")
  }

  if (!is.null(plant_genus) && length(plant_genus) != 0)
  {
    plant_genus_condition <- parameter_to_query("AND plant_genus IN (", plant_genus, ")")
  }
  if (!is.null(plant_species) && length(plant_species) != 0)
  {
    plant_species_condition <- parameter_to_query("AND plant_species IN (", plant_species, ")")
  }

  # select fields
  select_fields <- c(
    paste0("paleoflora.plantremains_idlocality AS ", cm_locality_idlocality),
    paste0("paleoflora.plantremains_idassemblage AS ", cm_assemblages_idassemblage),
    paste0("paleoflora.plantremains_plant_remains AS ", cm_paleoflora_plant_remains),
    paste0("plant_taxonomy.family AS ", cm_plant_taxonomy_family),
    paste0("plant_taxonomy.genus AS ", cm_plant_taxonomy_genus),
    paste0("plant_taxonomy.species AS ", cm_plant_taxonomy_species),
    paste0("element AS ", cm_paleoflora_element),
    paste0("abundance AS ", cm_paleoflora_abundance),
    paste0("relative_abundance AS ", cm_paleoflora_relative_abundance)
  )
  
  # combine query parts
  query <- paste(
    "SELECT DISTINCT * FROM ( SELECT DISTINCT",
    paste(select_fields, collapse = ", "),
    # paste0("paleoflora.plantremains_idlocality AS ", cm_locality_idlocality, ","),
    # paste0("paleoflora.plantremains_idassemblage AS ", cm_assemblages_idassemblage, ","),
    # paste0("paleoflora.plantremains_plant_remains AS ", cm_paleoflora_plant_remains, ","),
    # paste0("plant_taxonomy.family AS ", cm_plant_taxonomy_family, ","),
    # paste0("plant_taxonomy.genus AS ", cm_plant_taxonomy_genus, ","),
    # paste0("plant_taxonomy.species AS ", cm_plant_taxonomy_species),
    # paste0("element"),
    # paste0("abundance"),
    # paste0("relative_abundance"),
    "FROM paleoflora",
    "INNER JOIN plant_taxonomy ON paleoflora.plant_taxonomy_taxon = plant_taxonomy.taxon",
    ") as foo WHERE TRUE ",
    assemblage_condition,
    plant_remains_condition,
    plant_family_condition,
    plant_genus_condition,
    plant_species_condition,
    "ORDER BY ",
    cm_locality_idlocality,
    " ASC"
  )

  data <- road_run_query(query)

  if (nrow(data) == 0 & nrow(assemblages) > 0)
  {
    print_null_result_message(continent = continent,
                              subcontinent = subcontinent,
                              country = country,
                              locality_type = locality_type,
                              cultural_period = cultural_period,
                              technocomplex = technocomplex,
                              category = category,
                              age_min = age_min,
                              age_max = age_max,
                              plant_remains = plant_remains,
                              plant_family = plant_family,
                              plant_genus = plant_genus,
                              plant_species = plant_species)
    
  }

  data <- add_locality_columns(data, assemblages = assemblages)

  return(data)
}
