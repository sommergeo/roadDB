#' Get paleobotany data from ROAD database
#'
#' `road_get_plantremains` fetches data of paleobotanical remains from the ROAD database.
#'
#' Paleobotanical remains are plant remains found in archaeological contexts and are associated with
#' assemblages. This function allows you to query paleobotanical data based on
#' various parameters such as geographical location, cultural periods, plant taxonomy,
#' and assemblages. Use the parameters to filter the results or omit them to retrieve a broader dataset.
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
#' @param age_min specifies the minimum age of assemblage. The parameter age_min is an integer.
#' @param age_max specifies the maximum age of assemblage. The parameter age_max is an integer.
#' @param assemblages list of assemblages; return value from function `road_get_assemblages`.
#' Can be used instead of the other locality and assemblage parameters to filter the results.
#' @param plant_remains specifies the type of plant remains. Possible entries include: "pollen", "plant macroremains" etc.
#' The parameter family is a string (one item) or vector of strings (one or more items).
#' @param plant_family specifies the family to which the described plant remains is attributed to.
#' Possible entries include: "Poaceae", "Typhaceae" etc.
#' The parameter family is a string (one item) or vector of strings (one or more items).
#' @param fauna_genus specifies the genus to which the described faunal remains is attributed to.
#' Possible entries include: "Setaria", "Typha" etc.
#' The parameter genus is a string (one item) or vector of strings (one or more items).
#' @param fauna_species specifies the species to which the described faunal remains is attributed. Possible entries include:
#' "Setaria pumila (Poir.) Roem. & Schult.", "Typha angustifolia L." etc.
#' The parameter species is a string (one item) or vector of strings (one or more items).
#'
#' @return Database search result as a list of assemblages with paleobotanical remains.
#' @export
#'
#' @examples
#' road_get_plantremains(plant_family = "Poaceae", plant_genus = "Setaria")
#' road_get_plantremains(countries = c("Germany", "France"), plant_remains = "pollen")
road_get_plantremains <- function(
    continents = NULL,
    subcontinents = NULL,
    countries = NULL,
    locality_types = NULL,
    cultural_periods = NULL,
    categories = NULL,
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
  if ((!is.null(categories) || !is.null(age_min) || !is.null(age_max)) && !is.null(assemblages))
    warning("No assemblage search for categories or age_min/age_max is performed because a non-empty assemblage list was passed")

  if (is.null(assemblages)) assemblages <- road_get_assemblages(continents, subcontinents, countries, locality_types, cultural_periods, categories, age_min, age_max)

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

  # combine query parts
  query <- paste(
    "SELECT DISTINCT * FROM ( SELECT DISTINCT",
    paste0("paleoflora.plantremains_idlocality AS ", cm_locality_idlocality, ","),
    paste0("paleoflora.plantremains_idassemblage AS ", cm_assemblages_idassemblage, ","),
    paste0("paleoflora.plantremains_plant_remains AS ", cm_paleoflora_plant_remains, ","),
    paste0("plant_taxonomy.family AS ", cm_plant_taxonomy_family, ","),
    paste0("plant_taxonomy.genus AS ", cm_plant_taxonomy_genus, ","),
    paste0("plant_taxonomy.species AS ", cm_plant_taxonomy_species),
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
    continents_str <- ifelse(is.null(continents), "", paste("continents =", toString(continents)))
    subcontinents_str <- ifelse(is.null(subcontinents), "", paste("subcontinents =", toString(subcontinents)))
    countries_str <- ifelse(is.null(countries), "", paste("countries =", toString(countries)))
    locality_types_str <- ifelse(is.null(locality_types), "", paste("locality_types =", toString(locality_types)))
    cultural_periods_str <- ifelse(is.null(cultural_periods), "", paste("cultural_periods =", toString(cultural_periods)))
    
    categories_str <- ifelse(is.null(categories), "", paste("categories =", toString(categories)))
    age_min_str <- ifelse(is.null(age_min), "", paste("age_min =", age_min))
    age_max_str <- ifelse(is.null(age_max), "", paste("age_max =", age_max))
    
    plant_remains_str <- ifelse(is.null(plant_remains), "", paste("plant_remains = (", toString(plant_remains), ")"))
    plant_family_str <- ifelse(is.null(plant_family), "", paste("plant_family = (", toString(plant_family), ")"))
    plant_genus_str <- ifelse(is.null(plant_genus), "", paste("plant_genus = (", toString(plant_genus), ")"))
    plant_species_str <- ifelse(is.null(plant_species), "", paste("plant_species = (", toString(plant_species), ")"))

    message(paste("One or more of the following used parameters caused the empty result set:
                  ",
                  continents_str,
                  subcontinents_str,
                  countries_str,
                  locality_types_str,
                  cultural_periods_str,
                  categories_str,
                  age_min_str,
                  age_max_str,
                  plant_remains_str,
                  plant_family_str,
                  plant_genus_str,
                  plant_species_str,
                  "
       Please keep in mind, the data search needs exact parameter values. To get exact values for a given parameter 'p' you can use the function road_list_parameter_values('p')."))
    
    
    
    if ((is.vector(plant_remains) && is.vector(plant_family) && is.vector(plant_genus) && is.vector(plant_species))
        || (is.vector(plant_remains) && is.vector(plant_family) && is.vector(plant_genus))
        || (is.vector(plant_family) && is.vector(plant_genus) && is.vector(plant_species))
        || (is.vector(plant_remains) && is.vector(plant_family) && is.vector(plant_species))
        || (is.vector(plant_remains) && is.vector(plant_genus) && is.vector(plant_species))
        || (is.vector(plant_remains) && is.vector(plant_family))
        || (is.vector(plant_family) && is.vector(plant_genus))
        || (is.vector(plant_genus) && is.vector(plant_species))
        || (is.vector(plant_remains) && is.vector(plant_genus))
        || (is.vector(plant_family) && is.vector(plant_species))
        || (is.vector(plant_remains) && is.vector(plant_species))
    )
    {
      if (is.null(plant_remains))
      {
        plant_remains_out <- c("")
        pr <- ""
      }
      else
      {
        pr <- "plant_remains "
        plant_remains_out <- plant_remains
      }
      if (is.null(plant_family))
      {
        plant_family_out <- c("")
        pf <- ""
      }
      else
      {
        plant_family_out <- plant_family
        pf <- "plant_family "
      }
      if (is.null(plant_genus))
      {
        plant_genus_out <- c("")
        pg <- ""
      }
      else
      {
        plant_genus_out <- plant_genus
        pg <- "plant_genus "
      }
      if (is.null(plant_species))
      {
        plant_species_out <- c("")
        ps <- ""
      }
      else
      {
        plant_species_out <- plant_species
        ps <- "plant_species"
      }
      cp <- expand.grid(remains = plant_remains_out, family = plant_family_out, genus = plant_genus_out, species = plant_species_out)
      
      cp <- cp %>% mutate(remains_family_genus_species = paste(remains, family, genus, species, sep = " "))
      s <- paste(cp$remains_family_genus_species, collapse = "; ")
      message(paste0("
      Please keep in mind at least one of the the following combinations (", pr, pf, pg, ps, ")"," have to be in the database:
                  ", s))
    }
  }

  data <- add_locality_columns(data, assemblages = assemblages)

  return(data)
}
