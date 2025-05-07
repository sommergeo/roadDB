#' Get paleobotany data from ROAD database
#'
#' `road_get_plantremains` fetches data of paleobotanical remains from the ROAD database.
#'
#' Paleobotanical remains are plant remains found in archaeological contexts. This function allows you to query
#' paleobotanical data based on various parameters such as geographical location, cultural periods, plant taxonomy,
#' and assemblages. Use the parameters to filter the results or omit them to retrieve broader results.
#'
#' @param assemblages list of assemblages; return value from function `road_get_assemblages`.
#' @param continents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param subcontinents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param countries string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param locality_types string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param cultural_periods string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param categories string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param age_min integer; minimum age of paleobotanical remains.
#' @param age_max integer; maximum age of paleobotanical remains.
#' @param plant_remains string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param plant_family string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param plant_genus string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param plant_species string (one item) or vector of strings (one or more items); defaults to NULL.
#'
#' @return Database search result as a list of assemblages with paleobotanical remains.
#' @export
#'
#' @examples road_get_plantremains(countries = c("Germany", "France"), plant_family = "Poaceae")
#' @examples road_get_plantremains(continents = "Europe", cultural_periods = "Neolithic", plant_genus = "Triticum")
#' @examples road_get_plantremains(categories = "plant remains", age_min = 5000L, age_max = 10000L)
road_get_plantremains <- function(
    assemblages = NULL,
    continents = NULL,
    subcontinents = NULL,
    countries = NULL,
    locality_types = NULL,
    cultural_periods = NULL,
    categories = NULL,
    age_min = NULL,
    age_max = NULL,
    plant_remains = NULL,
    plant_family = NULL,
    plant_genus = NULL,
    plant_species = NULL
)
{  
  # calculate assemblage_condition
  if ((!is.null(categories) | !is.null(age_min) | !is.null(age_max)) & !is.null(assemblages)) 
    warning("No assemblage search for categories or age_min/age_max is performed because a non-empty assemblage list was passed")

  if (is.null(assemblages))
  {
    # run `road_get_assemblages` else preselected list of assemblages is used
    assemblages <- road_get_assemblages(continents,
                                        subcontinents, 
                                        countries, 
                                        locality_types, 
                                        cultural_periods, 
                                        categories, 
                                        age_min, 
                                        age_max)
  }
  assemblage_condition <- get_assemblage_condition(assemblages = assemblages, locality_id_column_name = "paleoflora.plantremains_idlocality", assemblage_id_column_name = "paleoflora.plantremains_idassemblage")

  plant_genus_conjuction <- ""
  plant_species_conjuction <- ""
  if (!is.null(plant_genus))
  {
    if (is.null(plant_family))
      plant_genus_conjuction <- "AND"
    else
      plant_genus_conjuction <- "OR"
  }
  if (!is.null(plant_species))
  {
    if (is.null(plant_family) && is.null(plant_genus))
      plant_species_conjuction <- "AND"
    else
      plant_species_conjuction <- "OR"
  }

  # combine query parts
  query <- paste(
    "SELECT DISTINCT",
    paste0("paleoflora.plantremains_idlocality AS ", cm_locality_idlocality, ","),
    paste0("paleoflora.plantremains_idassemblage AS ", cm_assemblages_idassemblage, ","),
    paste0("paleoflora.plantremains_plant_remains AS ", cm_paleoflora_plant_remains, ","),
    paste0("plant_taxonomy.family AS ", cm_plant_taxonomy_family, ","),
    paste0("plant_taxonomy.genus AS ", cm_plant_taxonomy_genus, ","),
    paste0("plant_taxonomy.species AS ", cm_plant_taxonomy_species),
    "FROM paleoflora",
    "INNER JOIN plant_taxonomy ON paleoflora.plant_taxonomy_taxon = plant_taxonomy.taxon",
    "WHERE",
    assemblage_condition,
    parameter_to_query("AND paleoflora.plantremains_plant_remains IN (", plant_remains, ")"),
    parameter_to_query("AND plant_taxonomy.family IN (", plant_family, ")"),
    plant_genus_conjuction,
    parameter_to_query("plant_taxonomy.genus IN (", plant_genus, ")"),
    plant_species_conjuction,
    parameter_to_query("plant_taxonomy.species IN (", plant_species, ")"),
    "ORDER BY paleoflora.plantremains_idlocality ASC"
  )

  
  data <- road_run_query(query)

  data <- add_locality_columns(data, assemblages = assemblages)

  return(data)
}
