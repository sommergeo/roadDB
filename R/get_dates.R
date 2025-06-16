#' Get dates for assemblages, geolayers and archlayers from ROAD Database
#'
#' `road_get_dates` fetches records of absolute dating results and is intended for users who analyse time series.
#' The function combines queries from the ROAD tables “assemblage_age”, “geological_layer_age” and “archaeological_layer_age” and allows the results to be filtered according to a series of criteria. The results are published values for minimum and maximum age, as well as the dating method and material.
#' Background on dating context: Finds cannot always be dated directly due to the potential destruction of the object during sampling or because the materials do not meet the prerequisites for absolute dating. In such cases, the surrounding sediment or other objects, made from more suitable materials, are dated instead and associated with the find. For this reason, absolute dates are stored and linked across three different tables in ROAD: The ‘geological_layer_age’ and ‘archaeological_layer_age’ tables refer to samples from stratigraphic units, while the ‘assemblage_age’ table contains sample results taken directly from objects.
#' Background on multiple dates: It may happen that several dates are available for one object. For example, if the top and bottom of a stratigraphic unit were sampled to bracket a find contained therein. In addition, finds are sometimes sampled using different methods to obtain robust results. This can sometimes lead to significant differences in the estimated age, especially when earlier dating results are updated using more modern and advanced methods. We therefore recommend that all results are thoroughly checked against the literature. In addition, ROCEEH has developed its own model that aggregates the many absolute and relative dating methods and derives the variable “age_min” and “age_max” from them. However, these aggregated ages are not explicitly included in the “road_get_dates” function, but rather the original raw data.
#' Background on radiocarbon dating: C14 dates are uncalibrated. Please use one of the manx calibration tools provided as R libraries for further analysis (e.g. https://cran.r-project.org/web/packages/rcarbon/index.html).
#'
#' All parameters are optional and should be omitted or set to NULL when not used.
#' @param continents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param subcontinents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param countries string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param locality_types string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param cultural_periods string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param categories string (one item) or vector of strings (one or more items).
#' @param assemblages list of assemblages; return value from function `road_get_assemblages`.
#' @param dating_methods string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param material_dated string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param age_min integer; defaults to NULL.
#' @param age_max integer; defaults to NULL.
#' @param technocomplex string (one item) or vector of strings (one or more items); defaults to NULL.
#'
#' @return Database search result as dataframe with absolute dates as rows, and columns representing dating-specific information such as minimal age,  maximal age, dating method and material, as well as auxiliary information on geographic information, locality type, cultural period, technocomplex, assemblage and category.
#' @export
#'
#' @examples 
#' road_get_dates(dating_methods = c("geology", "biostratigraphy"))
#' road_get_dates(material_dated = c("coprolite", "glass", "ivory"), age_min = 10000L, 
#'                          age_max = 100000L, dating_methods = c("geology", "biostratigraphy"))
#' road_get_dates(countries = c("Germany", "France"), cultural_periods = "Middle Paleolithic")                         
road_get_dates <- function (
    continents = NULL, 
    subcontinents = NULL, 
    countries = NULL, 
    locality_types = NULL, 
    cultural_periods = NULL, 
    dating_methods = NULL, 
    material_dated = NULL, 
    age_min = NULL, 
    age_max = NULL, 
    technocomplex = NULL, 
    categories = NULL, 
    assemblages = NULL
) 
{
  if ((!is.null(age_min) && !is.integer(age_min)) || (!is.null(age_max) && !is.integer(age_max)))
    stop("Parameters 'min_age' and 'max_age' have to be integers.")
  
  if (!is.null(age_min) && !is.null(age_max) && age_min > age_max)
    stop("Parameter 'min_age' can not be bigger than 'max_age'.")
  
  # calculate assemblage_condition
  # To do: !is.null(categories or continents or...) AND !is.null(assemblages)  ---> Warnung an den Benutzer
  if (is.null(assemblages)) assemblages <- road_get_assemblages(continents = continents, 
                                                                subcontinents = subcontinents, 
                                                                countries = countries, 
                                                                locality_types = locality_types, 
                                                                cultural_periods = cultural_periods,
                                                                categories = categories)
  
  assemblage_condition <- get_assemblage_condition(query_start = "AND ", assemblages = assemblages)
  
  # select fields
  select_fields_gla <- c(
    paste0("geological_layer_age.geolayer_idlocality AS  ", cm_locality_idlocality),
    # paste0("CAST(assemblage_idassemblage AS TEXT) AS  ", cm_assemblages_idassemblage),
    paste0("assemblage_idassemblage AS ", cm_assemblages_idassemblage),
    # paste0("assemblage.category AS \"", cm_assemblages_categories, "\""),
    paste0("geological_layer_age.geolayer_name AS ", cm_geolayer_geolayer_name),
    paste0("archlayer_name AS ", cm_archlayer_archlayer_name),
    paste0("age AS ", cm_age),
    paste0("negative_standard_deviation AS ", cm_negative_standard_deviation),
    paste0("positive_standard_deviation AS ", cm_positive_standard_deviation),
    paste0("material_dated AS ", cm_material_dated),
    paste0("dating_method AS ", cm_dating_method),
    paste0("laboratory_idlaboratory AS ", cm_laboratory_idlaboratory),
    paste0("technocomplex as technocomplex")
  )
  
  select_fields_ala <- c(
    paste0("archaeological_layer_age.archlayer_idlocality AS ", cm_locality_idlocality),
    # paste0("CAST(assemblage_idassemblage AS TEXT) AS ", cm_assemblages_idassemblage),
    paste0("assemblage_idassemblage AS ", cm_assemblages_idassemblage),
    # paste0("assemblage.category AS ", cm_assemblages_categories),
    paste0("archlayer_correl_geolayer.geolayer_name AS ", cm_geolayer_geolayer_name),
    paste0("archaeological_layer_age.archlayer_name AS ", cm_archlayer_archlayer_name),
    paste0("age AS ", cm_age),
    paste0("negative_standard_deviation AS ", cm_negative_standard_deviation),
    paste0("positive_standard_deviation AS ", cm_positive_standard_deviation),
    paste0("material_dated AS ", cm_material_dated),
    paste0("dating_method AS ", cm_dating_method),
    paste0("laboratory_idlaboratory AS ", cm_laboratory_idlaboratory),
    paste0("technocomplex as technocomplex")
  )
  
  select_fields_asa <- c(
    paste0("assemblage_age.assemblage_idlocality AS ", cm_locality_idlocality),
    # paste0("CAST(assemblage_age.assemblage_idassemblage AS TEXT) AS  \"", cm_assemblages_idassemblage, "\""),
    paste0("assemblage_age.assemblage_idassemblage AS ", cm_assemblages_idassemblage),
    # paste0("NULL AS ", cm_assemblages_categories, " "),
    paste0("assemblage_in_geolayer.geolayer_name AS ", cm_geolayer_geolayer_name),
    paste0("archlayer_name AS ", cm_archlayer_archlayer_name),
    paste0("age AS ", cm_age),
    paste0("negative_standard_deviation AS ", cm_negative_standard_deviation),
    paste0("positive_standard_deviation AS ", cm_positive_standard_deviation),
    paste0("material_dated AS ", cm_material_dated),
    paste0("dating_method AS ", cm_dating_method),
    paste0("laboratory_idlaboratory AS ", cm_laboratory_idlaboratory),
    paste0("technocomplex as technocomplex")
  )
  
  query <- paste0("SELECT * FROM (SELECT ", paste(select_fields_gla, collapse = ", "),
                  " FROM geological_layer_age ",
                  "LEFT JOIN assemblage_in_geolayer ON ",
                  " assemblage_in_geolayer.geolayer_idlocality = geological_layer_age.geolayer_idlocality ",
                  " AND assemblage_in_geolayer.geolayer_name = geological_layer_age.geolayer_name ", 
                  "LEFT JOIN archlayer_correl_geolayer ON ",
                  "archlayer_correl_geolayer.geolayer_idlocality = geological_layer_age.geolayer_idlocality",
                  " AND archlayer_correl_geolayer.geolayer_name = geological_layer_age.geolayer_name ",
                  "LEFT JOIN archaeological_layer ON ",
                  "locality_idlocality = archlayer_idlocality ",
                  " AND name =  archlayer_name ",
                  "LEFT JOIN archaeological_stratigraphy ON ",
                  " archstratigraphy_idarchstrat = idarchstrat ",
                  "UNION
            SELECT ", paste(select_fields_ala, collapse = ", "), 
                  " FROM archaeological_layer_age ",
                  "LEFT JOIN archaeological_layer ON ",
                  "locality_idlocality = archlayer_idlocality ",
                  " AND name =  archlayer_name ",
                  "LEFT JOIN archaeological_stratigraphy ON ",
                  " archstratigraphy_idarchstrat = idarchstrat ",
                  "LEFT JOIN archlayer_correl_geolayer ON ",
                  "archlayer_correl_geolayer.archlayer_idlocality = archaeological_layer_age.archlayer_idlocality",
                  " AND archlayer_correl_geolayer.archlayer_name = archaeological_layer_age.archlayer_name ",
                  "LEFT JOIN assemblage_in_geolayer ON ",
                  " assemblage_in_geolayer.geolayer_idlocality = archlayer_correl_geolayer.geolayer_idlocality ",
                  " AND assemblage_in_geolayer.geolayer_name = archlayer_correl_geolayer.geolayer_name ",
                  "UNION
            SELECT ", paste(select_fields_asa, collapse = ", "),
                  " FROM assemblage_age ",
                  "LEFT JOIN assemblage_in_geolayer ON ",
                  " assemblage_in_geolayer.assemblage_idlocality = assemblage_age.assemblage_idlocality ",
                  " AND assemblage_in_geolayer.assemblage_idassemblage = assemblage_age.assemblage_idassemblage ",
                  "LEFT JOIN archlayer_correl_geolayer ON ",
                  "archlayer_correl_geolayer.geolayer_idlocality = assemblage_in_geolayer.geolayer_idlocality ",
                  " AND archlayer_correl_geolayer.geolayer_name =  assemblage_in_geolayer.geolayer_name ",
                  "LEFT JOIN  archaeological_layer ON ",
                  "locality_idlocality = archlayer_idlocality ",
                  "AND name = archlayer_name ",
                  "LEFT JOIN archaeological_stratigraphy ON ",
                  " archstratigraphy_idarchstrat = idarchstrat",
                  ") as foo ",
                  "WHERE TRUE ",
                  assemblage_condition,
                  query_check_intersection("AND ", dating_methods, "dating_method "),
                  query_check_intersection("AND ", material_dated, "material_dated "),
                  parameter_to_query("AND age  <= ", age_max, " "),
                  parameter_to_query("AND age  >= ", age_min, " "),
                  query_check_intersection("AND ", technocomplex, "technocomplex"),
                  " ORDER BY ", cm_locality_idlocality, ", ", cm_geolayer_geolayer_name, 
                  ", ", cm_archlayer_archlayer_name)
  
  data <- road_run_query(query)
  
  data <- add_locality_columns(data, assemblages = assemblages)
  
  return(data)
}
