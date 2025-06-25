#' Retrieve Dating Information for Assemblages, Geolayers, and Archlayers from the ROAD Database
#'
#' The \code{road_get_dates} function retrieves absolute dating records for 
#' assemblages, geological layers, and archaeological layers from the ROAD database. 
#' 
#' @param continents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param subcontinents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param countries string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param locality_types string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param cultural_periods string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param categories string (one item) or vector of strings (one or more items).
#' @param assemblages list of assemblages; return value from function \code{road_get_assemblages}.
#' @param dating_methods string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param material_dated string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param age_min integer; defaults to NULL.
#' @param age_max integer; defaults to NULL.
#' @param technocomplex string (one item) or vector of strings (one or more items); defaults to NULL.
#'
#' @details
#' The function is designed for users analyzing time series and allows for 
#' filtering results based on various criteria.
#'
#' @details
#' \strong{Background on Dating Context:}
#' Not all finds can be directly dated, either due to potential damage during
#' sampling or because the materials are unsuitable for absolute dating. In such
#' cases, the surrounding sediment or other objects—made from more suitable
#' materials—are dated and associated with the find. As a result, absolute dates
#' are stored in three different tables within ROAD:
#' \itemize{
#'    \item \code{geological_layer_age}: Dates from samples taken from geological layers.
#'    \item \code{archaeological_layer_age}: Dates from samples taken from archaeological layers.
#'    \item \code{assemblage_age}: Dates from samples taken directly from objects.
#'    }
#'
#' \strong{Background on multiple dates:} Multiple dates may be available for a
#' single object. For example, both the top and bottom of a stratigraphic unit 
#' may be sampled to bracket the age of a find. Different dating methods might 
#' also be used to obtain robust results, which can lead to varying age estimates. 
#' This is especially true when earlier dating methods are updated with more 
#' modern techniques. It is recommended to cross-check all results against 
#' relevant literature. 
#'
#' \strong{Background on aggregated ages:} The ROCEEH project has developed its 
#' own model to aggregate both absolute and relative dating methods to derive 
#' the variables \code{age_min} and \code{age_max}. These aggregated ages are 
#' not included in the \code{road_get_dates} function, which only returns raw data.
#'
#' \strong{Background on radiocarbon dating:} Please note that C14 dates are 
#' uncalibrated. For further analysis, use one of the available calibration 
#' tools, such as the \pkg{rcarbon} package available on CRAN 
#' (see: \url{https://cran.r-project.org/web/packages/rcarbon/index.html}).
#'
#' All parameters are optional. If not used, omit them or set them to NULL.
#'
#' @return A data frame with absolute dating information. Rows represent individual dates, columns contain standard outputs and dating-related details on:
#' @return \code{age}: The attribute specifies the result of the dating analysis as reported by the laboratory in years before present (BP). Note that 14C dates are \strong{uncalibarated}.
#' @return \code{negative_standard_deviation} & \code{positive_standard_deviation}: The attributes specify the positive and negative standard deviation of the dating analysis in years.
#' @return \code{material_dated}: The attribute specifies the general type of material analyzed (e.g. bone, tooth, antler etc.).
#' @return \code{dating_method}: The attribute specifies the method of analysis (e.g. 14C, OSL, IRSL, etc.).
#' @return \code{laboratory_idlaboratory}: The attribute is the official abbreviation for the designated analytical laboratory.
#' 
#' @export
#' 
#' @examples 
#' road_get_dates(dating_methods = c("14C (radiocarbon) dating", "U series (uranium-thorium) dating"))
#' road_get_dates(material_dated = c("coprolite", "ivory"), 
#'                age_min = 10000L, 
#'                age_max = 50000L, 
#'                dating_method = "14C (radiocarbon) dating")
#' road_get_dates(countries = c("Germany", "France"), 
#'                cultural_periods = "Middle Paleolithic")                         
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
