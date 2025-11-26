#' Retrieve Dating Information for Assemblages, Geolayers, and Archlayers from the ROAD Database
#'
#' The  \strong{\code{road_get_dates}} function retrieves absolute dating records for 
#' assemblages, geological layers, and archaeological layers from the ROAD database. 
#' 
# @param continent specifies the continent(s) (e.g. Africa, Europe, Asia).
# Run \code{road_list_argument_values("continent")} to display possible values.
# The argument \code{continent} is a string (one item) or vector of strings 
# (one or more items); defaults to NULL.
# @param subcontinent specifies the continental region(s) (e.g. Southern Europe). 
# Run \code{road_list_argument_values("subcontinent")} to display possible values.
# The argument \code{subcontinent} is a string (one item) or vector of strings 
# (one or more items); defaults to NULL.
# @param country specifies the name of the country where a locality is situated 
# (e.g. Germany, Kenya, Saudi Arabia). Run \code{road_list_argument_values("country")} 
# to display possible values.
# The argument \code{country} is a string (one item) or vector of strings 
# (one or more items); defaults to NULL.
# @param locality_type specifies the type of locality (e.g. cave, rockshelter, open air).
# Run \code{road_list_argument_values("locality_type")} to display possible values.
# The argument \code{locality_type} is a string (one item) or vector of strings 
# (one or more items); defaults to NULL.
# @param cultural_period specifies the main cultural epoch(s) and includes the 
# Eurasian Paleolithic (Lower, Middle, Upper, Epi-) and the African Stone Age 
# (Earlier, Middle, Later). Run \code{road_list_argument_values("cultural_period")} 
# to display possible values. The argument \code{cultural_period} is a string 
# (one item) or vector of strings (one or more items); defaults to NULL.
# @param technocomplex specifies an archaeological culture or named stone tool 
# industry (e.g. Oldowan, Acheulean, Mousterian).
# Run \code{road_list_argument_values("technocomplex")} to display possible values.
# The argument \code{technocomplex} is a string (one item) or vector of strings 
# (one or more items); defaults to NULL.
# @param category specifies the assemblage category with the classes 
# human remains, raw material, typology, technology, function, organic tools, 
# symbolic artifacts, feature, miscellaneous finds, paleofauna, animal remains, 
# plant remains. The argument \code{category} is a string (one item) or 
# vector of strings (one or more items); defaults to NULL.
# @param age_min specifies the minimum age in years before present, using 1950 CE
# as the baseline. If possible the argument \code{age_min} will be converted to an integer; defaults to NULL.
# @param age_max specifies the maximum age in years before present, using 1950 CE
# as the baseline. If possible the argument \code{age_max} will be converted to an integer; defaults to NULL.
#' @param assemblages specifies a data frame necessarily containing columns 
#' locality_id, assemblage_id. It can be  generated as return value of the 
#' function 'road_get_assemblages'. It can be used instead of the locality 
#' and assemblage search parameters to filter the results.
# @param dating_methods specifies the method of temporal analysis (e.g. 14C, OSL, IRSL).
# Run \code{road_list_argument_values("dating_methods")} to display possible values.
# The argument \code{dating_methods} is a string (one item) or vector of strings 
# (one or more items); defaults to NULL.
# @param material_dated (string) specifies the material used for temporal analysis 
# (e.g. bone, tooth, antler). Run \code{road_list_argument_values("material_dated")} 
# to display possible values. The argument \code{datied_material} is a string 
# (one item) or vector of strings (one or more items); defaults to NULL.

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
# @examples 
# road_get_dates(dating_methods = c("14C (radiocarbon) dating", "U series (uranium-thorium) dating"))
# road_get_dates(material_dated = c("coprolite", "ivory"),
#                age_min = 10000L,
#                age_max = 50000L,
#                dating_method = "14C (radiocarbon) dating")
# road_get_dates(country = c("Germany", "France"),
#                cultural_period = "Middle Paleolithic")
                                      


# road_get_dates <- function (
#     continent = NULL, 
#     subcontinent = NULL, 
#     country = NULL, 
#     locality_type = NULL, 
#     cultural_period = NULL, 
#     technocomplex = NULL,
#     dating_methods = NULL, 
#     material_dated = NULL, 
#     age_min = NULL, 
#     age_max = NULL, 
#     category = NULL, 
#     assemblages = NULL
# ) 
# {
#   if ((!is.null(age_min) && !is.integer(age_min)) || (!is.null(age_max) && !is.integer(age_max)))
#     stop("Parameters 'min_age' and 'max_age' have to be integers.")
#   
#   if (!is.null(age_min) && !is.null(age_max) && age_min > age_max)
#     stop("Parameter 'min_age' can not be bigger than 'max_age'.")
#   
#   # calculate assemblage_condition
#   # To do: !is.null(category or continent or...) AND !is.null(assemblages)  ---> Warnung an den Benutzer
#   if (is.null(assemblages)) assemblages <- road_get_assemblages(continent = continent, 
#                                                                 subcontinent = subcontinent, 
#                                                                 country = country, 
#                                                                 locality_type = locality_type, 
#                                                                 cultural_period = cultural_period,
#                                                                 category = category)
#   
#   assemblage_condition <- get_assemblage_condition(query_start = "AND ", assemblages = assemblages)
#   
#   # select fields
#   select_fields_gla <- c(
#     paste0("geological_layer_age.geolayer_idlocality AS  ", cm_locality_idlocality),
#     # paste0("CAST(assemblage_idassemblage AS TEXT) AS  ", cm_assemblages_idassemblage),
#     paste0("assemblage_idassemblage AS ", cm_assemblages_idassemblage),
#     # paste0("assemblage.category AS \"", cm_assemblages_category, "\""),
#     paste0("geological_layer_age.geolayer_name AS ", cm_geolayer_geolayer_name),
#     paste0("archlayer_name AS ", cm_archlayer_archlayer_name),
#     paste0("age AS ", cm_age),
#     paste0("negative_standard_deviation AS ", cm_negative_standard_deviation),
#     paste0("positive_standard_deviation AS ", cm_positive_standard_deviation),
#     paste0("material_dated AS ", cm_material_dated),
#     paste0("dating_method AS ", cm_dating_method),
#     paste0("laboratory_idlaboratory AS ", cm_laboratory_idlaboratory),
#     paste0("technocomplex as technocomplex")
#   )
#   
#   select_fields_ala <- c(
#     paste0("archaeological_layer_age.archlayer_idlocality AS ", cm_locality_idlocality),
#     # paste0("CAST(assemblage_idassemblage AS TEXT) AS ", cm_assemblages_idassemblage),
#     paste0("assemblage_idassemblage AS ", cm_assemblages_idassemblage),
#     # paste0("assemblage.category AS ", cm_assemblages_category),
#     paste0("archlayer_correl_geolayer.geolayer_name AS ", cm_geolayer_geolayer_name),
#     paste0("archaeological_layer_age.archlayer_name AS ", cm_archlayer_archlayer_name),
#     paste0("age AS ", cm_age),
#     paste0("negative_standard_deviation AS ", cm_negative_standard_deviation),
#     paste0("positive_standard_deviation AS ", cm_positive_standard_deviation),
#     paste0("material_dated AS ", cm_material_dated),
#     paste0("dating_method AS ", cm_dating_method),
#     paste0("laboratory_idlaboratory AS ", cm_laboratory_idlaboratory),
#     paste0("technocomplex as technocomplex")
#   )
#   
#   select_fields_asa <- c(
#     paste0("assemblage_age.assemblage_idlocality AS ", cm_locality_idlocality),
#     # paste0("CAST(assemblage_age.assemblage_idassemblage AS TEXT) AS  \"", cm_assemblages_idassemblage, "\""),
#     paste0("assemblage_age.assemblage_idassemblage AS ", cm_assemblages_idassemblage),
#     # paste0("NULL AS ", cm_assemblages_category, " "),
#     paste0("assemblage_in_geolayer.geolayer_name AS ", cm_geolayer_geolayer_name),
#     paste0("archlayer_name AS ", cm_archlayer_archlayer_name),
#     paste0("age AS ", cm_age),
#     paste0("negative_standard_deviation AS ", cm_negative_standard_deviation),
#     paste0("positive_standard_deviation AS ", cm_positive_standard_deviation),
#     paste0("material_dated AS ", cm_material_dated),
#     paste0("dating_method AS ", cm_dating_method),
#     paste0("laboratory_idlaboratory AS ", cm_laboratory_idlaboratory),
#     paste0("technocomplex as technocomplex")
#   )
#   
#   query <- paste0("SELECT * FROM (SELECT ", paste(select_fields_gla, collapse = ", "),
#                   " FROM geological_layer_age ",
#                   "LEFT JOIN assemblage_in_geolayer ON ",
#                   " assemblage_in_geolayer.geolayer_idlocality = geological_layer_age.geolayer_idlocality ",
#                   " AND assemblage_in_geolayer.geolayer_name = geological_layer_age.geolayer_name ", 
#                   "LEFT JOIN archlayer_correl_geolayer ON ",
#                   "archlayer_correl_geolayer.geolayer_idlocality = geological_layer_age.geolayer_idlocality",
#                   " AND archlayer_correl_geolayer.geolayer_name = geological_layer_age.geolayer_name ",
#                   "LEFT JOIN archaeological_layer ON ",
#                   "locality_idlocality = archlayer_idlocality ",
#                   " AND name =  archlayer_name ",
#                   "LEFT JOIN archaeological_stratigraphy ON ",
#                   " archstratigraphy_idarchstrat = idarchstrat ",
#                   "UNION
#             SELECT ", paste(select_fields_ala, collapse = ", "), 
#                   " FROM archaeological_layer_age ",
#                   "LEFT JOIN archaeological_layer ON ",
#                   "locality_idlocality = archlayer_idlocality ",
#                   " AND name =  archlayer_name ",
#                   "LEFT JOIN archaeological_stratigraphy ON ",
#                   " archstratigraphy_idarchstrat = idarchstrat ",
#                   "LEFT JOIN archlayer_correl_geolayer ON ",
#                   "archlayer_correl_geolayer.archlayer_idlocality = archaeological_layer_age.archlayer_idlocality",
#                   " AND archlayer_correl_geolayer.archlayer_name = archaeological_layer_age.archlayer_name ",
#                   "LEFT JOIN assemblage_in_geolayer ON ",
#                   " assemblage_in_geolayer.geolayer_idlocality = archlayer_correl_geolayer.geolayer_idlocality ",
#                   " AND assemblage_in_geolayer.geolayer_name = archlayer_correl_geolayer.geolayer_name ",
#                   "UNION
#             SELECT ", paste(select_fields_asa, collapse = ", "),
#                   " FROM assemblage_age ",
#                   "LEFT JOIN assemblage_in_geolayer ON ",
#                   " assemblage_in_geolayer.assemblage_idlocality = assemblage_age.assemblage_idlocality ",
#                   " AND assemblage_in_geolayer.assemblage_idassemblage = assemblage_age.assemblage_idassemblage ",
#                   "LEFT JOIN archlayer_correl_geolayer ON ",
#                   "archlayer_correl_geolayer.geolayer_idlocality = assemblage_in_geolayer.geolayer_idlocality ",
#                   " AND archlayer_correl_geolayer.geolayer_name =  assemblage_in_geolayer.geolayer_name ",
#                   "LEFT JOIN  archaeological_layer ON ",
#                   "locality_idlocality = archlayer_idlocality ",
#                   "AND name = archlayer_name ",
#                   "LEFT JOIN archaeological_stratigraphy ON ",
#                   " archstratigraphy_idarchstrat = idarchstrat",
#                   ") as foo ",
#                   "WHERE TRUE ",
#                   assemblage_condition,
#                   query_check_intersection("AND ", dating_methods, "dating_method "),
#                   query_check_intersection("AND ", material_dated, "material_dated "),
#                   parameter_to_query("AND age  <= ", age_max, " "),
#                   parameter_to_query("AND age  >= ", age_min, " "),
#                   query_check_intersection("AND ", technocomplex, "technocomplex"),
#                   " ORDER BY ", cm_locality_idlocality, ", ", cm_geolayer_geolayer_name, 
#                   ", ", cm_archlayer_archlayer_name)
#   
#   data <- road_run_query(query)
#   
#   data <- add_locality_columns(data, assemblages = assemblages)
#   
#   return(data)
# }
# 
road_get_dates <- function (assemblages = NULL) 
{
  localities <- road_get_localities_internal()

  if (is.null(assemblages))
  {
    assemblage_condition <- ''
    geolayer_condition <- ''
    archlayer_condition <- ''
  }
  else
  {
    assemblage_condition <- get_assemblage_condition(query_start = "AND ", assemblages = assemblages)
    geolayer_condition <- get_geolayer_condition(query_start = "AND ", assemblages = assemblages)
    archlayer_condition <- get_archlayer_condition(query_start = "AND ", assemblages = assemblages)
  }
    
  # select fields
  select_fields_gla <- c(
    paste0("geological_layer_age.geolayer_idlocality AS  ", cm_locality_idlocality),
    paste0("-1 AS ", cm_assemblages_idassemblage),
    paste0("geological_layer_age.geolayer_name AS ", cm_geolayer_geolayer_name),
    paste0("'-1' AS ", cm_archlayer_archlayer_name),
    paste0("age AS ", cm_age),
    paste0("negative_standard_deviation AS ", cm_negative_standard_deviation),
    paste0("positive_standard_deviation AS ", cm_positive_standard_deviation),
    paste0("material_dated AS ", cm_material_dated),
    paste0("dating_method AS ", cm_dating_method),
    paste0("laboratory_idlaboratory AS ", cm_laboratory_idlaboratory)
  )
  
  select_fields_ala <- c(
    paste0("archaeological_layer_age.archlayer_idlocality AS ", cm_locality_idlocality),
    paste0("-1 AS ", cm_assemblages_idassemblage),
    paste0("'-1' AS ", cm_geolayer_geolayer_name),
    paste0("archaeological_layer_age.archlayer_name AS ", cm_archlayer_archlayer_name),
    paste0("age AS ", cm_age),
    paste0("negative_standard_deviation AS ", cm_negative_standard_deviation),
    paste0("positive_standard_deviation AS ", cm_positive_standard_deviation),
    paste0("material_dated AS ", cm_material_dated),
    paste0("dating_method AS ", cm_dating_method),
    paste0("laboratory_idlaboratory AS ", cm_laboratory_idlaboratory)
  )
  
  select_fields_asa <- c(
    paste0("assemblage_age.assemblage_idlocality AS ", cm_locality_idlocality),
    paste0("assemblage_age.assemblage_idassemblage AS ", cm_assemblages_idassemblage),
    paste0("'-1' AS ", cm_geolayer_geolayer_name),
    paste0("'-1' AS ", cm_archlayer_archlayer_name),
    paste0("age AS ", cm_age),
    paste0("negative_standard_deviation AS ", cm_negative_standard_deviation),
    paste0("positive_standard_deviation AS ", cm_positive_standard_deviation),
    paste0("material_dated AS ", cm_material_dated),
    paste0("dating_method AS ", cm_dating_method),
    paste0("laboratory_idlaboratory AS ", cm_laboratory_idlaboratory)
  )
  
  query <- paste0("SELECT * FROM (SELECT * FROM (SELECT ", paste(select_fields_gla, collapse = ", "),
                  " FROM geological_layer_age) as fooo ", "WHERE TRUE ", geolayer_condition,
                  " UNION
            SELECT * FROM (SELECT ", paste(select_fields_ala, collapse = ", "), 
                  " FROM archaeological_layer_age) as foooo ", "WHERE TRUE ", archlayer_condition,
                  " UNION
            SELECT * FROM (SELECT ", paste(select_fields_asa, collapse = ", "),
                  " FROM assemblage_age) as fooooo ", "WHERE TRUE ", assemblage_condition,
                  ") as foo ",
                  " ORDER BY ", cm_locality_idlocality)
  
  data <- road_run_query(query)
  
  # data <- add_locality_columns(data, assemblages = assemblages)
  data <- add_locality_columns(data, localities = localities)
  
  return(data)
}
