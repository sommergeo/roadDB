#' Retrieve Dating Information for Assemblages, Geological and Archaeological Layers from the ROAD Database
#'
#' The \strong{\code{road_get_dates}} function retrieves absolute dating records for 
#' assemblages, geological layers, and archaeological layers from the ROAD database. 
#' 
#' @param assemblages specifies a data frame with the subset of assemblages, for
#' which dating information should be retrieved. It must necessarily contain the
#' columns \code{locality_id} and \code{assemblage_id} (e.g., the output of 
#' \code{road_get_assemblages()} and other functions with this level of detail).
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
#' (see: \url{https://cran.r-project.org/package=rcarbon}).
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
#' assemblages <- road_get_assemblages(country="Slovenia")
#' road_get_dates(assemblages)
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
  
  #message(query)
  
  # data <- add_locality_columns(data, assemblages = assemblages)
  data <- add_locality_columns(data, localities = localities)
  
  return(data)
}
