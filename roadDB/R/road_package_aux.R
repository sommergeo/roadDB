# source("roadDB/R/login.R")
source("roadDB/R/road_package.R")
library(assertthat)
library(RPostgres)

cm_assemblages_locality_idlocality <- "locality_id"

attributes <- c("type", "continent", "continent_region", "country", "category", 
              "cultural_period", "example", "dating_method", "material_dated")
tables <- list("locality", "geopolitical_units", "geopolitical_units", "locality",  
            "assemblage", "archaeological_stratigraphy", "roadDB/attr_values/ex.txt", 
            c("geological_layer_age", "archaeological_layer_age", "assemblage_age"), 
            c("geological_layer_age", "archaeological_layer_age", "assemblage_age"))

#' Get attribute value from ROAD Database
#'
#' `road_list_values` fetches values of a given attribute in the database or 
#' read values from file
#'
#'#' All parameters are optional and should be omitted or set to NULL when not used.
#'
#' @param attribute_name name of an attribute; defaults to NULL.
#'
#' @return List of attribute values.
#' @export
#'
#' @examples road_list_values("category")
#' @examples road_list_values("cultural_period")
road_list_values <- function (attribute_name = NULL)
{
  if (is.null(attribute_name))
    return("No attribute name is given.")
  
  table <- NULL
  
  # computing length of attributes array
  size = length(attributes)
  # iterating over elements of attributes
  for (i in 1:size){
    if(attribute_name == attributes[i]) {
      table = tables[i]
    }
  }  
  # table <- c("geological_layer_age", "archaeological_layer_age", "assemblage_age")
  if (is.null(table))
    return(paste("No data source for parameter ", attribute_name, " was not found."))
  
  if (grepl(".txt", table, fixed = TRUE) && grepl("/", table, fixed = TRUE)) {
    message(table)
    
    data <- read.csv(toString(table))
    return(data)
  }
  
  # if we use tables <- list(...), all elements of the list are vectors
  #if (is.vector(table)) {
  q <- paste( "SELECT DISTINCT(unnest(string_to_array(string_agg(", attribute_name, ", ', '),', '))) as ",
              attribute_name, " from ")
  qu <- paste0(q, table)
  que <- paste(
    sapply(table, function(x) paste0(q, x)), 
    collapse = " UNION "
  )
  query <- paste0(que, " ORDER BY ", attribute_name)
  # }
  # else
  #  query <- paste( "SELECT DISTINCT(unnest(string_to_array(string_agg(", attribute_name, ", ', '),', '))) as ",
  #                 attribute_name, " from ", table, " ORDER BY ", attribute_name)

  
  # query <- paste( "SELECT DISTINCT ", attribute_name, " FROM (select distinct(unnest(string_to_array(
  # string_agg(", attribute_name, ", ', '),', '))) as ",
  # attribute_name, ", 'dummy' as dummy from ", table,  " GROUP BY dummy) as foo ", 
  # " ORDER BY ", attribute_name)
  
  data <- road_run_query(query)
  
  return(data)
}

#' Get dates for assemblages, geolayers and archlayers from ROAD Database
#'
#' `road_get_dates` fetches records of the age tables in ROAD
#'
#' All parameters are optional and should be omitted or set to NULL when not used.
#'
#' @param dating_methods; defaults to NULL.
#' @param material_dated; defaults to NULL.
#' @param age_min; defaults to NULL.
#' @param age_max; defaults to NULL.
#'
#' @return date records
#' @export
#'
#' @examples road_get_dates(c("geology", "biostratigraphy"))
road_get_dates <- function (dating_methods = NULL, material_dated = NULL, age_min = NULL, age_max = NULL)
{
  if ((!is.null(age_min) && !is.integer(age_min)) || (!is.null(age_max) && !is.integer(age_max)))
    stop("Parameters 'min_age' and 'max_age' have to be integers.")
  
  if (!is.null(age_min) && !is.null(age_max) && age_min > age_max)
    stop("Parameter 'min_age' can not be bigger than 'max_age'.")
  
  # select fields
  select_fields_gla <- c(
    paste0("geolayer_idlocality AS ", cm_assemblages_locality_idlocality),
    paste0("NULL AS ", cm_assemblages_idassemblage),
    paste0("geolayer_name AS \"", cm_assemblage_in_geolayer_geolayer_name, "\""),
    paste0("NULL AS archlayer"),
    paste0("age AS \"", cm_age, "\""),
    paste0("negative_standard_deviation"),
    paste0("positive_standard_deviation"),
    paste0("cm_material_dated"),
    paste0("dating_method"),
    paste0("laboratory_idlaboratory"),
  )
  
  query <- paste0("SELECT * FROM (SELECT geolayer_idlocality as locality, NULL as
            assemblage, geolayer_name as geolayer, NULL as archlayer, age, 
            negative_standard_deviation, positive_standard_deviation, material_dated, 
            dating_method, laboratory_idlaboratory 
            FROM geological_layer_age
            UNION
            SELECT archlayer_idlocality as locality, NULL as assemblage, NULL as geolayer, 
            archlayer_name as archlayer, age, negative_standard_deviation, 
            positive_standard_deviation, 
            material_dated, dating_method, laboratory_idlaboratory 
            FROM archaeological_layer_age
            UNION
            SELECT assemblage_idlocality, CAST(assemblage_idassemblage AS TEXT) as assemblage, NULL as geolayer, 
            NULL as archlayer, 
            age, negative_standard_deviation, positive_standard_deviation, 
            material_dated, dating_method, laboratory_idlaboratory 
            FROM assemblage_age) as foo ",
            "WHERE true ", 
            parameter_to_query("AND dating_method IN (", dating_methods, ") "),
            parameter_to_query("AND material_dated IN (", material_dated, ") "),
            parameter_to_query("AND age  <= ", age_max, " "),
            parameter_to_query("AND age  >= ", age_min, " "),
            " ORDER BY locality, geolayer, archlayer")
  
  data <- road_run_query(query)
  
  return(data)
  
}