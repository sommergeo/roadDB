# source("roadDB/R/login.R")
library(assertthat)
library(RPostgres)

attributes <- c("type", "continent", "continent_region", "country", "category", "cultural_period", "example")
tables <- c("locality", "geopolitical_units", "geopolitical_units", "locality",  "assemblage", "archaeological_stratigraphy", "attr_values/ex.txt")

#' Get attribute value from ROAD Database
#'
#' `road_list_values` fetches values of a given attribute
#'
#' @param continents string (one item) or vector of strings (one or more items); defaults to NULL.
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
  
    # computing length of attributes array
  size = length(attributes)
  # iterating over elements of attributes
  for (i in 1:size){
    if(attribute_name == attributes[i]){
      table = tables[i]
      if (grepl(".txt", table, fixed = TRUE)) {
        data <- read.csv(table)
        return(data)
      }
    }
  }  
  # query <- paste( "SELECT DISTINCT ", attribute_name, " FROM ", table, " ORDER BY ", attribute_name)
  # query <- paste( "SELECT DISTINCT ", attribute_name, " FROM (select distinct(unnest(string_to_array(
                  # string_agg(", attribute_name, ", ', '),', '))) as ",
                  # attribute_name, ", 'dummy' as dummy from ", table,  " GROUP BY dummy) as foo ", 
                  # " ORDER BY ", attribute_name)
  query <- paste( "SELECT DISTINCT(unnest(string_to_array(string_agg(", attribute_name, ", ', '),', '))) as ",
                  attribute_name, " from ", table, " ORDER BY ", attribute_name)

  data <- road_run_query(query)
  
  return(data)
}

#' Get dates for assemblages, geolayers and archlayers from ROAD Database
#'
#' `road_get_dates` fetches values of a given attribute
#'
#' @param .
#'
#' @return dates
#' @export
#'
#' @examples road_get_dates()
road_get_dates <- function ()
{
  #query <- "SELECT DISTINCT on (assemblage.locality_idlocality, assemblage.name, 
  #          geological_layer_age.age, geological_layer_age.material_dated, 
  #          geological_layer_age.dating_method, geological_layer_age.laboratory_idlaboratory) 
  #          assemblage.locality_idlocality, assemblage.name, geological_layer_age.age, 
  #          geological_layer_age.material_dated, geological_layer_age.dating_method, 
  #          geological_layer_age.laboratory_idlaboratory 
  #          FROM assemblage, assemblage_in_geolayer, geological_layer_age 
  #          WHERE (assemblage.locality_idlocality = assemblage_in_geolayer.assemblage_idlocality 
  #          and assemblage.idassemblage = assemblage_in_geolayer.assemblage_idassemblage 
  #          and assemblage_in_geolayer.geolayer_idlocality = geological_layer_age.geolayer_idlocality 
  #          and assemblage_in_geolayer.geolayer_name = geological_layer_age.geolayer_name)
  #          UNION 
  #          SELECT DISTINCT on (assemblage.locality_idlocality, assemblage.name, 
  #          archaeological_layer_age.age, archaeological_layer_age.material_dated, 
  #          archaeological_layer_age.dating_method, archaeological_layer_age.laboratory_idlaboratory) 
  #          assemblage.locality_idlocality, assemblage.name, archaeological_layer_age.age, 
  #          archaeological_layer_age.material_dated, archaeological_layer_age.dating_method, 
  #          archaeological_layer_age.laboratory_idlaboratory 
  #          FROM assemblage, assemblage_in_archlayer, archaeological_layer_age 
  #          WHERE (assemblage.locality_idlocality = assemblage_in_archlayer.assemblage_idlocality 
  #          and assemblage.idassemblage = assemblage_in_archlayer.assemblage_idassemblage 
  #          and assemblage_in_archlayer.archlayer_idlocality = archaeological_layer_age.archlayer_idlocality 
  #          and assemblage_in_archlayer.archlayer_name = archaeological_layer_age.archlayer_name)"
  
  query <- "SELECT * FROM (SELECT geolayer_idlocality as locality, -1 as assemblage, geolayer_name as geolayer, '-' as archlayer, age, negative_standard_deviation, positive_standard_deviation, 
            material_dated, dating_method, laboratory_idlaboratory 
            FROM geological_layer_age
            UNION
            SELECT archlayer_idlocality as locality, -1 as assemblage, '-' as geolayer, archlayer_name as archlayer, age, negative_standard_deviation, positive_standard_deviation, 
            material_dated, dating_method, laboratory_idlaboratory 
            FROM archaeological_layer_age
            UNION
            SELECT assemblage_idlocality, assemblage_idassemblage as assemblage, '-' as geolayer, '-' as archlayer, 
            age, negative_standard_deviation, positive_standard_deviation, 
            material_dated, dating_method, laboratory_idlaboratory 
            FROM assemblage_age) as foo ORDER BY locality, geolayer, archlayer"
  
  
  
  data <- road_run_query(query)
  
  return(data)
  
}