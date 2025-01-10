source("roadDB/R/login.R")
library(assertthat)
library(RPostgres)

attributes <- c("type", "continent", "continent_region", "country", "category", "cultural_period")
tables <- c("locality", "geopolitical_units", "geopolitical_units", "locality",  "assemblage", "archaeological_stratigraphy")


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
  # computing length of attributes array
  size = length(attributes)
  # iterating over elements of attributes
  for (i in 1:size){
    if(attribute_name == attributes[i]){
      table = tables[i]
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

