source("roadDB/R/login.R")
library(assertthat)
library(RPostgres)

attributes <- c("type", "continent", "continent_region", "country")
tables <- c("locality", "geopolitical_units", "geopolitical_units", "geopolitical_units")

road_list_values <- function (attribute_name = NULL)
{
  # computing length of attributes
  size = length(attributes)
  # iterating over elements of attributes
  for (i in 1:size){
    if(attribute_name == attributes[i]){
      table = tables[i]
    }
  }  
  query <- paste( "SELECT DISTINCT ", attribute_name, " FROM ", table, " ORDER BY ", attribute_name)
  
  data <- road_run_query(query)
  
  return(data)
}

