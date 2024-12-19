source("roadDB/R/login.R")
library(assertthat)
library(RPostgres)

attributes <- c("type", "continent", "continent_region", "country")
tables <- c("locality", "geopolitical_units", "geopolitical_units", "geopolitical_units")

road_list_values <- function (attributeName = NULL)
{
  # computing length of attributes
  size = length(attributes)
  # iterating over elements of attributes
  for (i in 1:size){
    if(attributeName == attributes[i]){
      table = tables[i]
    }
  }  
  query <- paste( "SELECT DISTINCT ", attributeName, " FROM ", table, " ORDER BY ", attributeName)
  
  data <- road_run_query(query)
  
  return(data)
}

