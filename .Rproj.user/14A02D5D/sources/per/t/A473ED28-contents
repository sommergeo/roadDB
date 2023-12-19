library(tidyverse)
library(sf)
library(RPostgreSQL)
library(RPostgres)



road_query <- function(culture, spatial=T){
  
  # Connect do ROAD
  con <- dbConnect(RPostgres::Postgres(), dbname = "roceeh", host="134.2.216.14", port=5432, user=rstudioapi::askForPassword("Database username"), password=rstudioapi::askForPassword("Database password"))
  
  # Prepare query
  if(exists('culture')){
    # Load sql query template
    query_culture <- readr::read_file('query_culture.sql')
    
    # Integrate query 
    query <- sprintf(query_culture, culture)
  }
  
  # Run the query
  dat <- dbGetQuery(con, query)
  
  # Make it *spatial*
  if(spatial==T){
    dat <- st_as_sf(dat, coords = c('x','y')) %>% st_set_crs(4326)
  }
  
}


test <- road_query(culture='Oldowan - Africa')

head(test)

#### Test ----

con <- dbConnect(RPostgres::Postgres(), dbname = "roceeh", host="134.2.216.14", port=5432, user=rstudioapi::askForPassword("Database username"), password=rstudioapi::askForPassword("Database password"))
query_culture <- "SELECT DISTINCT on (archaeological_layer.locality_idlocality, archaeological_layer.archstratigraphy_idarchstrat, locality.x, locality.y) archaeological_layer.locality_idlocality, archaeological_layer.archstratigraphy_idarchstrat, locality.x, locality.y FROM archaeological_layer, locality WHERE (locality.idlocality = archaeological_layer.locality_idlocality and archaeological_layer.archstratigraphy_idarchstrat like '%s')"
query_culture <- readr::read_file('query_culture.sql')
culture <- 'Oldowan - Africa'
exists(culture)
query <- sprintf(query_culture, culture)
dat <- dbGetQuery(con, query)
dat <- st_as_sf(dat, coords = c('x','y'))
dat <- st_as_sf(dat, coords = c('x','y')) %>% st_set_crs(4326)

st_crs(dat)
