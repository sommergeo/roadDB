library(tidyverse)
library(sf)
library(RPostgreSQL)
library(RPostgres)
library(assertthat)



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
# road_locality

road_locality <- function(country="'%'", continent="'%'"){
  # Connect do ROAD
  con <- dbConnect(RPostgres::Postgres(), dbname = "roceeh", host="134.2.216.14", port=5432, user=rstudioapi::askForPassword("Database username"), password=rstudioapi::askForPassword("Database password"))
  
  
  if(is.string(country) && country==''){
    country <- "country ILIKE '%'"
  }else if(is.string(country) && length(country) ==1){
    country <- paste0("country IN ('",country,"')")
    print('clause1')
    
  }else if(is.vector(country) && length(country) >1){
    country <- paste0("country IN (", paste(sapply(country, function(x) paste0("'", x, "'")), collapse = ", "), ")")
    print('clause2')
  }
  print(country)
  
  if(is.string(continent) && continent==''){
    continent <- "continent ILIKE '%'"
  }else if(is.string(continent) && length(continent)==1){
    continent <- paste0("continent IN ('",continent,"')")
    print('clause1')
  }else if(is.vector(continent) && length(continent) >1){
    continent <- paste0("continent IN (", paste(sapply(continent, function(x) paste0("'", x, "'")), collapse = ", "), ")")
    print('clause2')
  }

  
  #query_locality <- "SELECT * FROM locality, geopolitical_units WHERE country=geopolitical_name AND rank=1 AND country ILIKE 'Germany' AND continent ILIKE 'Europe'"
  query_locality <- "SELECT * FROM locality, geopolitical_units WHERE country=geopolitical_name AND rank=1 AND %s AND %s"
  
  query <- sprintf(query_locality, country, continent)
  print(query)
  
  # Run the query
  dat <- dbGetQuery(con, query)
  
  return(dat)
  
}

continent <- 'Europe'
continent <- c('Europe')
continent <- c('Europe', 'France')

road_locality(c('France'), c('Europe'))
road_locality(continent = 'Europe')
road_locality('', 'Europe')

road_locality('France', 'Europe')

test <- road_locality(continent=c('Africa', 'Europe'))


country = "'%'"
continent = "'Europe'"

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
