# source("roadDB/R/road_package.R")

# road_get_localities <- function(continent = NULL, subcontinent = NULL, country = NULL, locality_type = NULL)

localities1 <- road_get_localities(continents = "Africa")
localities1 <- road_get_localities(continents = c('Africa', "Asia"))
# localities11 <- road_get_localities(continents = "Africa", country = "South Africa")

humanremains0 <- road_get_human_remains(countries = "Ukraine")
humanremains1 <- road_get_human_remains(age_min = 80000L, age_max = 120000L)
humanremains2 <- road_get_human_remains(continents = "Africa", genus = "Homo", species = "rudolfensis")

values0 <- road_list_values()
values1 <- road_list_values("type")
values2 <- road_list_values("continent")
# values3 <- road_list_values("subcontinent")
values3 <- road_list_values("continent_region")
values4 <- road_list_values("category")
values6 <- road_list_values("cultural_period")
values7 <- road_list_values("country")
# values8 <- road_list_values("example")
values9 <- road_list_values("dating_method")
values10 <- road_list_values("material_dated")
values11 <- road_list_values("technocomplex")
values12 <- road_list_values("publication_desc_humanremains.species")

# road_get_localities()
localities2 <- road_get_localities(continents = c("Europe"), countries = c("Germany", "France"))
localities3 <- road_get_localities(continents = "Europe")
localities4 <- road_get_localities(countries = c("Germany", "France"), locality_type = "cave")
localities5 <- road_get_localities(countries = "Ukraine")

localities6 <- road_get_localities(countries = c("Germany", "France"), cultural_periods = "Middle Paleolithic")

# road_get_assemblages()
localities0 <- c("Bacho Kiro")
assemblages2 <- road_get_assemblages(localities = localities5)
assemblages3 <- road_get_assemblages(localities = localities2, age_min = 80000L, age_max = 120000L)
assemblages3 <- road_get_assemblages(localities = localities2, age_min = 80000L, age_max = 120000L)
assemblages4 <- road_get_assemblages(localities = localities2, categories = c("typology", "paleofauna"), age_max = 100000L)
assemblages5 <- road_get_assemblages(localities = localities3)

# aux functions
query_check_intersection("and ", "technology, typology", "technology, paleofauna")

#
dates0 <- road_get_dates(dating_methods = c("geology", "biostratigraphy", "U series (uranium-lead) dating"), material_dated = "calcite")
dates1 <- road_get_dates()
dates2 <- road_get_dates(age_min = 10000L, age_max = 100000L)
dates3 <- road_get_dates(material_dated = c("coprolite", "glass", "ivory"))
dates4 <- road_get_dates(material_dated = c("glass"))
dates5 <- road_get_dates(technocomplex = c("UP/ Aurignacian"))
dates6 <- road_get_dates(continents = "Europe", countries = c("Germany", "France"), cultural_periods = "Middle Paleolithic", dating_methods = c("geology", "biostratigraphy"), technocomplex = c("UP/ Aurignacian"))
dates7 <- road_get_dates(continents = "Europe", cultural_periods = "Middle Paleolithic", dating_methods = c("geology", "biostratigraphy"))
dates8 <- road_get_dates(continents = "Europe", countries = c("Germany", "France"), cultural_periods = "Middle Paleolithic", dating_methods = c("geology", "biostratigraphy"), technocomplex = c("UP/ Aurignacian"))
dates9 <- road_get_dates(countries = "France", categories = c("feature"))
dates10 <- road_get_dates(technocomplex = "EP/ Epipaleolithic")
dates11 <- road_get_dates(localities=localities5) #, technocomplex = "UP/ Upper Paleolithic - Eurasia")
dates12 <- road_get_dates(assemblages=assemblages2, technocomplex = "UP/ Upper Paleolithic - Eurasia")
