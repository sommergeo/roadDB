# source("roadDB/R/road_package.R")

# road_get_localities <- function(continent = NULL, subcontinent = NULL, country = NULL, locality_type = NULL)

localities1 <- road_get_localities(continent = "Africa")
localities1 <- road_get_localities(continent = c('Africa', "Asia"))
# localities11 <- road_get_localities(continent = "Africa", country = "South Africa")


# road_get_assemblages <- function(localities, category = NULL, age_min = NULL, age_max = NULL)

assemblages1 <- road_get_assemblages(localities1)
# assemblages11 <- road_get_assemblages(localities1, category = "human remains")

# road_get_human_remains <- function(assemblages, genus = NULL, species = NULL, genus_species = NULL)

# humanremains1 <- road_get_human_remains(assemblages1)

values0 <- road_list_values()
values1 <- road_list_values("type")
values2 <- road_list_values("continent")
# values3 <- road_list_values("subcontinent")
values3 <- road_list_values("continent_region")
values4 <- road_list_values("category")
values6 <- road_list_values("cultural_period")
values7 <- road_list_values("country")
values8 <- road_list_values("example")
values9 <- road_list_values("dating_method")

# road_get_localities()
localities2 <- road_get_localities(continents = c("Europe"), countries = c("Germany", "France"))
localities3 <- road_get_localities(continents = "Europe", countries = c("Germany", "France"))
localities4 <- road_get_localities(countries = c("Germany", "France"), locality_type = "cave")
# localities5 <- road_get_localities(NULL, NULL, "Bulgaria")
# localities5 <- road_get_localities(NULL, NULL, "Angola")
localities5 <- road_get_localities(NULL, NULL, "Ukraine") # Locality "Doroshivtsi III", geolayer "Unit VII-2"
localities6 <- road_get_localities(countries = c("Germany", "France"), cultural_periods = "Middle Paleolithic")

# road_get_assemblages()
localities0 <- c("Bacho Kiro")
assemblages2 <- road_get_assemblages(localities5, NULL, NULL, NULL)
assemblages3 <- road_get_assemblages(localities2, NULL, 80000L, 120000L)
assemblages4 <- road_get_assemblages(localities = localities2, categories = "typology", age_max = 100000L)

# aux functions
query_check_intersection("and ", "technology, typology", "technology, paleofauna")

#
dates0 <- road_get_dates("geology, biostratigraphy")
