source("roadDB/R/road_package.R")

# road_get_localities <- function(continent = NULL, subcontinent = NULL, country = NULL, locality_type = NULL)

localities1 <- road_get_localities(continent = "Africa")
# localities11 <- road_get_localities(continent = "Africa", country = "South Africa")


# road_get_assemblages <- function(localities, category = NULL, age_min = NULL, age_max = NULL)

assemblages1 <- road_get_assemblages(localities1)
# assemblages11 <- road_get_assemblages(localities1, category = "human remains")

# road_get_human_remains <- function(assemblages, genus = NULL, species = NULL, genus_species = NULL)

humanremains1 <- road_get_human_remains(assemblages1)

values1 <- road_list_values("type")
values2 <- road_list_values("continent")
# values3 <- road_list_values("subcontinent")
values3 <- road_list_values("continent_region")
values4 <- road_list_values("category")
# values5 <- road_list_values("country") needs query extension
values6 <- road_list_values("cultural_period")
