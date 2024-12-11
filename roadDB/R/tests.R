# road_get_localities <- function(continent = NULL, subcontinent = NULL, country = NULL, locality_type = NULL)

localities1 <- road_get_localities(continent = "Africa")
localities12 <- road_get_localities(continent = "Africa", country = "South Africa")


# road_get_assemblages <- function(localities, category = NULL, age_min = NULL, age_max = NULL)

assemblages1 <- road_get_assemblages(localities1)
assemblages12 <- road_get_assemblages(localities1, category = "human remains")
