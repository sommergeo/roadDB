# source("roadDB/R/road_package.R")

# road_get_localities <- function(continent = NULL, subcontinent = NULL, country = NULL, locality_type = NULL)

# localities1 <- road_get_localities(continents = "Africa")
# localities2 <- road_get_localities(continents = c('Africa', "Asia"))
# localities3 <- road_get_localities(countries = c("Germany", "France"), cultural_periods = "Middle Paleolithic")
# 
# humanremains0 <- road_get_human_remains(countries = "Ukraine")
# humanremains1 <- road_get_human_remains(age_min = 80000L, age_max = 120000L)
# humanremains2 <- road_get_human_remains(continents = "Africa", human_genus = "Homo", human_species = "rudolfensis")
# 
# values0 <- road_list_argument_values()
# values1 <- road_list_argument_values("type")
# values2 <- road_list_argument_values("continent")
# values3 <- road_list_argument_values("subcontinent")
# values3 <- road_list_argument_values("continent_region")
# values4 <- road_list_argument_values("category")
# values6 <- road_list_argument_values("cultural_period")
# values7 <- road_list_argument_values("country")
# values8 <- road_list_argument_values("example")
# values9 <- road_list_argument_values("dating_method")
# values10 <- road_list_argument_values("material_dated")
# values11 <- road_list_argument_values("technocomplex")
# values12 <- road_list_argument_values("humanremains:genus")
# values13 <- road_list_argument_values("tool_list")
# values14 <- road_list_argument_values("raw_material_list")
# values15 <- road_list_argument_values("material")
# values16 <- road_list_argument_values("feature:interpretation")
# values17 <- road_list_argument_values("organic_tools:interpretation")
# values18 <- road_list_argument_values("plant_taxonomy:family")
# 
# # road_get_localities()
# localities2 <- road_get_localities(continents = c("Europe"), countries = c("Germany", "France"))
# localities3 <- road_get_localities(continents = "Europe")
# localities4 <- road_get_localities(countries = c("Germany", "France"), locality_type = "cave")
# localities5 <- road_get_localities(countries = "Ukraine")
# 
# localities6 <- road_get_localities(countries = c("Germany", "France"), cultural_periods = "Middle Paleolithic")
# localities7 <- road_get_localities(countries = "Ukraine", cultural_periods = "Middle Paleolithic")
# 
# # road_get_assemblages()
# localities0 <- c("Bacho Kiro")
# assemblages2 <- road_get_assemblages(countries = c("Germany", "France"), cultural_periods = "Middle Paleolithic")
# assemblages3 <- road_get_assemblages(age_min = 80000L, age_max = 120000L)
# assemblages4 <- road_get_assemblages(categories = c("typology", "paleofauna"), age_max = 100000L)
# assemblages6 <- road_get_assemblages(countries = "Ukraine")
# assemblages7 <- road_get_assemblages(countries = "Ukraine", cultural_periods = "Middle Paleolithic")
# assemblages8 <- road_get_assemblages(continents = "Europe", cultural_periods = "Middle Paleolithic")
# 
# 
# # aux functions
# query_check_intersection("and ", "technology, typology", "technology, paleofauna")
# 
# 
# dates0 <- road_get_dates(dating_methods = c("geology", "biostratigraphy", "U series (uranium-lead) dating"), material_dated = "calcite")
# dates1 <- road_get_dates()
# dates2 <- road_get_dates(age_min = 10000L, age_max = 100000L)
# dates3 <- road_get_dates(material_dated = c("coprolite", "glass", "ivory"))
# dates4 <- road_get_dates(material_dated = c("glass"))
# dates5 <- road_get_dates(technocomplex = c("UP/ Aurignacian"))
# dates6 <- road_get_dates(continents = "Europe", countries = c("Germany", "France"), cultural_periods = "Middle Paleolithic", dating_methods = c("geology", "biostratigraphy"), technocomplex = c("UP/ Aurignacian"))
# dates7 <- road_get_dates(continents = "Europe", cultural_periods = "Middle Paleolithic", dating_methods = c("geology", "biostratigraphy"))
# dates8 <- road_get_dates(continents = "Europe", countries = c("Germany", "France"), cultural_periods = "Middle Paleolithic", dating_methods = c("geology", "biostratigraphy"), technocomplex = c("UP/ Aurignacian"))
# dates9 <- road_get_dates(countries = "France", categories = c("feature"))
# dates10 <- road_get_dates(technocomplex = "EP/ Epipaleolithic")
# #dates11 <- road_get_dates(localities=localities5) #, technocomplex = "UP/ Upper Paleolithic - Eurasia")
# dates12 <- road_get_dates(assemblages=assemblages2, technocomplex = "UP/ Upper Paleolithic - Eurasia")
# 
# 
# #road_summarize_archaeology
# summary1 <- road_summarize_archaeology(term = "Cores")
# summary2 <- road_summarize_archaeology(term = "ochre")
# 
# #
# paleofauna0 <- road_get_paleofauna()
# paleofauna1 <- road_get_paleofauna(continents = "Asia")
# paleofauna2 <- road_get_paleofauna(countries = c("Germany", "France"))
# paleofauna3 <- road_get_paleofauna(countries = c("Germany", "France"), fauna_genus = "Canis")
# paleofauna4 <- road_get_paleofauna(countries = c("Germany", "France"), fauna_genus = "Canis", fauna_species = c("lupus", "vulpes"))
# paleofauna5 <- road_get_paleofauna(countries = c("Germany", "France"), fauna_genus = c("Canis", "Lepus"), fauna_species = c("lupus", "vulpes"))

