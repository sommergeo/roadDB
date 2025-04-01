# source("roadDB/R/road_package.R")

# road_get_localities <- function(continent = NULL, subcontinent = NULL, country = NULL, locality_type = NULL)

localities1 <- road_get_localities(continents = "Africa")
localities2 <- road_get_localities(continents = c('Africa', "Asia"))
localities3 <- road_get_localities(countries = c("Germany", "France"), cultural_periods = "Middle Paleolithic")

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
values12 <- road_list_values("humanremains:genus")
values13 <- road_list_values("tool_list")
values14 <- road_list_values("raw_material_list")
values15 <- road_list_values("miscellaneous_finds:material")
values16 <- road_list_values("feature:interpretation")
values17 <- road_list_values("organic_tools:interpretation")

# road_get_localities()
localities2 <- road_get_localities(continents = c("Europe"), countries = c("Germany", "France"))
localities3 <- road_get_localities(continents = "Europe")
localities4 <- road_get_localities(countries = c("Germany", "France"), locality_type = "cave")
localities5 <- road_get_localities(countries = "Ukraine")

localities6 <- road_get_localities(countries = c("Germany", "France"), cultural_periods = "Middle Paleolithic")
localities7 <- road_get_localities(countries = "Ukraine", cultural_periods = "Middle Paleolithic")

# road_get_assemblages()
localities0 <- c("Bacho Kiro")
assemblages2 <- road_get_assemblages(countries = c("Germany", "France"), cultural_periods = "Middle Paleolithic")
#assemblages3 <- road_get_assemblages(localities = localities2, age_min = 80000L, age_max = 120000L)
#assemblages3 <- road_get_assemblages(localities = localities2, age_min = 80000L, age_max = 120000L)
#assemblages4 <- road_get_assemblages(localities = localities2, categories = c("typology", "paleofauna"), age_max = 100000L)
#assemblages5 <- road_get_assemblages(localities = localities3)
assemblages6 <- road_get_assemblages(countries = "Ukraine")
assemblages7 <- road_get_assemblages(countries = "Ukraine", cultural_periods = "Middle Paleolithic")
assemblages8 <- road_get_assemblages(continents = "Europe", cultural_periods = "Middle Paleolithic")


# aux functions
query_check_intersection("and ", "technology, typology", "technology, paleofauna")


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
#dates11 <- road_get_dates(localities=localities5) #, technocomplex = "UP/ Upper Paleolithic - Eurasia")
dates12 <- road_get_dates(assemblages=assemblages2, technocomplex = "UP/ Upper Paleolithic - Eurasia")

# road_get_lithic_typology()
lithic_typology1 <- road_get_lithic_typology(continents = "Europe", tool_list = "biface")

#road_get_lithic_raw_material()
lithic_raw_material1 <- road_get_lithic_raw_material(countries = "France", raw_material_list = c("volcanic rock", "obsidian"))

#road_get_organic_tools()
organic_tools1 <- road_get_organic_tools(continents = "Europe", organic_tools_interpretation = "abrader/polisher")

#road_get_symbolic_artifacts()
symbolic_artifacts1 <- road_get_symbolic_artifacts(continents = "Europe", symbolic_artifacts_interpretation = "abstract", cultural_periods = "Middle Paleolithic")

#road_get_feature()
feature1 <- road_get_feature(continents = "Europe", feature_interpretation = "bedding", cultural_periods = "Middle Paleolithic")

#road_get_miscellaneous_finds()
miscellaneous_finds1 <- road_get_miscellaneous_finds(continents = "Europe", miscellaneous_finds_material = "wood fossil", cultural_periods = "Middle Paleolithic")

#road_summerize_archaeology
summary1 <- road_summerize_archaeology(term = "Cores")
summary2 <- road_summerize_archaeology(term = "ochre")

# archaeo5 <- road_get_archaeology(raw_material_list = "syenite")
# archaeo6 <- road_get_archaeology(cultural_periods = "Middle Paleolithic", raw_material_list = c("flysch", "syenite"))
# archaeo7 <- road_get_archaeology(countries = "France", miscellaneous_finds_material = "glass")
# archaeo8 <- road_get_archaeology(feature_interpretation = "footprints")
# archaeo9 <- road_get_archaeology(organic_tools_interpretation = "retoucher", countries = "France")

#
paleofauna0 <- road_get_paleofauna()
paleofauna1 <- road_get_paleofauna(continents = "Asia")
paleofauna2 <- road_get_paleofauna(countries = c("Germany", "France"))
paleofauna3 <- road_get_paleofauna(countries = c("Germany", "France"), fauna_genus = "Canis")
paleofauna4 <- road_get_paleofauna(countries = c("Germany", "France"), fauna_genus = "Canis", fauna_species = c("lupus", "vulpes"))
paleofauna5 <- road_get_paleofauna(countries = c("Germany", "France"), fauna_genus = c("Canis", "Lepus"), fauna_species = c("lupus", "vulpes"))

