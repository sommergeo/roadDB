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
