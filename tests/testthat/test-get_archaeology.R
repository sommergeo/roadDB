test_that("road_get_lithic_typology()", {
  # Call function without any filters
  result_all_columns <- road_get_lithic_typology()

  # Check if the column types match the expected types
  expect_equal(class(result_all_columns$locality_id), "character")
  expect_equal(class(result_all_columns$assemblage_id), "integer")
  expect_equal(class(result_all_columns$continent), "character")
  expect_equal(class(result_all_columns$subcontinent), "character")
  expect_equal(class(result_all_columns$country), "character")
  expect_equal(class(result_all_columns$locality_types), "character")
  expect_equal(class(result_all_columns$coord_x), "numeric")
  expect_equal(class(result_all_columns$coord_y), "numeric")
  expect_equal(class(result_all_columns$assemblage_name), "character")
  expect_equal(class(result_all_columns$categories), "character")
  expect_equal(class(result_all_columns$age_min), "integer")
  expect_equal(class(result_all_columns$age_max), "integer")
  expect_equal(class(result_all_columns$cultural_periods), "character")
  expect_equal(class(result_all_columns$tool_list), "character")
  expect_equal(class(result_all_columns$typology), "character")
  expect_equal(class(result_all_columns$percentage), "integer")
  expect_equal(class(result_all_columns$comments), "character")

  # Check if the result is a data frame and has the expected number of rows and columns
  expect_s3_class(result_all_columns, "data.frame")
  expect_equal(ncol(result_all_columns), 17)

  # Check that the percentage column is within the expected range (0 to 100), or NA is allowed
  expect_true(
    all((result_all_columns$percentage >= 0 & result_all_columns$percentage <= 100) | is.na(result_all_columns$percentage)),
    info = "Some values in the percentage column are outside the expected range (0 to 100)"
  )

  # Filter for "flakes" in tool_list column
  result_flakes <- road_get_lithic_typology(tool_list = "flake")
  expect_true(
    all(grepl("flake", result_flakes$tool_list, ignore.case = TRUE)),
    info = "Some values in the tool_list column do not contain the string 'flake'"
  )
})


test_that("road_get_lithic_raw_material()", {
  # Call function without any filters
  result_all_columns <- road_get_lithic_raw_material()

  # Check if the column types match the expected types
  expect_equal(class(result_all_columns$locality_id), "character")
  expect_equal(class(result_all_columns$assemblage_id), "integer")
  expect_equal(class(result_all_columns$continent), "character")
  expect_equal(class(result_all_columns$subcontinent), "character")
  expect_equal(class(result_all_columns$country), "character")
  expect_equal(class(result_all_columns$locality_types), "character")
  expect_equal(class(result_all_columns$coord_x), "numeric")
  expect_equal(class(result_all_columns$coord_y), "numeric")
  expect_equal(class(result_all_columns$assemblage_name), "character")
  expect_equal(class(result_all_columns$categories), "character")
  expect_equal(class(result_all_columns$age_min), "integer")
  expect_equal(class(result_all_columns$age_max), "integer")
  expect_equal(class(result_all_columns$cultural_periods), "character")
  expect_equal(class(result_all_columns$raw_material_list), "character")
  expect_equal(class(result_all_columns$transport_distance), "character")
  expect_equal(class(result_all_columns$percentage), "integer")
  expect_equal(class(result_all_columns$comments), "character")

  # Check if the result is a data frame and has the expected number of rows and columns
  expect_s3_class(result_all_columns, "data.frame")
  expect_equal(ncol(result_all_columns), 17)

  # Check that the percentage column is within the expected range (0 to 100), or NA is allowed
  expect_true(
    all((result_all_columns$percentage >= 0 & result_all_columns$percentage <= 100) | is.na(result_all_columns$percentage)),
    info = "Some values in the percentage column are outside the expected range (0 to 100)"
  )

  # Filter for "flint" in tool_list column
  result_flint <- road_get_lithic_typology(tool_list = "flint")
  expect_true(
    all(grepl("flint", result_flint$tool_list, ignore.case = TRUE)),
    info = "Some values in the tool_list column do not contain the string 'flint'"
  )
})


test_that("road_get_organic_tools()", {
  # Call function without any filters
  result_all_columns <- road_get_organic_tools()

  # Check if the column types match the expected types
  expect_equal(class(result_all_columns$locality_id), "character")
  expect_equal(class(result_all_columns$assemblage_id), "integer")
  expect_equal(class(result_all_columns$continent), "character")
  expect_equal(class(result_all_columns$subcontinent), "character")
  expect_equal(class(result_all_columns$country), "character")
  expect_equal(class(result_all_columns$locality_types), "character")
  expect_equal(class(result_all_columns$coord_x), "numeric")
  expect_equal(class(result_all_columns$coord_y), "numeric")
  expect_equal(class(result_all_columns$assemblage_name), "character")
  expect_equal(class(result_all_columns$categories), "character")
  expect_equal(class(result_all_columns$age_min), "integer")
  expect_equal(class(result_all_columns$age_max), "integer")
  expect_equal(class(result_all_columns$cultural_periods), "character")
  expect_equal(class(result_all_columns$organic_tools_interpretation), "character")
  expect_equal(class(result_all_columns$organic_raw_material), "character")
  expect_equal(class(result_all_columns$number), "integer")
  expect_equal(class(result_all_columns$comments), "character")

  # Check if the result is a data frame and has the expected number of rows and columns
  expect_s3_class(result_all_columns, "data.frame")
  expect_equal(ncol(result_all_columns), 18)

  result_filter1 <- road_get_organic_tools(organic_tools_interpretation = "abrader/polisher")
  expect_true(
    all(grepl("abrader/polisher", result_filter1$organic_tools_interpretation, ignore.case = TRUE)),
    info = "Some values in the organic_tools_interpretation column do not contain the string 'abrader/polisher'"
  )
})


# road_get_lithic_typology()
#lithic_typology1 <- road_get_lithic_typology(continents = "Europe", tool_list = "biface")

#road_get_lithic_raw_material()
#lithic_raw_material1 <- road_get_lithic_raw_material(countries = "France", raw_material_list = c("volcanic rock", "obsidian"))

#road_get_organic_tools()
#organic_tools1 <- road_get_organic_tools(continents = "Europe", organic_tools_interpretation = "abrader/polisher")

#road_get_symbolic_artifacts()
#symbolic_artifacts1 <- road_get_symbolic_artifacts(continents = "Europe", symbolic_artifacts_interpretation = "abstract", cultural_periods = "Middle Paleolithic")

#road_get_feature()
#feature1 <- road_get_feature(continents = "Europe", feature_interpretation = "bedding", cultural_periods = "Middle Paleolithic")

#road_get_miscellaneous_finds()
#miscellaneous_finds1 <- road_get_miscellaneous_finds(continents = "Europe", miscellaneous_finds_material = "wood fossil", cultural_periods = "Middle Paleolithic")
