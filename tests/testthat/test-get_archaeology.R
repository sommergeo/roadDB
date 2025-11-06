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
  expect_equal(class(result_all_columns$technocomplexes), "character")
  expect_equal(class(result_all_columns$tool_list), "character")
  expect_equal(class(result_all_columns$typology), "character")
  expect_equal(class(result_all_columns$percentage), "integer")
  expect_equal(class(result_all_columns$comments), "character")

  # Check if the result is a data frame and has the expected number of rows and columns
  expect_s3_class(result_all_columns, "data.frame")
  expect_equal(ncol(result_all_columns), 20)

  # Check that the percentage column is within the expected range (0 to 100), or NA is allowed
  expect_true(
    all((result_all_columns$percentage >= 0 & result_all_columns$percentage <= 100) | is.na(result_all_columns$percentage)),
    info = "Some values in the percentage column are outside the expected range (0 to 100)"
  )

  # Filter for "flakes" in tool_list column
  result_filter1 <- road_get_lithic_typology(tool_list = "flake")
  expect_true(
    all(grepl("flake", result_filter1$tool_list, ignore.case = TRUE)),
    info = "Some values in the tool_list column do not contain the string 'flake'"
  )
  # Check if there is at least one row in the result
  expect_true(
    nrow(result_filter1) > 0,
    info = "The result does not contain any rows"
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
  expect_equal(class(result_all_columns$technocomplexes), "character")
  expect_equal(class(result_all_columns$raw_material_list), "character")
  expect_equal(class(result_all_columns$transport_distance), "character")
  expect_equal(class(result_all_columns$percentage), "integer")
  expect_equal(class(result_all_columns$comments), "character")

  # Check if the result is a data frame and has the expected number of rows and columns
  expect_s3_class(result_all_columns, "data.frame")
  expect_equal(ncol(result_all_columns), 20)

  # Check that the percentage column is within the expected range (0 to 100), or NA is allowed
  expect_true(
    all((result_all_columns$percentage >= 0 & result_all_columns$percentage <= 100) | is.na(result_all_columns$percentage)),
    info = "Some values in the percentage column are outside the expected range (0 to 100)"
  )

  # Filter for "flint" in raw_material_list column
  result_filter1 <- road_get_lithic_raw_material(raw_material_list = "flint")
  expect_true(
    all(grepl("flint", result_filter1$raw_material_list, ignore.case = TRUE)),
    info = "Some values in the raw_material_list column do not contain the string 'flint'"
  )
  # Check if there is at least one row in the result
  expect_true(
    nrow(result_filter1) > 0,
    info = "The result does not contain any rows"
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
  expect_equal(class(result_all_columns$technocomplexes), "character")
  expect_equal(class(result_all_columns$organic_tools_interpretation), "character")
  expect_equal(class(result_all_columns$organic_raw_material), "character")
  expect_equal(class(result_all_columns$number), "integer")
  expect_equal(class(result_all_columns$comments), "character")

  # Check if the result is a data frame and has the expected number of rows and columns
  expect_s3_class(result_all_columns, "data.frame")
  expect_equal(ncol(result_all_columns), 21)

  result_filter1 <- road_get_organic_tools(organic_tools_interpretation = "abrader/polisher")
  expect_true(
    all(grepl("abrader/polisher", result_filter1$organic_tools_interpretation, ignore.case = TRUE)),
    info = "Some values in the organic_tools_interpretation column do not contain the string 'abrader/polisher'"
  )
  # Check if there is at least one row in the result
  expect_true(
    nrow(result_filter1) > 0,
    info = "The result does not contain any rows"
  )
})


test_that("road_get_symbolic_artifacts()", {
  # Call function without any filters
  result_all_columns <- road_get_symbolic_artifacts()

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
  expect_equal(class(result_all_columns$technocomplexes), "character")
  expect_equal(class(result_all_columns$symbolic_artifacts_interpretation), "character")
  expect_equal(class(result_all_columns$symbolic_artifacts_category), "character")
  expect_equal(class(result_all_columns$symbolic_artifacts_material), "character")
  expect_equal(class(result_all_columns$symbolic_artifacts_raw_material_source), "character")
  expect_equal(class(result_all_columns$comments), "character")

  # Check if the result is a data frame and has the expected number of rows and columns
  expect_s3_class(result_all_columns, "data.frame")
  expect_equal(ncol(result_all_columns), 22)

  result_filter1 <- road_get_symbolic_artifacts(symbolic_artifacts_interpretation = "abstract")
  expect_true(
    all(grepl("abstract", result_filter1$symbolic_artifacts_interpretation, ignore.case = TRUE)),
    info = "Some values in the symbolic_artifacts_interpretation column do not contain the string 'abstract'"
  )
  # Check if there is at least one row in the result
  expect_true(
    nrow(result_filter1) > 0,
    info = "The result does not contain any rows"
  )
})


test_that("road_get_feature()", {
  # Call function without any filters
  result_all_columns <- road_get_feature()

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
  expect_equal(class(result_all_columns$technocomplexes), "character")
  expect_equal(class(result_all_columns$feature_interpretation), "character")
  expect_equal(class(result_all_columns$comments), "character")

  # Check if the result is a data frame and has the expected number of rows and columns
  expect_s3_class(result_all_columns, "data.frame")
  expect_equal(ncol(result_all_columns), 18)

  result_filter1 <- road_get_feature(feature_interpretation = "bedding")
  expect_true(
    all(grepl("bedding", result_filter1$feature_interpretation, ignore.case = TRUE)),
    info = "Some values in the feature_interpretation column do not contain the string 'bedding'"
  )
  # Check if there is at least one row in the result
  expect_true(
    nrow(result_filter1) > 0,
    info = "The result does not contain any rows"
  )
})


test_that("road_get_miscellaneous_finds()", {
  # Call function without any filters
  result_all_columns <- road_get_miscellaneous_finds()

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
  expect_equal(class(result_all_columns$technocomplexes), "character")
  expect_equal(class(result_all_columns$miscellaneous_finds_material), "character")
  expect_equal(class(result_all_columns$miscellaneous_finds_raw_material_source), "character")
  expect_equal(class(result_all_columns$number), "integer")
  expect_equal(class(result_all_columns$comments), "character")

  # Check if the result is a data frame and has the expected number of rows and columns
  expect_s3_class(result_all_columns, "data.frame")
  expect_equal(ncol(result_all_columns), 20)

  result_filter1 <- road_get_miscellaneous_finds(miscellaneous_finds_material = "wood fossil")
  expect_true(
    all(grepl("wood fossil", result_filter1$miscellaneous_finds_material, ignore.case = TRUE)),
    info = "Some values in the miscellaneous_finds_material column do not contain the string 'wood fossil'"
  )
  # Check if there is at least one row in the result
  expect_true(
    nrow(result_filter1) > 0,
    info = "The result does not contain any rows"
  )
})
