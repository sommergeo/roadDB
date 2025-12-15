test_that("road_get_publications() returns the correct data types without localities", {
  # Call the function without localities parameter
  result_no_localities <- road_get_publications()

  # Check if the result is a data frame
  expect_s3_class(result_no_localities, "data.frame")

  # Check that it has exactly 1 column when no localities are specified
  expect_equal(ncol(result_no_localities), 1)

  # Check if the column type is character
  expect_equal(class(result_no_localities$Publication), "character")

  # Check if the result has at least one row
  expect_true(nrow(result_no_localities) > 0)
})

test_that("road_get_publications() returns the correct data types with localities", {
  # Call the function with localities parameter
  result_with_localities <- road_get_publications(localities = c("Apollo 11", "Berg Aukas"))

  # Check if the result is a data frame
  expect_s3_class(result_with_localities, "data.frame")

  # Check that it has exactly 2 columns when localities are specified
  expect_equal(ncol(result_with_localities), 2)

  # Check that the columns are named correctly
  expect_true("Locality" %in% colnames(result_with_localities))
  expect_true("Publication" %in% colnames(result_with_localities))

  # Check if the column types are character
  expect_equal(class(result_with_localities$Locality), "character")
  expect_equal(class(result_with_localities$Publication), "character")

  # Check if the result has at least one row
  expect_true(nrow(result_with_localities) > 0)
})

test_that("road_get_publications() works with single locality string", {
  # Call the function with a single locality as string
  result_single <- road_get_publications(localities = "Apollo 11")

  # Check if the result is a data frame
  expect_s3_class(result_single, "data.frame")

  # Check that it has 2 columns
  expect_equal(ncol(result_single), 2)

  # Check column names
  expect_true("Locality" %in% colnames(result_single))
  expect_true("Publication" %in% colnames(result_single))
})

test_that("road_get_publications() works with localities data frame", {
  # Get localities first
  locs <- road_get_localities(country = "Namibia")

  # Call the function with localities data frame
  result_df <- road_get_publications(localities = locs)

  # Check if the result is a data frame
  expect_s3_class(result_df, "data.frame")

  # Check that it has 2 columns
  expect_equal(ncol(result_df), 2)

  # Check column names
  expect_true("Locality" %in% colnames(result_df))
  expect_true("Publication" %in% colnames(result_df))
})

test_that("road_get_publications() returns bibtex format when requested", {
  # Call the function with bibtex = TRUE
  result_bibtex <- road_get_publications(localities = "Apollo 11", bibtex = TRUE)

  # Check if the result is a data frame
  expect_s3_class(result_bibtex, "data.frame")

  # Check that it has 2 columns
  expect_equal(ncol(result_bibtex), 2)

  # Check if publications contain bibtex formatting (should start with @)
  expect_true(any(grepl("^@", result_bibtex$Publication)))
})
