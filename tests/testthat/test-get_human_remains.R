test_that("road_get_human_remains() returns the correct data types", {
  # Call the function
  result_all_columns <- road_get_human_remains()
  
  # Check if the column types match the expected types
  expect_equal(class(result_all_columns$locality_id), "character")
  expect_equal(class(result_all_columns$assemblage_id), "integer")
  expect_equal(class(result_all_columns$humanremains_id), "integer")
  expect_equal(class(result_all_columns$continent), "character")
  expect_equal(class(result_all_columns$subcontinent), "character")
  expect_equal(class(result_all_columns$country), "character")
  expect_equal(class(result_all_columns$locality_types), "character")
  expect_equal(class(result_all_columns$coord_x), "numeric")
  expect_equal(class(result_all_columns$coord_y), "numeric")
  expect_equal(class(result_all_columns$cultural_periods), "character")
  expect_equal(class(result_all_columns$technocomplexes), "character")
  expect_equal(class(result_all_columns$assemblage_name), "character")
  expect_equal(class(result_all_columns$categories), "character")
  expect_equal(class(result_all_columns$age_min), "integer")
  expect_equal(class(result_all_columns$age_max), "integer")
  expect_equal(class(result_all_columns$genus), "character")
  expect_equal(class(result_all_columns$species), "character")
  expect_equal(class(result_all_columns$age), "character")
  expect_equal(class(result_all_columns$sex), "character")
  
  # Check if the result is a data frame and has the expected number of rows 
  # and columns
  expect_s3_class(result_all_columns, "data.frame")
  expect_equal(ncol(result_all_columns), 21)
  expect_true(nrow(road_get_human_remains()) > 0)
})

# test_that("road_get_human_remains() returns sane age results", {
#   # Call the function
#   result <- road_get_human_remains()
#   
#   # Check that the age columns are within the expected range, or NA is allowed
#   expect_true(all(is.na(result$age_min) | (result$age_min >= 0 & result$age_min <= 7000000)), 
#               info = "Some values in the age_min column are outside the expected range (0 to 7 million), or there are unexpected NAs.")
# 
#   expect_true(all(is.na(result$age_max) | (result$age_max >= 0 & result$age_max <= 7000000)), 
#               info = "Some values in the age_max column are outside the expected range (0 to 7 million), or there are unexpected NAs.")
#   
# })

test_that("road_get_human_remains() returns sane results for attribute sex", {
  # Call the function
  result <- road_get_human_remains()

  # Check that sex values are: F, M or NA
  expect_true(all(is.na(result$sex) | result$sex == "M" | result$sex == "F" | result$sex == "M?" | result$sex == "F?"), 
              info = "Some values are not expected values or there are unexpected NAs.")
})