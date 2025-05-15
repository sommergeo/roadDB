test_that("road_get_assemblages() returns the correct data types", {
  # Call the function
  result <- road_get_assemblages()
  
  # Check if the column types match the expected types
  expect_equal(class(result$locality_id), "character")
  expect_equal(class(result$assemblage_id), "integer")
  expect_equal(class(result$continent), "character")
  expect_equal(class(result$subcontinent), "character")
  expect_equal(class(result$country), "character")
  expect_equal(class(result$locality_types), "character")
  expect_equal(class(result$coord_x), "numeric")
  expect_equal(class(result$coord_y), "numeric")
  expect_equal(class(result$cultural_periods), "character")
  expect_equal(class(result$assemblage_name), "character")
  expect_equal(class(result$categories), "character")
  expect_equal(class(result$age_min), "integer")
  expect_equal(class(result$age_max), "integer")
  expect_equal(class(result$geolayers), "character")
  expect_equal(class(result$archlayers), "character")
  expect_equal(class(result$humanremains), "logical")
  expect_equal(class(result$paleofauna), "logical")
  expect_equal(class(result$archaeology), "logical")
  expect_equal(class(result$plantremains), "logical")
  
  # Check if the result is a data frame and has the expected number of rows 
  # and columns
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 19)
  expect_true(nrow(road_get_assemblages()) > 0)
})

test_that("road_get_assemblages returns sane age results", {
  # Call the function
  result <- road_get_assemblages()
  
  # Check that the age columns are within the expected range, or NA is allowed
  expect_true(all(is.na(result$age_min) | (result$age_min >= 0 & result$age_min <= 7000000)), 
              info = "Some values in the age_min column are outside the expected range (0 to 7 million), or there are unexpected NAs.")
  
  expect_true(all(is.na(result$age_max) | (result$age_max >= 0 & result$age_max <= 7000000)), 
              info = "Some values in the age_max column are outside the expected range (0 to 7 million), or there are unexpected NAs.")
  
})
