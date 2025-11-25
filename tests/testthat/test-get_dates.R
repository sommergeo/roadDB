test_that("road_get_dates() returns the correct data types", {
  # Call the function
  result <- road_get_dates()
  
  # Check if the column types match the expected types
  expect_equal(class(result$locality_id), "character")
  expect_equal(class(result$assemblage_id), "integer")
  expect_equal(class(result$continent), "character")
  expect_equal(class(result$subcontinent), "character")
  expect_equal(class(result$country), "character")
  expect_equal(class(result$locality_type), "character")
  expect_equal(class(result$coord_x), "numeric")
  expect_equal(class(result$coord_y), "numeric")
  expect_equal(class(result$assemblage_name), "character")
  expect_equal(class(result$category), "character")
  expect_equal(class(result$age_min), "integer")
  expect_equal(class(result$age_max), "integer")
  expect_equal(class(result$cultural_periods), "character")
  expect_equal(class(result$geolayer), "character")
  expect_equal(class(result$archlayer), "character")
  expect_equal(class(result$age), "integer")
  expect_equal(class(result$negative_standard_deviation), "integer")
  expect_equal(class(result$positive_standard_deviation), "integer")
  expect_equal(class(result$material_dated), "character")
  expect_equal(class(result$dating_method), "character")
  expect_equal(class(result$laboratory_idlaboratory), "character")
  expect_equal(class(result$technocomplex), "character")
  
  # Check if the result is a data frame and has the expected number of rows and columns
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 25)
  expect_true(nrow(road_get_dates()) > 0)
})


test_that("road_get_dates() returns sane age results", {
  # Call the function
  result <- road_get_dates()
  
  # Check that the age column is within the expected range, or NA is allowed
  expect_true(all(is.na(result$age) | (result$age >= 0 & result$age <= 7000000)), 
              info = "Some values in the age column are outside the expected range (0 to 7 million), or there are unexpected NAs.")
  
  # Check that all values in the 'negative_standard_deviation' column are positive, or NA is allowed
  expect_true(all(is.na(result$negative_standard_deviation) | (result$negative_standard_deviation >= 0)), 
              info = "Some values in the 'negative_standard_deviation' column are non-positive, or there are unexpected NAs.")
  
  # Check that all values in the 'positive_standard_deviation' column are positive, or NA is allowed
  expect_true(all(is.na(result$positive_standard_deviation) | (result$positive_standard_deviation >= 0)), 
              info = "Some values in the 'positive_standard_deviation' column are non-positive, or there are unexpected NAs.")
})
