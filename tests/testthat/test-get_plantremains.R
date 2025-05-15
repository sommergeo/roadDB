test_that("road_get_plantremains() returns the correct data types", {
  # Call the function
  result <- road_get_plantremains()
  
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
  expect_equal(class(result$plant_remains), "character")
  expect_equal(class(result$plant_family), "character")
  expect_equal(class(result$plant_genus), "character")
  expect_equal(class(result$plant_species), "character")
  
  # Check if the result is a data frame and has the expected number of rows 
  # and columns
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 17)
  expect_true(nrow(road_get_plantremains()) > 0)
})

# test_that("road_get_plantremains() returns sane age results", {
#   # Call the function
#   result <- road_get_plantremains()
#   
#   # Check that the age column is within the expected range, or NA is allowed
#   expect_true(all(is.na(result$age_min) | (result$age_min >= 0 & result$age_min <= 7000000)), 
#               info = "Some values in the age_min column are outside the expected range (0 to 7 million), or there are unexpected NAs.")
#   
#   # Check that the age column is within the expected range, or NA is allowed
#   expect_true(all(is.na(result$age_max) | (result$age_max >= 0 & result$age_max <= 7000000)), 
#               info = "Some values in the age_max column are outside the expected range (0 to 7 million), or there are unexpected NAs.")
# })
  