test_that("road_get_localities() returns the correct data types", {
  # Call the function
  result_all_columns <- road_get_localities()
  
  # Check if the column types match the expected types
  expect_equal(class(result_all_columns$locality_id), "character")
  expect_equal(class(result_all_columns$continent), "character")
  expect_equal(class(result_all_columns$subcontinent), "character")
  expect_equal(class(result_all_columns$country), "character")
  expect_equal(class(result_all_columns$locality_type), "character")
  expect_equal(class(result_all_columns$coord_x), "numeric")
  expect_equal(class(result_all_columns$coord_y), "numeric")
  expect_equal(class(result_all_columns$coordinate_source), "character")
  expect_equal(class(result_all_columns$cultural_period), "character")
  expect_equal(class(result_all_columns$technocomplex), "character")
  expect_equal(class(result_all_columns$subset_age_min), "integer")
  expect_equal(class(result_all_columns$subset_age_max), "integer")
  expect_equal(class(result_all_columns$locality_age_min), "integer")
  expect_equal(class(result_all_columns$locality_age_max), "integer")
  
  # Check if the result is a data frame and has the expected number of rows 
  # and columns
  expect_s3_class(result_all_columns, "data.frame")
  expect_equal(ncol(result_all_columns), 15)
  expect_true(nrow(road_get_localities()) > 0)
  
  #expect_message()
  #expect_warning()
  #expect_error()
})

# Testing whether the information about the function is helpful
help(road_get_localities)

# road_list_argument_values("continent") # Works
# road_list_argument_values("subcontinent") # when running this it returns nothing
# road_list_argument_values("country") # Works, but very last on list is "the"
# road_list_argument_values("locality_type") # when running this it returns nothing
# road_list_argument_values("cultural_period") # the very first row here is "Age", also, there is no "Middle Paleolithic" as used in one of the examples?

# # testing examples

# ESA_Africa <- road_get_localities(continent = "Africa", cultural_period = "ESA") # Works

# ESA <- road_get_localities(cultural_period = "ESA") # Works

# pal <- road_get_localities(cultural_period = "Paleolithic") # No Paleolithic in the world?

# DE_middle_pal <- road_get_localities(country = "Germany", cultural_period = c("ESA/MSA", "LP/MP", "MP/UP", "MSA", "MSA/LSA")) # Returns nothing?

# test_example <- road_get_localities(country = c("Germany", "France"), cultural_period = "Middle Paleolithic") # This example from the help function works, but no "Middle Paleolithic" in the list of cultural periods?

# ## Looking into cultural periods with play example ##
# # getting all sites in the from Europe, Africa and Asia

World <- road_get_localities(continent = c("Europe", "Asia", "Africa"))

table(World$cultural_period)



# seperating the many cultural periods in the cultural period column
split_periods <- strsplit(as.character(World$cultural_period), ",")


all_periods <- unlist(split_periods)

table(all_periods)


# Making a dataframe for plotting sites in a barplot
all_periods <- trimws(all_periods)  # Clean the values
df_counts <- as.data.frame(table(all_periods))
colnames(df_counts) <- c("Cultural_Period", "Count")

# Barplot
# ggplot(df_counts, aes(x = Cultural_Period, y = Count)) +
#   geom_bar(stat = "identity", fill = "skyblue") +
#   labs(title = "Site Frequency",
#        x = "Cultural Period",
#        y = "Site") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# There is a mismatch between the cultural periods provided by the road_list_values("cultural_period") function and the data itself (cf. plot above)
