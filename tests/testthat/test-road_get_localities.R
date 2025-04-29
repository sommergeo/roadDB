test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


# Testing whether the information about the function is helpful
help(road_get_localities)

road_list_values_("continent") # Works
road_list_values_("subcontinent") # when running this it returns nothing
road_list_values_("country") # Works, but very last on list is "the"
road_list_values_("locality_type") # when running this it returns nothing
road_list_values_("cultural_period") # the very first row here is "Age", also, there is no "Middle Paleolithic" as used in one of the examples?


# testing examples

ESA_Africa <- road_get_localities(continents = "Africa", cultural_periods = "ESA") # Works

ESA <- road_get_localities(cultural_periods = "ESA") # Works

pal <- road_get_localities(cultural_periods = "Paleolithic") # No Paleolithic in the world?

DE_middle_pal <- road_get_localities(countries = "Germany", cultural_periods = c("ESA/MSA", "LP/MP", "MP/UP", "MSA", "MSA/LSA")) # Returns nothing?

test_example <- road_get_localities(countries = c("Germany", "France"), cultural_periods = "Middle Paleolithic") # This example from the help function works, but no "Middle Paleolithic" in the list of cultural periods?



## Looking into cultural periods with play example ##
# getting all sites in the from Europe, Africa and Asia

World <- road_get_localities(continents = c("Europe", "Asia", "Africa"))

table(World$cultural_periods)



# seperating the many cultural periods in the cultural period column
split_periods <- strsplit(as.character(World$cultural_periods), ",")


all_periods <- unlist(split_periods)

table(all_periods)


# Making a dataframe for plotting sites in a barplot
all_periods <- trimws(all_periods)  # Clean the values
df_counts <- as.data.frame(table(all_periods))
colnames(df_counts) <- c("Cultural_Period", "Count")

# Barplot
ggplot(df_counts, aes(x = Cultural_Period, y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Site Frequency",
       x = "Cultural Period",
       y = "Site") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# There is a mismatch between the cultural periods provided by the road_list_values("cultural_period") function and the data itself (cf. plot above)
