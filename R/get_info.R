# tables <- list("geopolitical_units", 
#                "geopolitical_units", 
#                "locality", 
#                "locality", 
#                "archaeological_stratigraphy", 
#                "assemblage", 
#                "attr_values/ex.txt", 
#                c("geological_layer_age", "archaeological_layer_age", "assemblage_age"), 
#                c("geological_layer_age", "archaeological_layer_age", "assemblage_age"),
#                "archaeological_stratigraphy", 
#                "typology",
#                "raw_material",
#                "organic_tools",                     # rethink
#                "feature",                           # rethink
#                "miscellaneous_finds",               # rethink
#                "symbolic_artifacts",                # rethink
#                "publication_desc_humanremains", 
#                "publication_desc_humanremains",
#                "plantremains",
#                "plant_taxonomy", 
#                "plant_taxonomy",
#                "plant_taxonomy", 
#                "taxonomical_classification", 
#                "paleofauna")
# attributes <- c("continent", 
#                 "continent_region", 
#                 "country", 
#                 "type", 
#                 "cultural_period",
#                 "category", 
#                 "example", 
#                 "dating_method", 
#                 "material_dated", 
#                 "technocomplex", 
#                 "tool_list",
#                 "raw_material_list",
#                 "organic_tools:interpretation",       # rethink
#                 "feature:interpretation",             # rethink
#                 "miscellaneous_finds:material",       # rethink
#                 "symbolic_artifacts:interpretation",  # rethink
#                 "humanremains:genus", 
#                 "humanremains:species", 
#                 "plant_remains",
#                 "plant:family",
#                 "plant:genus", 
#                 "plant:species",
#                 "fauna:genus",
#                 "fauna:species")
#' Get parameter value from ROAD Database
#'
#' `road_list_parameter_values` fetches values of a given parameter in the database or 
#' read values from file
#'
#' @param function_parameter name of a function parameter
#' @param function_name name of a function
#'
#' @return List of attribute values.
#' @export
#'
#' @examples road_list_parameter_values("locality_types")
road_list_parameter_values <- function (function_parameter, function_name = NULL)
{
  if (is.null(function_parameter))
    stop("No parameter name is given.")
  
  attribute_name = case_when(
    #(function_name == "road_get_localities" | 
    #  function_name == "road_get_assemblages") & 
    function_parameter == "continents"
      ~ "continent",
    #function_name == "road_get_localities" & 
    function_parameter == "subcontinents"
      ~ "continent_region",
    #function_name == "road_get_localities" & 
    function_parameter == "countries"
      ~ "country",
    #function_name == "road_get_localities" & 
    function_parameter == "locality_types"
      ~ "type",
    #function_name == "road_get_localities" & 
    function_parameter == "cultural_periods"
      ~ "cultural_period",
    #function_name == "road_get_assemblages" &
    function_parameter == "categories" 
      ~ "category",
    function_parameter == "dating_methods" 
      ~ "dating_method",
    function_parameter == "material_dated" 
      ~ "material_dated",
    function_parameter == "technocomplex" 
      ~ "technocomplex",
    function_parameter == "tool_list" 
      ~ "tool_list",
    function_parameter == "raw_material_list" 
      ~ "raw_material_list",
    function_parameter == "transport_distance" 
      ~ "transport_distance",
    function_parameter == "organic_tools_interpretation" 
      ~ "organic_tools:interpretation",
    function_parameter == "symbolic_artifacts_interpretation" 
      ~ "symbolic_artifacts:interpretation",
    function_parameter == "feature_interpretation" 
      ~ "feature:interpretation",
    function_parameter == "miscellaneous_finds_material" 
      ~ "miscellaneous_finds:material",
    function_parameter == "genus" 
      ~ "humanremains:genus",
    function_parameter == "species" 
      ~ "humanremains:species",
    function_parameter == "fauna_genus" 
      ~ "fauna:genus",
    function_parameter == "fauna_species" 
      ~ "fauna:species",
    function_parameter == "plant_remains" 
      ~ "plant_remains",
    function_parameter == "plant_family" 
      ~ "plant:family",
    function_parameter == "plant_genus" 
      ~ "plant:genus",
    function_parameter == "plant_species" 
      ~ "plant:species",
    TRUE  ~ "NULL"
  )
  
  if (attribute_name == "NULL") stop("No such function parameter.")
  
  data <- road_list_values(attribute_name)
  
  return(data)
  
}

#' Retrieve Attribute Values from the ROAD Database
#'
#' The `road_list_values` function allows you to find valid attribute values for a given attribute name. 
#' You can search for attribute names from any `road_get_*` function, and the function will return a list of all possible attribute values used in the associated tables.
#' This is particularly useful when you need to specify an attribute but are unsure of its exact spelling or available values.
#'
#' @param attribute_name name of an attribute.
#'
#' @return List of attribute values.
#' @export
#'
#' @examples
#' road_list_values("category")
#' road_list_values("cultural_period")
road_list_values <- function (attribute_name)
{ 
  tables <- list("geopolitical_units", 
                 "geopolitical_units", 
                 "locality", 
                 "locality", 
                 "archaeological_stratigraphy", 
                 "assemblage", 
                 "attr_values/ex.txt", 
                 c("geological_layer_age", "archaeological_layer_age", "assemblage_age"), 
                 c("geological_layer_age", "archaeological_layer_age", "assemblage_age"),
                 "archaeological_stratigraphy", 
                 "typology",
                 "raw_material",
                 "raw_material",
                 "organic_tools",
                 "feature", 
                 "miscellaneous_finds",
                 "symbolic_artifacts",
                 "publication_desc_humanremains", 
                 "publication_desc_humanremains",
                 "plantremains",
                 "plant_taxonomy", 
                 "plant_taxonomy",
                 "plant_taxonomy", 
                 "taxonomical_classification", 
                 "paleofauna")
  attributes <- c("continent", 
                  "continent_region", 
                  "country", 
                  "type", 
                  "cultural_period",
                  "category", 
                  "example", 
                  "dating_method", 
                  "material_dated", 
                  "technocomplex", 
                  "tool_list",
                  "raw_material_list",
                  "transport_distance",
                  "organic_tools:interpretation",
                  "feature:interpretation", 
                  "miscellaneous_finds:material", 
                  "symbolic_artifacts:interpretation",
                  "humanremains:genus", 
                  "humanremains:species", 
                  "plant_remains",
                  "plant:family",
                  "plant:genus", 
                  "plant:species",
                  "fauna:genus",
                  "fauna:species")
  
  if (is.null(attribute_name))
    stop("No attribute name is given.")
  
  table <- NULL

  # computing length of attributes array
  size = length(attributes)
  # iterating over elements of attributes to get table list
  for (i in 1:size){
    if(attribute_name == attributes[i]) {
      table = tables[i]
    }
  }  
  # table <- c("geological_layer_age", "archaeological_layer_age", "assemblage_age")
  if (is.null(table))
  {
    warning(paste("No data source for parameter ", attribute_name, " was found."))
    return(table)
  }
  
  if (grepl(".txt", table, fixed = TRUE) && grepl("/", table, fixed = TRUE)) {
    data <- read.csv(toString(table))
    return(data)
  }
  
  x <- strsplit(attribute_name, ":")
  
  if (length(x[[1]]) > 1) cm_attribute_name <- x[[1]][2]
  else cm_attribute_name <- attribute_name
  
  # if we use tables <- list(...), all elements of the list are vectors
  q_extension <- paste( "SELECT DISTINCT regexp_replace(", cm_attribute_name,", '.+[1234567890]+', '') AS ",
                        cm_attribute_name,
                        " FROM ( ")
  
  q <- paste( "SELECT 
              DISTINCT(unnest(regexp_split_to_array(", cm_attribute_name, ",',[ ]*'))) AS ",
              cm_attribute_name,
              " from ")
  que <- paste(
    sapply(table, function(x) paste0(q, x)), 
    collapse = " UNION "
  )
  query <- paste0(q_extension, que, ") AS foo ORDER BY ", cm_attribute_name, "")
   message(query)
  data <- road_run_query(query)
  
  return(data)
}

#' Get an Overview of the ROAD Archaeology Database
#'
#' The `road_summarize_archaeology` function provides a quick overview of the presence of a given search term across archaeology-related `road_get_*` functions in the ROAD database.
#' It queries all relevant functions and returns a list indicating whether each function contains the search term. This helps you identify the appropriate `road_get_*` functions for your query.
#' Once identified, you can further query these functions to retrieve the relevant assemblages.
#' 
#' Background: The ROAD archaeology database is rich in categorized data, where archaeological findings are grouped according to their characteristics. These categories can be accessed using a variety of `road_get_*` functions, such as:
#' 
#' - `road_get_lithic_typology()`
#' - `road_get_lithic_raw_material()`
#' - `road_get_organic_tools()`
#' - `road_get_symbolic_artifacts()`
#' - `road_get_feature()`
#' - `road_get_miscellaneous_finds()`
#' 
#' Some search terms may not be easily categorized, while others may appear across multiple categories depending on contextual interpretations. This function assists in identifying the most relevant categories for your search.
#'
#' @param term A string containing the search term (one item).
#' 
#' @return Database search result as list of lithic finds with info about typology.
#' @export
#'
#' @examples 
#' road_summerize_archaeology(term = "Cores")
road_summerize_archaeology <- function(term)
{
  if (is.null(term))
    stop("No term is given.")
  else
    query <- paste0("SELECT * FROM ( ",
                    "SELECT '", term, "' AS term, 'typology' AS table_, 'typology' AS attribute, count(*) AS hit_number 
                   FROM typology WHERE typology ILIKE '%", term, "%'", 
                    " UNION ",
                    "SELECT '", term, "' AS term, 'typology' AS table_, 'tool_list' AS attribute, count(*) AS hit_number 
                   FROM typology WHERE tool_list ILIKE '%", term, "%'",
                    " UNION ",
                    "SELECT '", term, "' AS term, 'typology' AS table_, 'comments' AS attribute, count(*) AS hit_number 
                   FROM typology WHERE comments ILIKE '%", term, "%'",
                    " UNION ",
                    "SELECT '", term, "' AS term, 'technology' AS table_, 'technology' AS attribute, count(*) AS hit_number 
                   FROM technology WHERE technology ILIKE '%", term, "%'",
                    " UNION ",
                    "SELECT '", term, "' AS term, 'technology' AS table_, 'technology_type' AS attribute, count(*) AS hit_number 
                   FROM technology WHERE technology_type ILIKE '%", term, "%'",
                    " UNION ",
                    "SELECT '", term, "' AS term, 'technology' AS table_, 'product_list' AS attribute, count(*) AS hit_number 
                   FROM technology WHERE product_list ILIKE '%", term, "%'",
                    " UNION ",
                    "SELECT '", term, "' AS term, 'technology' AS table_, 'comments' AS attribute, count(*) AS hit_number 
                   FROM technology WHERE comments ILIKE '%", term, "%'",
                    " UNION ",
                    "SELECT '", term, "' AS term, 'function' AS table_, 'functional_traces' AS attribute, count(*) AS hit_number 
                   FROM function WHERE functional_traces ILIKE '%", term, "%'",
                    " UNION ",
                    "SELECT '", term, "' AS term, 'function' AS table_, 'function_list' AS attribute, count(*) AS hit_number 
                   FROM function WHERE function_list ILIKE '%", term, "%'",
                    " UNION ",
                    "SELECT '", term, "' AS term, 'function' AS table_, 'comments' AS attribute, count(*) AS hit_number 
                   FROM function WHERE comments ILIKE '%", term, "%'",
                    " UNION ",
                    "SELECT '", term, "' AS term, 'raw_material' AS table_, 'raw_material_list' AS attribute, count(*) AS hit_number 
                   FROM raw_material WHERE raw_material_list ILIKE '%", term, "%'", 
                    " UNION ",
                    "SELECT '", term, "' AS term, 'raw_material' AS table_, 'comments' AS attribute, count(*) AS hit_number 
                   FROM raw_material WHERE comments ILIKE '%", term, "%'", 
                    " UNION ",
                    "SELECT '", term, "' AS term, 'symbolic_artifacts' AS table_, 'material' AS attribute, count(*) AS hit_number 
                   FROM symbolic_artifacts WHERE material ILIKE '%", term, "%'",
                    " UNION ",
                    "SELECT '", term, "' AS term, 'symbolic_artifacts' AS table_, 'interpretation' AS attribute, count(*) AS hit_number 
                   FROM symbolic_artifacts WHERE interpretation ILIKE '%", term, "%'",
                    " UNION ",
                    "SELECT '", term, "' AS term, 'symbolic_artifacts' AS table_, 'technology' AS attribute, count(*) AS hit_number 
                   FROM symbolic_artifacts WHERE technology ILIKE '%", term, "%'",
                    " UNION ",
                    "SELECT '", term, "' AS term, 'symbolic_artifacts' AS table_, 'category' AS attribute, count(*) AS hit_number 
                   FROM symbolic_artifacts WHERE category ILIKE '%", term, "%'",
                    " UNION ",
                    "SELECT '", term, "' AS term, 'symbolic_artifacts' AS table_, 'raw_material_source' AS attribute, count(*) AS hit_number 
                   FROM symbolic_artifacts WHERE raw_material_source ILIKE '%", term, "%'",
                    " UNION ",
                    "SELECT '", term, "' AS term, 'symbolic_artifacts' AS table_, 'comments' AS attribute, count(*) AS hit_number 
                   FROM symbolic_artifacts WHERE comments ILIKE '%", term, "%'",
                    " UNION ",
                    "SELECT '", term, "' AS term, 'organic_tools' AS table_, 'organic_raw_material' AS attribute, count(*) AS hit_number 
                   FROM organic_tools WHERE organic_raw_material ILIKE '%", term, "%'",
                    " UNION ",
                    "SELECT '", term, "' AS term, 'organic_tools' AS table_, 'interpretation' AS attribute, count(*) AS hit_number 
                   FROM organic_tools WHERE interpretation ILIKE '%", term, "%'",
                    " UNION ",
                    "SELECT '", term, "' AS term, 'organic_tools' AS table_, 'technology' AS attribute, count(*) AS hit_number 
                   FROM organic_tools WHERE technology ILIKE '%", term, "%'",
                    " UNION ",
                    "SELECT '", term, "' AS term, 'organic_tools' AS table_, 'comments' AS attribute, count(*) AS hit_number 
                   FROM organic_tools WHERE comments ILIKE '%", term, "%'",
                    " UNION ",
                    "SELECT '", term, "' AS term, 'miscellaneous_finds' AS table_, 'raw_material_source' AS attribute, count(*) AS hit_number 
                   FROM miscellaneous_finds WHERE raw_material_source ILIKE '%", term, "%'",
                    " UNION ",
                    "SELECT '", term, "' AS term, 'miscellaneous_finds' AS table_, 'material' AS attribute, count(*) AS hit_number 
                   FROM miscellaneous_finds WHERE material ILIKE '%", term, "%'",
                    " UNION ",
                    "SELECT '", term, "' AS term, 'miscellaneous_finds' AS table_, 'comments' AS attribute, count(*) AS hit_number 
                   FROM miscellaneous_finds WHERE comments ILIKE '%", term, "%'",
                    " UNION ",
                    "SELECT '", term, "' AS term, 'feature' AS table_, 'interpretation' AS attribute, count(*) AS hit_number 
                   FROM feature WHERE interpretation ILIKE '%", term, "%'",
                    " UNION ",
                    "SELECT '", term, "' AS term, 'feature' AS table_, 'comments' AS attribute, count(*) AS hit_number 
                   FROM feature WHERE comments ILIKE '%", term, "%'",
                    " ) as foo ORDER BY hit_number DESC,table_, attribute "
    )

  data <- road_run_query(query)
  
  return(data)
}