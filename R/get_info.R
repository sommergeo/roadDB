#' Get argument value from ROAD Database
#'
#' The  \strong{\code{road_list_argument_values}} function fetches values of a 
#' given argument in the database or read values from file
#'
#' @param function_argument name of a function argument
#'
#' @return List of attribute values.
#' @export
#'
#' @examples road_list_argument_values("locality_types")
road_list_argument_values <- function(function_argument)
{
  if (is.null(function_argument))
    stop("No argument name is given.")

  attribute_name <- ""
  table_names <- NULL

  attribute_name <- case_when(
    function_argument == "continents"
      ~ "continent",
    function_argument == "subcontinents"
      ~ "continent_region",
    function_argument == "countries"
      ~ "country",
    function_argument == "locality_types"
      ~ "type",
    function_argument == "cultural_periods"
      ~ "cultural_period",
    function_argument == "categories"
      ~ "category",
    function_argument == "dating_methods"
      ~ "dating_method",
    function_argument == "material_dated"
      ~ "material_dated",
    function_argument == "technocomplexes"
      ~ "technocomplex",
    function_argument == "tool_list"
      ~ "tool_list",
    function_argument == "raw_material_list"
      ~ "raw_material_list",
    function_argument == "transport_distance"
      ~ "transport_distance",
    function_argument == "organic_tools_interpretation"
      ~ "interpretation",
    function_argument == "symbolic_artifacts_interpretation"
      ~ "interpretation",
    function_argument == "feature_interpretation"
      ~ "interpretation",
    function_argument == "miscellaneous_finds_material"
      ~ "material",
    function_argument == "human_genus"
      ~ "genus",
    function_argument == "human_species"
      ~ "species",
    function_argument == "fauna_genus"
      ~ "genus",
    function_argument == "fauna_species"
      ~ "species",
    function_argument == "plant_remains"
      ~ "plant_remains",
    function_argument == "plant_family"
      ~ "family",
    function_argument == "plant_genus"
      ~ "genus",
    function_argument == "plant_species"
      ~ "species",
    function_argument == "assemblages"
    ~ "assemblages",
    TRUE  ~ "NULL"
  )

  table_names <- case_when(
    function_argument == "continents"
      ~ "geopolitical_units",
    function_argument == "subcontinents"
      ~ "geopolitical_units",
    function_argument == "countries"
      ~ "locality",
    function_argument == "locality_types"
      ~ "locality",
    function_argument == "cultural_periods"
      ~ "archaeological_stratigraphy",
    function_argument == "categories"
      ~ "assemblage",
    function_argument == "technocomplexes"
      ~ "archaeological_stratigraphy",
    function_argument == "tool_list"
      ~ "typology",
    function_argument == "raw_material_list"
      ~ "raw_material",
    function_argument == "transport_distance"
      ~ "raw_material",
    function_argument == "organic_tools_interpretation"
      ~ "organic_tools",
    function_argument == "symbolic_artifacts_interpretation"
      ~ "symbolic_artifacts",
    function_argument == "feature_interpretation"
      ~ "feature",
    function_argument == "miscellaneous_finds_material"
      ~ "miscellaneous_finds",
    function_argument == "human_genus"
      ~ "publication_desc_humanremains",
    function_argument == "human_species"
      ~ "publication_desc_humanremains",
    function_argument == "fauna_genus"
      ~ "taxonomical_classification",
    function_argument == "fauna_species"
      ~ "paleofauna",
    function_argument == "plant_remains"
      ~ "plantremains",
    function_argument == "plant_family"
      ~ "plant_taxonomy",
    function_argument == "plant_genus"
      ~ "plant_taxonomy",
    function_argument == "plant_species"
      ~ "plant_taxonomy",
    #TRUE  = NULL
  )

  if (function_argument == "dating_methods" || function_argument == "material_dated")
   table_names <- c("geological_layer_age", "archaeological_layer_age", "assemblage_age")

  if (is.null(table_names)) stop("No table name found.")
  #if (length(table_names) < 1) stop("No table name found.") 
  if (attribute_name == "NULL") stop("No attribute name found.")

  data <- road_list_values(table_names = table_names, attribute_name = attribute_name)
  
  return(data)
}

#' Retrieve Attribute Values from the ROAD Database
#'
#' The  \strong{\code{road_list_values}} function allows you to find valid 
#' attribute values for a given attribute name. You can search for attribute 
#' names from any `road_get_*` function, and the function will return a list of 
#' all possible attribute values used in the associated tables. This is 
#' particularly useful when you need to specify an attribute but are unsure of 
#' its exact spelling or available values.
#'
#' @param attribute_name is a name of an attribute.
#' @param table_names is a table name or table names to which the attribute belongs.
#'
#' @return List of attribute values.
#' @export
#'
#' @examples
#' road_list_values(table_names = "assemblage", attribute_name = "category")
#' road_list_values(table_names = c("geological_layer_age", 
#' "archaeological_layer_age", "assemblage_age"), attribute_name = "dating_method")
road_list_values <- function (table_names, attribute_name)
{ 
   if (is.null(attribute_name))
    stop("No attribute name is given.")

   if (attribute_name == 'assemblages') return("List of assemblages; return value from function 'road_get_assemblages'. Can be used instead of the other locality and assemblage parameters to filter the results.")
  
   if (is.null(table_names))
    stop(paste("No data source for argument ", attribute_name, " was found."))

   # table_names is a file name
   #if (grepl(".txt", table_names, fixed = TRUE) && grepl("/", table_names, fixed = TRUE)) {
   # data <- read.csv(toString(table_names))
   # return(data)
   #}

   cm_attribute_name <- attribute_name

   q_extension <- ""
   q <- ""

   q_extension <- paste( "SELECT DISTINCT regexp_replace(", cm_attribute_name,", ' +[1234567890]+', '') AS ",
                        cm_attribute_name,
                        " FROM ( ")

   q <- paste( "SELECT 
              DISTINCT(unnest(regexp_split_to_array(", cm_attribute_name, ",',[ ]*'))) AS ",
              cm_attribute_name,
              " from ")
   if (cm_attribute_name == "transport_distance") q <- paste( "SELECT DISTINCT ", cm_attribute_name,
                                                              " AS ", cm_attribute_name,
                                                              " from ")

   que <- paste(
    sapply(table_names, function(x) paste0(q, x)), 
    collapse = " UNION "
   )
   query <- paste0(q_extension, que, ") AS foo ORDER BY ", cm_attribute_name, "")

   # First exception
   if (cm_attribute_name == "transport_distance") query <- paste( "SELECT DISTINCT ", cm_attribute_name,
                                                              " AS ", cm_attribute_name,
                                                              " FROM ", table_names)
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
#' road_summarize_archaeology(term = "Cores")
road_summarize_archaeology <- function(term)
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