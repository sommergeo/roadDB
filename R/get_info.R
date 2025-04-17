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
               "organic_tools",                     # rethink
               "feature",                           # rethink
               "miscellaneous_finds",               # rethink
               "symbolic_artifacts",                # rethink
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
                "organic_tools:interpretation",       # rethink
                "feature:interpretation",             # rethink
                "miscellaneous_finds:material",       # rethink
                "symbolic_artifacts:interpretation",  # rethink
                "humanremains:genus", 
                "humanremains:species", 
                "plant_remains",
                "plant:family",
                "plant:genus", 
                "plant:species",
                "fauna:genus",
                "fauna:species")

#' Get attribute value from ROAD Database
#'
#' `road_list_values` fetches values of a given attribute in the database or 
#' read values from file
#'
#'#' All parameters are optional and should be omitted or set to NULL when not used.
#'
#' @param attribute_name name of an attribute; defaults to NULL.
#'
#' @return List of attribute values.
#' @export
#'
#' @examples road_list_values("category")
#' @examples road_list_values("cultural_period")
road_list_values <- function (attribute_name = NULL)
{
  if (is.null(attribute_name))
    return("No attribute name is given.")
  
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
    return(paste("No data source for parameter ", attribute_name, " was not found."))
  
  if (grepl(".txt", table, fixed = TRUE) && grepl("/", table, fixed = TRUE)) {
    data <- read.csv(toString(table))
    return(data)
  }
  
  x <- strsplit(attribute_name, ":")
  
  if (length(x[[1]]) > 1) cm_attribute_name <- x[[1]][2]
  else cm_attribute_name <- attribute_name
  
  # if we use tables <- list(...), all elements of the list are vectors
  q_extension <- paste( "SELECT DISTINCT regexp_replace(", cm_attribute_name,", '.+[1234567890 ]+', '') AS ",
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
  
  # query <- paste( "SELECT DISTINCT ", attribute_name, " FROM (select distinct(unnest(string_to_array(
  # string_agg(", attribute_name, ", ', '),', '))) as ",
  # attribute_name, ", 'dummy' as dummy from ", table,  " GROUP BY dummy) as foo ", 
  # " ORDER BY ", attribute_name)
  
  
  
  data <- road_run_query(query)
  
  #data_d <- data.frame(lapply(data, function(x) {gsub("1|2|3|5|6|7<89|0", "", x)}))
  
  return(data)
}

#' Get table and attribute overview of ROAD database
#'
#' `road_summerize_archaeology` fetches archaeological tables and their attributes from ROAD database.
#'
#'
#' @param term string (one item)
#' 
#' @return Database search result as list of lithic finds with info about typology.
#' @export
#'
#' @examples road_summerize_archaeology(term = "Cores")
road_summerize_archaeology <- function(term = NULL)
{
  if (!is.null(term))
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
  else query <- "SELECT null AS term, null AS table, null AS attribute, null AS hit_number"
  
  data <- road_run_query(query)
  #colnames(data)
  
  return(data)
}