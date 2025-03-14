library(assertthat)
library(RPostgres)
# library(stringr)

tables <- list("feature", "locality", "geopolitical_units", "geopolitical_units", "locality",  
            "assemblage", "archaeological_stratigraphy", "attr_values/ex.txt", 
            c("geological_layer_age", "archaeological_layer_age", "assemblage_age"), 
            c("geological_layer_age", "archaeological_layer_age", "assemblage_age"),
            "archaeological_stratigraphy", "miscellaneous_finds", "publication_desc_humanremains", 
            "symbolic_artifacts",
            "publication_desc_humanremains", "organic_tools", "raw_material", 
            "symbolic_artifacts", "typology")
attributes <- c("feature:interpretation", "type", "continent", "continent_region", 
                "country", "category", 
                "cultural_period", "example", "dating_method", "material_dated", 
                "technocomplex","miscellaneous_finds:material", "humanremains:genus", 
                "symbolic_artifacts:interpretation",
                "humanremains:species", "organic_tools:interpretation", "raw_material_list", 
                "symbolic_artifacts:interpretation", "tool_list")


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
  query <- paste0(q_extension, que, ") AS foo ORDER BY ", cm_attribute_name)
  
  # message(query)
  
  # query <- paste( "SELECT DISTINCT ", attribute_name, " FROM (select distinct(unnest(string_to_array(
  # string_agg(", attribute_name, ", ', '),', '))) as ",
  # attribute_name, ", 'dummy' as dummy from ", table,  " GROUP BY dummy) as foo ", 
  # " ORDER BY ", attribute_name)
  
  
  
  data <- road_run_query(query)
  
  #data_d <- data.frame(lapply(data, function(x) {gsub("1|2|3|5|6|7<89|0", "", x)}))
  
  return(data)
}

#' Get dates for assemblages, geolayers and archlayers from ROAD Database
#'
#' `road_get_dates` fetches records of the age tables in ROAD
#'
#' All parameters are optional and should be omitted or set to NULL when not used.
#' @param continents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param subcontinents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param countries string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param locality_types string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param cultural_periods string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param localities list of localities; return value from function `road_get_localities`.
#' @param categories string (one item) or vector of strings (one or more items).
#' @param assemblages list of assemblages; return value from function `road_get_assemblages`.
#' @param dating_methods string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param material_dated string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param age_min integer; defaults to NULL.
#' @param age_max integer; defaults to NULL.
#' @param technocomplex string (one item) or vector of strings (one or more items); defaults to NULL.
#'
#' @return date records
#' @export
#'
#' @examples road_get_dates(dating_methods = c("geology", "biostratigraphy"))
#' @examples road_get_dates(material_dated = c("coprolite", "glass", "ivory"), age_min = 10000L, 
#'                          age_max = 100000L, dating_methods = c("geology", "biostratigraphy"))
#' @examples road_get_dates(countries = c("Germany", "France"), cultural_periods = "Middle Paleolithic")                         
#'                          
#'                          
road_get_dates <- function (continents = NULL, subcontinents = NULL, countries = NULL, 
                            locality_types = NULL, cultural_periods = NULL, 
                            dating_methods = NULL, material_dated = NULL, 
                            age_min = NULL, age_max = NULL, technocomplex = NULL, 
                            categories = NULL, assemblages = NULL, localities = NULL)
{
  if ((!is.null(age_min) && !is.integer(age_min)) || (!is.null(age_max) && !is.integer(age_max)))
    stop("Parameters 'min_age' and 'max_age' have to be integers.")
  
  if (!is.null(age_min) && !is.null(age_max) && age_min > age_max)
    stop("Parameter 'min_age' can not be bigger than 'max_age'.")

  # calculate locality_condition
  # To do: !is.null(one of localities parameters) AND !is.null(localities)  ---> Warnung an den Benutzer
  if (is.null(localities)) localities <- road_get_localities(continents = continents, 
                                                             subcontinents = subcontinents, 
                                                             countries = countries, 
                                                             locality_types = locality_types, 
                                                             cultural_periods = cultural_periods)
  
  # locality_condition <- get_locality_condition(localities = localities)
  query_localities <- paste(
    sapply(localities$locality_id, function(x) paste0("'", x, "'")),
    collapse = ", "
  )
  # calculate output extention
  locality_info_for_output <- get_output_extention_locality(localities)

  # calculate assemblage_condition
  # To do: !is.null(categories) AND !is.null(assemblages)  ---> Warnung an den Benutzer
  if (is.null(assemblages)) assemblages <- road_get_assemblages(categories = categories, localities = localities)
  assemblage_condition <- get_assemblage_condition(query_start = "AND ", assemblages = assemblages)
  # calculate output extention
  assemblage_info_for_output <- get_output_extention_assemblage(assemblages)

  # select fields
  select_fields_gla <- c(
    paste0("geological_layer_age.geolayer_idlocality AS  ", cm_locality_idlocality),
    # paste0("CAST(assemblage_idassemblage AS TEXT) AS  ", cm_assemblages_idassemblage),
    paste0("assemblage_idassemblage AS ", cm_assemblages_idassemblage),
    # paste0("assemblage.category AS \"", cm_assemblages_categories, "\""),
    paste0("geological_layer_age.geolayer_name AS ", cm_geolayer_geolayer_name),
    paste0("archlayer_name AS ", cm_archlayer_archlayer_name),
    paste0("age AS ", cm_age),
    paste0("negative_standard_deviation AS ", cm_negative_standard_deviation),
    paste0("positive_standard_deviation AS ", cm_positive_standard_deviation),
    paste0("material_dated AS ", cm_material_dated),
    paste0("dating_method AS ", cm_dating_method),
    paste0("laboratory_idlaboratory AS ", cm_laboratory_idlaboratory),
    paste0("technocomplex as technocomplex")
  )
  
  select_fields_ala <- c(
    paste0("archaeological_layer_age.archlayer_idlocality AS ", cm_locality_idlocality),
    # paste0("CAST(assemblage_idassemblage AS TEXT) AS ", cm_assemblages_idassemblage),
    paste0("assemblage_idassemblage AS ", cm_assemblages_idassemblage),
    # paste0("assemblage.category AS ", cm_assemblages_categories),
    paste0("archlayer_correl_geolayer.geolayer_name AS ", cm_geolayer_geolayer_name),
    paste0("archaeological_layer_age.archlayer_name AS ", cm_archlayer_archlayer_name),
    paste0("age AS ", cm_age),
    paste0("negative_standard_deviation AS ", cm_negative_standard_deviation),
    paste0("positive_standard_deviation AS ", cm_positive_standard_deviation),
    paste0("material_dated AS ", cm_material_dated),
    paste0("dating_method AS ", cm_dating_method),
    paste0("laboratory_idlaboratory AS ", cm_laboratory_idlaboratory),
    paste0("technocomplex as technocomplex")
  )
  
  select_fields_asa <- c(
    paste0("assemblage_age.assemblage_idlocality AS ", cm_locality_idlocality),
    # paste0("CAST(assemblage_age.assemblage_idassemblage AS TEXT) AS  \"", cm_assemblages_idassemblage, "\""),
    paste0("assemblage_age.assemblage_idassemblage AS ", cm_assemblages_idassemblage),
    # paste0("NULL AS ", cm_assemblages_categories, " "),
    paste0("assemblage_in_geolayer.geolayer_name AS ", cm_geolayer_geolayer_name),
    paste0("archlayer_name AS ", cm_archlayer_archlayer_name),
    paste0("age AS ", cm_age),
    paste0("negative_standard_deviation AS ", cm_negative_standard_deviation),
    paste0("positive_standard_deviation AS ", cm_positive_standard_deviation),
    paste0("material_dated AS ", cm_material_dated),
    paste0("dating_method AS ", cm_dating_method),
    paste0("laboratory_idlaboratory AS ", cm_laboratory_idlaboratory),
    paste0("technocomplex as technocomplex")
  )
  
  query <- paste0("SELECT * FROM (SELECT ", paste(select_fields_gla, collapse = ", "),
            " FROM geological_layer_age ",
            "LEFT JOIN assemblage_in_geolayer ON ",
            " assemblage_in_geolayer.geolayer_idlocality = geological_layer_age.geolayer_idlocality ",
            " AND assemblage_in_geolayer.geolayer_name = geological_layer_age.geolayer_name ", 
            "LEFT JOIN archlayer_correl_geolayer ON ",
            "archlayer_correl_geolayer.geolayer_idlocality = geological_layer_age.geolayer_idlocality",
            " AND archlayer_correl_geolayer.geolayer_name = geological_layer_age.geolayer_name ",
            "LEFT JOIN archaeological_layer ON ",
            "locality_idlocality = archlayer_idlocality ",
            " AND name =  archlayer_name ",
            "LEFT JOIN archaeological_stratigraphy ON ",
            " archstratigraphy_idarchstrat = idarchstrat ",
            "UNION
            SELECT ", paste(select_fields_ala, collapse = ", "), 
            " FROM archaeological_layer_age ",
            "LEFT JOIN archaeological_layer ON ",
            "locality_idlocality = archlayer_idlocality ",
            " AND name =  archlayer_name ",
            "LEFT JOIN archaeological_stratigraphy ON ",
            " archstratigraphy_idarchstrat = idarchstrat ",
            "LEFT JOIN archlayer_correl_geolayer ON ",
            "archlayer_correl_geolayer.archlayer_idlocality = archaeological_layer_age.archlayer_idlocality",
            " AND archlayer_correl_geolayer.archlayer_name = archaeological_layer_age.archlayer_name ",
            "LEFT JOIN assemblage_in_geolayer ON ",
            " assemblage_in_geolayer.geolayer_idlocality = archlayer_correl_geolayer.geolayer_idlocality ",
            " AND assemblage_in_geolayer.geolayer_name = archlayer_correl_geolayer.geolayer_name ",
            "UNION
            SELECT ", paste(select_fields_asa, collapse = ", "),
            " FROM assemblage_age ",
            "LEFT JOIN assemblage_in_geolayer ON ",
            " assemblage_in_geolayer.assemblage_idlocality = assemblage_age.assemblage_idlocality ",
            " AND assemblage_in_geolayer.assemblage_idassemblage = assemblage_age.assemblage_idassemblage ",
            "LEFT JOIN archlayer_correl_geolayer ON ",
            "archlayer_correl_geolayer.geolayer_idlocality = assemblage_in_geolayer.geolayer_idlocality ",
            " AND archlayer_correl_geolayer.geolayer_name =  assemblage_in_geolayer.geolayer_name ",
            "LEFT JOIN  archaeological_layer ON ",
            "locality_idlocality = archlayer_idlocality ",
            "AND name = archlayer_name ",
            "LEFT JOIN archaeological_stratigraphy ON ",
            " archstratigraphy_idarchstrat = idarchstrat",
            ") as foo ",
            "WHERE ", cm_locality_idlocality," IN (",
            query_localities, ") ", 
            assemblage_condition,
            query_check_intersection("AND ", dating_methods, "dating_method "),
            query_check_intersection("AND ", material_dated, "material_dated "),
            parameter_to_query("AND age  <= ", age_max, " "),
            parameter_to_query("AND age  >= ", age_min, " "),
            query_check_intersection("AND ", technocomplex, "technocomplex"),
            " ORDER BY ", cm_locality_idlocality, ", ", cm_geolayer_geolayer_name, 
            ", ", cm_archlayer_archlayer_name)
  
  data <- road_run_query(query)
  
  data$negative_standard_deviation[data$negative_standard_deviation == "-1"] <- NA
  data$positive_standard_deviation[data$positive_standard_deviation == "-1"] <- NA
  data$age[data$age == "-1"] <- NA
  data$dating_method[data$dating_method == ""] <- NA
  data$material_dated[data$material_dated == ""] <- NA
  data$laboratory_idlaboratory[data$laboratory_idlaboratory == ""] <- NA
  
  data_plus_assemblage_info <- merge(x = data, y = assemblage_info_for_output, 
            by = c(cm_locality_idlocality, cm_assemblages_idassemblage))
  
  return(merge(x = data_plus_assemblage_info, y = locality_info_for_output, by = cm_locality_idlocality))
  
}


#' Get lithic typology from ROAD database
#'
#' `road_get_lithic_typology` fetches data of lithic finds from ROAD database.
#'
#'
#' @param continents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param subcontinents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param countries string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param locality_types string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param cultural_periods string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param localities list of localities; return value from function `road_get_localities`.
#' @param categories string (one item) or vector of strings (one or more items).
#' @param age_min integer; minimum age of assemblage.
#' @param age_max integer; maximum age of assemblage.
#' @param assemblages list of assemblages; return value from function `road_get_assemblages`.
#' @param tool_list string (one item) or vector of strings
#' 
#' @return Database search result as list of lithic finds with info about typology.
#' @export
#'
#' @examples road_get_lithic_typology(continents = "Europe")
#' @examples road_get_lithic_typology(continents = "Europe",)
#' @examples road_get_lithic_typology(continents = "Europe",)
road_get_lithic_typology <- function(continents = NULL, subcontinents = NULL, countries = NULL, 
                                 locality_types = NULL, cultural_periods = NULL, categories = NULL, 
                                 age_min = NULL, age_max = NULL, tool_list = NULL,
                                 assemblages = NULL, localities = NULL)
{
  # calculate assemblage_condition
  # To do: !is.null(categories) AND !is.null(assemblages)  ---> Warnung an den Benutzer
  if (is.null(assemblages)) assemblages <- road_get_assemblages(continents = continents,
                                                                subcontinents = subcontinents,
                                                                countries = countries,
                                                                categories = categories, 
                                                                locality_types = locality_types,
                                                                cultural_periods = cultural_periods,
                                                                age_min = age_min, age_max = age_max, 
                                                                localities = localities)
  assemblage_condition <- get_assemblage_condition(query_start = "AND ", assemblages = assemblages)
  # calculate output extention
  assemblage_info_for_output <- get_output_extention_assemblage(assemblages = assemblages)
  
  # select fields
  select_fields <- c(
    paste0(" assemblage_idlocality AS ", cm_locality_idlocality),
    paste0(" assemblage_idassemblage AS ", cm_assemblages_idassemblage),
    paste0(" tool_list AS ", cm_tool_list),
    paste0(" typology AS ", cm_typology),
    "percentage",
    "comments"
  )
  
  if (!is.null(tool_list)) 
    tool_list_condition <- query_check_intersection("AND ", tool_list, cm_tool_list)
  else 
    tool_list_condition <- ""
    
  query <- paste(
   "SELECT DISTINCT * FROM (",
   "SELECT DISTINCT ",
     paste(select_fields, collapse = ", "),
     " FROM typology) as foo WHERE true ",
     assemblage_condition,
     tool_list_condition
  )

  # message(query)
  data <- road_run_query(query)
  
  return (merge(x = data, y = assemblage_info_for_output, 
                by = c(cm_locality_idlocality, cm_assemblages_idassemblage)))
}


#' Get lithic raw material from ROAD database
#'
#' `road_get_lithic_raw_material` fetches data of lithic finds from ROAD database.
#'
#'
#' @param continents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param subcontinents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param countries string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param locality_types string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param cultural_periods string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param localities list of localities; return value from function `road_get_localities`.
#' @param categories string (one item) or vector of strings (one or more items).
#' @param age_min integer; minimum age of assemblage.
#' @param age_max integer; maximum age of assemblage.
#' @param assemblages list of assemblages; return value from function `road_get_assemblages`.
#' @param raw_material_list string (one item) or vector of strings
#' 
#' @return Database search result as list of lithic finds with info about raw material.
#' @export
#'
#' @examples road_get_lithic_typology(continents = "Europe")
#' @examples road_get_lithic_typology(continents = "Europe", )
#' @examples road_get_lithic_typology(continents = "Europe", ))
road_get_lithic_raw_material <- function(continents = NULL, subcontinents = NULL, countries = NULL, 
                                     locality_types = NULL, cultural_periods = NULL, categories = NULL, 
                                     age_min = NULL, age_max = NULL, raw_material_list = NULL,
                                     assemblages = NULL, localities = NULL)
{
  # calculate assemblage_condition
  # To do: !is.null(categories) AND !is.null(assemblages)  ---> Warnung an den Benutzer
  if (is.null(assemblages)) assemblages <- road_get_assemblages(continents = continents,
                                                                subcontinents = subcontinents,
                                                                countries = countries,
                                                                categories = categories, 
                                                                locality_types = locality_types,
                                                                cultural_periods = cultural_periods,
                                                                age_min = age_min, age_max = age_max, 
                                                                localities = localities)
  assemblage_condition <- get_assemblage_condition(query_start = "AND ", assemblages = assemblages)
  # calculate output extention
  assemblage_info_for_output <- get_output_extention_assemblage(assemblages = assemblages)
  
  # select fields
  select_fields <- c(
    paste0(" assemblage_idlocality AS ", cm_locality_idlocality),
    paste0(" assemblage_idassemblage AS ", cm_assemblages_idassemblage),
    paste0(" raw_material_list AS ", cm_raw_material_list),
    paste0(" transport_distance AS ", cm_transport_distance),
    "percentage",
    "comments"
  )
  
  if (!is.null(raw_material_list)) 
    raw_material_condition <- query_check_intersection("AND ", raw_material_list, cm_raw_material_list)
  else 
    raw_material_condition <- ""
  
  query <- paste(
    "SELECT DISTINCT * FROM (",
    "SELECT DISTINCT ",
    paste(select_fields, collapse = ", "),
    " FROM raw_material) as foo WHERE true ",
    assemblage_condition,
    raw_material_condition
  )
  
  # message(query)
  data <- road_run_query(query)
  
  #data_plus_assemblage_info <- 
  return(merge(x = data, y = assemblage_info_for_output, 
                by = c(cm_locality_idlocality, cm_assemblages_idassemblage)))
}


#' Get organic tools from ROAD database
#'
#' `road_get_organic_tools` fetches data of organic tools from ROAD database.
#'
#'
#' @param continents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param subcontinents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param countries string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param locality_types string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param cultural_periods string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param localities list of localities; return value from function `road_get_localities`.
#' @param categories string (one item) or vector of strings (one or more items).
#' @param age_min integer; minimum age of assemblage.
#' @param age_max integer; maximum age of assemblage.
#' @param assemblages list of assemblages; return value from function `road_get_assemblages`.
#' @param organic_tools_interpretation string (one item) or vector of strings
#' 
#' @return Database search result as list of organic tools.
#' @export
#'
#' @examples road_get_organic_tools(continents = "Europe")
#' @examples road_get_organic_tools(continents = "Europe", )
#' @examples road_get_organic_tools(continents = "Europe", )
road_get_organic_tools <- function(continents = NULL, subcontinents = NULL, countries = NULL, 
                                         locality_types = NULL, cultural_periods = NULL, categories = NULL, 
                                         age_min = NULL, age_max = NULL, organic_tools_interpretation = NULL,
                                         assemblages = NULL, localities = NULL)
{
  # calculate assemblage_condition
  # To do: !is.null(categories) AND !is.null(assemblages)  ---> Warnung an den Benutzer
  if (is.null(assemblages)) assemblages <- road_get_assemblages(continents = continents,
                                                                subcontinents = subcontinents,
                                                                countries = countries,
                                                                categories = categories, 
                                                                locality_types = locality_types,
                                                                cultural_periods = cultural_periods,
                                                                age_min = age_min, age_max = age_max, 
                                                                localities = localities)
  assemblage_condition <- get_assemblage_condition(query_start = "AND ", assemblages = assemblages)
  # calculate output extention
  assemblage_info_for_output <- get_output_extention_assemblage(assemblages = assemblages)
  
  # select fields
  select_fields <- c(
    paste0(" assemblage_idlocality AS ", cm_locality_idlocality),
    paste0(" assemblage_idassemblage AS ", cm_assemblages_idassemblage),
    paste0(" interpretation AS ", cm_organic_tools_interpretation),
    paste0(" organic_raw_material AS ", cm_organic_raw_material),
    paste0(" technology AS ", cm_organic_tools_technology),
    "number",
    "comments"
  )
  
  if (!is.null(organic_tools_interpretation)) 
    organic_tools_interpretation_condition <- query_check_intersection("AND ", 
                                                                       organic_tools_interpretation, 
                                                                       cm_organic_tools_interpretation)
  else 
    organic_tools_interpretation_condition <- ""
  
  query <- paste(
    "SELECT DISTINCT * FROM (",
    "SELECT DISTINCT ",
    paste(select_fields, collapse = ", "),
    " FROM organic_tools) as foo WHERE true ",
    assemblage_condition,
    organic_tools_interpretation_condition
  )
  
  # message(query)
  data <- road_run_query(query)
  
  return(merge(x = data, y = assemblage_info_for_output, 
               by = c(cm_locality_idlocality, cm_assemblages_idassemblage)))
}


#' Get symbolic artifacts from ROAD database
#'
#' `road_get_symbolic_artifacts` fetches data of symbolic artifacts from ROAD database.
#'
#'
#' @param continents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param subcontinents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param countries string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param locality_types string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param cultural_periods string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param localities list of localities; return value from function `road_get_localities`.
#' @param categories string (one item) or vector of strings (one or more items).
#' @param age_min integer; minimum age of assemblage.
#' @param age_max integer; maximum age of assemblage.
#' @param assemblages list of assemblages; return value from function `road_get_assemblages`.
#' @param symbolic_artifacts_interpretation string (one item) or vector of strings
#' 
#' @return Database search result as list of symbolic artifacts with info about symbolic.
#' @export
#'
#' @examples road_get_symbolic_artifacts(continents = "Europe")
#' @examples road_get_symbolic_artifacts(continents = "Europe", )
#' @examples road_get_symbolic_artifacts(continents = "Europe", )
road_get_symbolic_artifacts <- function(continents = NULL, subcontinents = NULL, countries = NULL, 
                                   locality_types = NULL, cultural_periods = NULL, categories = NULL, 
                                   age_min = NULL, age_max = NULL, 
                                   symbolic_artifacts_interpretation = NULL,
                                   assemblages = NULL, localities = NULL)
{
  # calculate assemblage_condition
  # To do: !is.null(categories) AND !is.null(assemblages)  ---> Warnung an den Benutzer
  if (is.null(assemblages)) assemblages <- road_get_assemblages(continents = continents,
                                                                subcontinents = subcontinents,
                                                                countries = countries,
                                                                categories = categories, 
                                                                locality_types = locality_types,
                                                                cultural_periods = cultural_periods,
                                                                age_min = age_min, age_max = age_max, 
                                                                localities = localities)
  assemblage_condition <- get_assemblage_condition(query_start = "AND ", assemblages = assemblages)
  # calculate output extention
  assemblage_info_for_output <- get_output_extention_assemblage(assemblages = assemblages)
  
  # select fields
  select_fields <- c(
    paste0(" assemblage_idlocality AS ", cm_locality_idlocality),
    paste0(" assemblage_idassemblage AS ", cm_assemblages_idassemblage),
    paste0(" interpretation AS ", cm_symbolic_artifacts_interpretation),
    paste0(" category AS ", cm_symbolic_artifacts_category),
    paste0(" material AS ", cm_symbolic_artifacts_material),
    paste0(" technology AS ", cm_symbolic_artifacts_technology),
    paste0(" raw_material_source AS ", cm_symbolic_artifacts_raw_material_source),
    "comments"
  )
  
  if (!is.null(symbolic_artifacts_interpretation)) 
    symbolic_artifacts_interpretation_condition <- query_check_intersection("AND ", 
                                                                            symbolic_artifacts_interpretation, 
                                                                            cm_symbolic_artifacts_interpretation)
  else 
    symbolic_artifacts_interpretation_condition <- ""
  
  query <- paste(
    "SELECT DISTINCT * FROM (",
    "SELECT DISTINCT ",
    paste(select_fields, collapse = ", "),
    " FROM symbolic_artifacts) as foo WHERE true ",
    assemblage_condition,
    symbolic_artifacts_interpretation_condition
  )
  
  # message(query)
  data <- road_run_query(query)
  
  return(merge(x = data, y = assemblage_info_for_output, 
               by = c(cm_locality_idlocality, cm_assemblages_idassemblage)))
}


#' Get feature assemblages from ROAD database
#'
#' `road_get_feature` fetches data of feature finds from ROAD database.
#'
#'
#' @param continents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param subcontinents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param countries string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param locality_types string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param cultural_periods string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param localities list of localities; return value from function `road_get_localities`.
#' @param categories string (one item) or vector of strings (one or more items).
#' @param age_min integer; minimum age of assemblage.
#' @param age_max integer; maximum age of assemblage.
#' @param assemblages list of assemblages; return value from function `road_get_assemblages`.
#' @param feature_interpretation string (one item) or vector of strings
#' 
#' @return Database search result as list of feature finds.
#' @export
#'
#' @examples road_get_feature(continents = "Europe")
#' @examples road_get_feature(continents = "Europe", )
#' @examples road_get_feature(continents = "Europe", )
road_get_feature <- function(continents = NULL, subcontinents = NULL, countries = NULL, 
                                        locality_types = NULL, cultural_periods = NULL, categories = NULL, 
                                        age_min = NULL, age_max = NULL, 
                                        feature_interpretation = NULL,
                                        assemblages = NULL, localities = NULL)
{
  # calculate assemblage_condition
  # To do: !is.null(categories) AND !is.null(assemblages)  ---> Warnung an den Benutzer
  if (is.null(assemblages)) assemblages <- road_get_assemblages(continents = continents,
                                                                subcontinents = subcontinents,
                                                                countries = countries,
                                                                categories = categories, 
                                                                locality_types = locality_types,
                                                                cultural_periods = cultural_periods,
                                                                age_min = age_min, age_max = age_max, 
                                                                localities = localities)
  assemblage_condition <- get_assemblage_condition(query_start = "AND ", assemblages = assemblages)
  # calculate output extention
  assemblage_info_for_output <- get_output_extention_assemblage(assemblages = assemblages)
  
  # select fields
  select_fields <- c(
    paste0(" assemblage_idlocality AS ", cm_locality_idlocality),
    paste0(" assemblage_idassemblage AS ", cm_assemblages_idassemblage),
    paste0(" interpretation AS ", cm_feature_interpretation),
    "comments"
  )
  
  if (!is.null(feature_interpretation)) 
    feature_interpretation_condition <- query_check_intersection("AND ", 
                                                                 feature_interpretation, 
                                                                 cm_feature_interpretation)
  else 
    feature_interpretation_condition <- ""
  
  query <- paste(
    "SELECT DISTINCT * FROM (",
    "SELECT DISTINCT ",
    paste(select_fields, collapse = ", "),
    " FROM feature) as foo WHERE true ",
    assemblage_condition,
    feature_interpretation_condition
  )
  
  # message(query)
  data <- road_run_query(query)
  
  return(merge(x = data, y = assemblage_info_for_output, 
               by = c(cm_locality_idlocality, cm_assemblages_idassemblage)))
}


#' Get miscellaneous finds from ROAD database
#'
#' `road_get_miscellaneous_finds` fetches data of feature finds from ROAD database.
#'
#'
#' @param continents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param subcontinents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param countries string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param locality_types string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param cultural_periods string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param localities list of localities; return value from function `road_get_localities`.
#' @param categories string (one item) or vector of strings (one or more items).
#' @param age_min integer; minimum age of assemblage.
#' @param age_max integer; maximum age of assemblage.
#' @param assemblages list of assemblages; return value from function `road_get_assemblages`.
#' @param miscellaneous_finds_material string (one item) or vector of strings
#' 
#' @return Database search result as list of miscellaneous finds.
#' @export
#'
#' @examples road_get_miscellaneous_finds(continents = "Europe")
#' @examples road_get_miscellaneous_finds(continents = "Europe", )
#' @examples road_get_miscellaneous_finds(continents = "Europe", )
road_get_miscellaneous_finds <- function(continents = NULL, subcontinents = NULL, countries = NULL, 
                             locality_types = NULL, cultural_periods = NULL, categories = NULL, 
                             age_min = NULL, age_max = NULL, 
                             miscellaneous_finds_material = NULL,
                             assemblages = NULL, localities = NULL)
{
  # calculate assemblage_condition
  # To do: !is.null(categories) AND !is.null(assemblages)  ---> Warnung an den Benutzer
  if (is.null(assemblages)) assemblages <- road_get_assemblages(continents = continents,
                                                                subcontinents = subcontinents,
                                                                countries = countries,
                                                                categories = categories, 
                                                                locality_types = locality_types,
                                                                cultural_periods = cultural_periods,
                                                                age_min = age_min, age_max = age_max, 
                                                                localities = localities)
  assemblage_condition <- get_assemblage_condition(query_start = "AND ", assemblages = assemblages)
  # calculate output extention
  assemblage_info_for_output <- get_output_extention_assemblage(assemblages = assemblages)
  
  # select fields
  select_fields <- c(
    paste0(" assemblage_idlocality AS ", cm_locality_idlocality),
    paste0(" assemblage_idassemblage AS ", cm_assemblages_idassemblage),
    paste0(" material AS ", cm_miscellaneous_finds_material),
    paste0(" raw_material_source AS ", cm_miscellaneous_finds_raw_material_source),
    "number",
    "comments"
  )
  
  if (!is.null(miscellaneous_finds_material)) 
    miscellaneous_finds_material_condition <- query_check_intersection("AND ", 
                                                                       miscellaneous_finds_material, 
                                                                       cm_miscellaneous_finds_material)
  else 
    miscellaneous_finds_material_condition <- ""
  
  query <- paste(
    "SELECT DISTINCT * FROM (",
    "SELECT DISTINCT ",
    paste(select_fields, collapse = ", "),
    " FROM miscellaneous_finds) as foo WHERE true ",
    assemblage_condition,
    miscellaneous_finds_material_condition
  )
  
  # message(query)
  data <- road_run_query(query)
  
  return(merge(x = data, y = assemblage_info_for_output, 
               by = c(cm_locality_idlocality, cm_assemblages_idassemblage)))
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
  
  #message(query)
  
  data <- road_run_query(query)
  colnames(data)
  
  return(data)
}

#' Get paleofauna finds from ROAD database
#'
#' `road_get_paleofauna` fetches data of paleofauna finds from ROAD database.
#'
#' Paleofauna finds are often part of an assemblage which means the function needs a list of
#' assemblages (return value of function `road_get_assemblages`) as its parameter.
#'
#' @param continents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param subcontinents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param countries string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param locality_types string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param cultural_periods string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param localities list of localities; return value from function `road_get_localities`.
#' @param categories string (one item) or vector of strings (one or more items).
#' @param age_min integer; minimum age of assemblage.
#' @param age_max integer; maximum age of assemblage.
#' @param assemblages list of assemblages; return value from function `road_get_assemblages`.
#' 
#' @return Database search result as list of archaeological finds.
#' @export
#'
#' @examples road_get_archaeology(continents = "Europe")
#' @examples road_get_archaeology(continents = "Europe", archaeological_category = "feature")
#' @examples road_get_archaeology(continents = "Europe", archaeological_category = c("feature", "symbolic artefacts"))
road_get_paleofauna <- function(continents = NULL, subcontinents = NULL, countries = NULL, 
                                 locality_types = NULL, cultural_periods = NULL, 
                                 categories = NULL, age_min = NULL, age_max = NULL, 
                                 assemblages = NULL, localities = NULL)
{
  # calculate locality_condition
  # To do: !is.null(one of localities parameters) AND !is.null(localities)  ---> Warnung an den Benutzer
  if (is.null(localities)) localities <- road_get_localities(continents = continents, 
                                                             subcontinents = subcontinents, 
                                                             countries = countries, 
                                                             locality_types = locality_types, 
                                                             cultural_periods = cultural_periods)
  # locality_condition <- get_locality_condition(localities = localities)
  query_localities <- paste(
    sapply(localities$locality_id, function(x) paste0("'", x, "'")),
    collapse = ", "
  )
  # calculate output extention
  locality_info_for_output <- get_output_extention_locality(localities=localities)
  
  # calculate assemblage_condition
  # To do: !is.null(categories) AND !is.null(assemblages)  ---> Warnung an den Benutzer
  if (is.null(assemblages)) assemblages <- road_get_assemblages(categories = categories, 
                                                                age_min = age_min, age_max = age_max, 
                                                                localities = localities)
  assemblage_condition <- get_assemblage_condition(query_start = "AND ", assemblages = assemblages)
  # calculate output extention
  assemblage_info_for_output <- get_output_extention_assemblage(assemblages)
  
  # select fields
  select_fields <- c(
    paste0(" assemblage_idlocality AS ", cm_locality_idlocality),
    paste0(" assemblage_idassemblage AS ", cm_assemblages_idassemblage)
    # paste0(" archaeological_category AS ", cm_archaeological_category)
  )
  
  # combine query parts
  query <- paste(
    "SELECT DISTINCT * FROM ( SELECT ",
    paste(select_fields, collapse = ", "),
    " FROM paleofauna",
    ") as foo WHERE ", cm_locality_idlocality," IN (", query_localities, ")",
    assemblage_condition,
    # genus_species_condition,
    "ORDER BY ", cm_locality_idlocality, ", ", cm_assemblages_idassemblage 
  )
  
  # message(query)
  
  data <- road_run_query(query)
  
  # data$genus[data$genus == ""] <- NA
  
  data_plus_assemblage_info <- merge(x = data, y = assemblage_info_for_output, 
                                     by = c(cm_locality_idlocality, cm_assemblages_idassemblage))
  
  return(merge(x = data_plus_assemblage_info, y = locality_info_for_output, by = cm_locality_idlocality))
  
}