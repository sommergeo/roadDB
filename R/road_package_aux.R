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
  assemblage_condition <- get_assemblage_condition(assemblages = assemblages)
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


#' Get archaeology from ROAD database
#'
#' `road_get_archaeology` fetches data of archaeological finds from ROAD database.
#'
#' Archaeological finds are often part of an assemblage which means the function needs a list of
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
#' @param archaeological_category string (one item) or vector of strings ('lithics', 'organic tools', 'symbolic artefacts', 'miscellaneous finds', 'feature');
#'
#' @param tool_list string (one item) or vector of strings
#' @param raw_material_list string (one item) or vector of strings
#' @param organic_tools_interpretation string (one item) or vector of strings
#' @param symbolic_artefacts_interpretation string (one item) or vector of strings
#' @param feature_interpretation string (one item) or vector of strings
#' @param miscellaneous_finds_material string (one item) or vector of strings
#' 
#' @return Database search result as list of archaeological finds.
#' @export
#'
#' @examples road_get_archaeology(continents = "Europe")
#' @examples road_get_archaeology(continents = "Europe", archaeological_category = "feature")
#' @examples road_get_archaeology(continents = "Europe", archaeological_category = c("feature", "symbolic artefacts"))
road_get_archaeology <- function(continents = NULL, subcontinents = NULL, countries = NULL, 
                                   locality_types = NULL, cultural_periods = NULL, 
                                   categories = NULL, age_min = NULL, age_max = NULL, 
                                   archaeological_category = NULL, tool_list = NULL,
                                   raw_material_list = NULL, organic_tools_interpretation = NULL,
                                   feature_interpretation = NULL, miscellaneous_finds_material = NULL, 
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
  assemblage_condition <- get_assemblage_condition(assemblages = assemblages)
  # calculate output extention
  assemblage_info_for_output <- get_output_extention_assemblage(assemblages = assemblages)
  
  run_query <- FALSE
  if (!is.null(archaeological_category) || !is.null(tool_list) || !is.null(raw_material_list)
      || !is.null(organic_tools_interpretation) || !is.null(feature_interpretation) 
      || !is.null(miscellaneous_finds_material)) 
    run_query <- TRUE
   
  # select fields
  select_fields <- c(
    paste0(" assemblage_idlocality AS ", cm_locality_idlocality),
    paste0(" assemblage_idassemblage AS ", cm_assemblages_idassemblage)
    # paste0(" archaeological_category AS ", cm_archaeological_category)
  )
  
  fromPart_dummy <- paste("SELECT ", paste(select_fields, collapse = ", "),
                          ", 'dummy' as ", cm_archaeological_category, " FROM typology WHERE false")
 
  if (is.null(archaeological_category) || !is.vector(archaeological_category) && archaeological_category == 'lithics' ||
      is.element('lithics', archaeological_category))
                                    fromPart_lithics <- paste(" UNION ",
                                                 " SELECT DISTINCT ", paste(select_fields, collapse = ", "), ", 
                                                          'typology' as ", cm_archaeological_category, " FROM typology ",
                                                 " UNION ",
                                                 "SELECT DISTINCT ", paste(select_fields, collapse = ", "), ", 
                                                          'technology' as ", cm_archaeological_category, " FROM technology ",
                                                 " UNION ",
                                                 "SELECT DISTINCT ", paste(select_fields, collapse = ", "), ", 
                                                          'function' as ", cm_archaeological_category, " FROM function ",
                                                 " UNION ",
                                                 "SELECT DISTINCT ", paste(select_fields, collapse = ", "), ", 
                                                          'raw material' as ", cm_archaeological_category, " FROM raw_material ")
  else fromPart_lithics <- ""
  
  if (is.null(archaeological_category) || !is.vector(archaeological_category) && archaeological_category == 'organic tools' ||
      is.element('organic tools', archaeological_category))
                                     fromPart_organic_tools <- paste(" UNION ", 
                                                  "SELECT DISTINCT ", paste(select_fields, collapse = ", "),
                                                  ", 'organic tools' as ", cm_archaeological_category, " FROM organic_tools")
  else fromPart_organic_tools <- ""
  
  if (is.null(archaeological_category) || !is.vector(archaeological_category) && archaeological_category == 'symbolic artefacts' ||
      is.element('symbolic artefacts', archaeological_category))
                                    fromPart_symbolic_artifacts <- paste(" UNION ", 
                                                  "SELECT DISTINCT ", paste(select_fields, collapse = ", "),
                                                  ", 'symbolic_artifacts' as ", cm_archaeological_category, " FROM symbolic_artifacts")
  else fromPart_symbolic_artifacts <- ""
  
  if (is.null(archaeological_category) || !is.vector(archaeological_category) && archaeological_category == 'miscellaneous finds' ||
      is.element('miscellaneous finds', archaeological_category))
                                    fromPart_miscellaneous_finds <- paste(" UNION ", 
                                                  "SELECT DISTINCT ", paste(select_fields, collapse = ", "),
                                                  ", 'miscellaneous finds' as ", cm_archaeological_category, " FROM miscellaneous_finds")
  else fromPart_miscellaneous_finds <- ""
  
  if (is.null(archaeological_category) || !is.vector(archaeological_category) && archaeological_category == 'feature' ||
      is.element('feature', archaeological_category))
                                    fromPart_feature <- paste(" UNION ", 
                                                  "SELECT DISTINCT ", paste(select_fields, collapse = ", "),
                                                  ", 'feature' as ", cm_archaeological_category, " FROM feature")
  else fromPart_feature <- ""

  # combine query parts
  if (!is.null(archaeological_category))
  {
    query <- paste(
      "SELECT DISTINCT * FROM (",
      fromPart_dummy,
      fromPart_lithics,
      fromPart_organic_tools,
      fromPart_symbolic_artifacts,
      fromPart_feature,
      ") as foo WHERE true ",  # cm_locality_idlocality," IN (", query_localities, ")",
      assemblage_condition,
      # genus_species_condition,
      "ORDER BY ", cm_locality_idlocality, ", ", cm_assemblages_idassemblage 
    )
  }

  # select fields
  select_fields <- c(
    paste0(" assemblage_idlocality AS ", cm_locality_idlocality),
    paste0(" assemblage_idassemblage AS ", cm_assemblages_idassemblage)
    # paste0(" tool_list ") # AS ", cm_archaeological_category)
  )
  
  if (!is.null(tool_list))
  {
    query <- paste(
      "SELECT DISTINCT * FROM (",
      "SELECT DISTINCT ",
      paste(select_fields, collapse = ", "),
      ", tool_list as ", cm_tool_list,
      " FROM typology) as foo WHERE true ",
      assemblage_condition,
      query_check_intersection("AND ", tool_list, cm_tool_list)
    )
  }
  
  if (!is.null(raw_material_list))
  {
    query <- paste(
      "SELECT DISTINCT * FROM (",
      "SELECT DISTINCT ",
      paste(select_fields, collapse = ", "),
      ", raw_material_list as ", cm_raw_material_list,
      " FROM raw_material ) as foo WHERE true ",
      assemblage_condition,
      query_check_intersection("AND ", raw_material_list, cm_raw_material_list)
    )
  }
  
  if (!is.null(organic_tools_interpretation))
  {
    query <- paste(
      "SELECT DISTINCT * FROM (",
      "SELECT DISTINCT ",
      paste(select_fields, collapse = ", "),
      ", interpretation as ", cm_organic_tools_interpretation,
      " FROM organic_tools ) as foo WHERE true ",
      assemblage_condition,
      query_check_intersection("AND ", organic_tools_interpretation, cm_organic_tools_interpretation)
    )
  }
  
  if (!is.null(feature_interpretation))
  {
    query <- paste(
      "SELECT DISTINCT * FROM (",
      "SELECT DISTINCT ",
      paste(select_fields, collapse = ", "),
      ", interpretation as ", cm_feature_interpretation,
      " FROM feature ) as foo WHERE true ",
      assemblage_condition,
      query_check_intersection("AND ", feature_interpretation, cm_feature_interpretation)
    )
  }
  
  if (!is.null(miscellaneous_finds_material))
  {
    query <- paste(
      "SELECT DISTINCT * FROM (",
      "SELECT DISTINCT ",
      paste(select_fields, collapse = ", "),
      ", material as ", cm_miscellaneous_finds_material,
      " FROM miscellaneous_finds ) as foo WHERE true ",
      assemblage_condition,
      query_check_intersection("AND ", miscellaneous_finds_material, cm_miscellaneous_finds_material)
    )
  }
  
  if (run_query)
  {
    # message(query)
    data <- road_run_query(query)
  
    # data$genus[data$genus == ""] <- NA

    data_plus_assemblage_info <- merge(x = data, y = assemblage_info_for_output, 
                                     by = c(cm_locality_idlocality, cm_assemblages_idassemblage))
  
    # return(merge(x = data_plus_assemblage_info, y = locality_info_for_output, by = cm_locality_idlocality))
  
    return(data_plus_assemblage_info)
  }
  else return(assemblages)
  
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
  assemblage_condition <- get_assemblage_condition(assemblages = assemblages)
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