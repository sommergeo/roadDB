library(assertthat)
library(RPostgres)

attributes <- c("type", "continent", "continent_region", "country", "category", 
              "cultural_period", "example", "dating_method", "material_dated", "technocomplex")
tables <- list("locality", "geopolitical_units", "geopolitical_units", "locality",  
            "assemblage", "archaeological_stratigraphy", "roadDB/attr_values/ex.txt", 
            c("geological_layer_age", "archaeological_layer_age", "assemblage_age"), 
            c("geological_layer_age", "archaeological_layer_age", "assemblage_age"),
            "archaeological_stratigraphy")

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
  # iterating over elements of attributes
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
  
  # if we use tables <- list(...), all elements of the list are vectors
  #if (is.vector(table)) {
  q <- paste( "SELECT DISTINCT(unnest(string_to_array(string_agg(", attribute_name, ", ', '),', '))) as ",
              attribute_name, " from ")
  qu <- paste0(q, table)
  que <- paste(
    sapply(table, function(x) paste0(q, x)), 
    collapse = " UNION "
  )
  query <- paste0(que, " ORDER BY ", attribute_name)
  # }
  # else
  #  query <- paste( "SELECT DISTINCT(unnest(string_to_array(string_agg(", attribute_name, ", ', '),', '))) as ",
  #                 attribute_name, " from ", table, " ORDER BY ", attribute_name)

  
  # query <- paste( "SELECT DISTINCT ", attribute_name, " FROM (select distinct(unnest(string_to_array(
  # string_agg(", attribute_name, ", ', '),', '))) as ",
  # attribute_name, ", 'dummy' as dummy from ", table,  " GROUP BY dummy) as foo ", 
  # " ORDER BY ", attribute_name)
  
  data <- road_run_query(query)
  
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
tt#'
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
                            categories = NULL, localities = NULL, assemblages = NULL)
{
  if ((!is.null(age_min) && !is.integer(age_min)) || (!is.null(age_max) && !is.integer(age_max)))
    stop("Parameters 'min_age' and 'max_age' have to be integers.")
  
  if (!is.null(age_min) && !is.null(age_max) && age_min > age_max)
    stop("Parameter 'min_age' can not be bigger than 'max_age'.")

  if (is.null(localities))
  {
    # run `road_get_localities` else preselected list of localities is used
    localities <- road_get_localities(continents, subcontinents, countries, locality_types, cultural_periods)
  }
  localities <- localities[cm_locality_idlocality]

  query_localities <- paste(
    sapply(localities, function(x) paste0("'", x, "'")),
    collapse = ", "
  )
  
  if (is.null(assemblages))
  {
    assemblages <- road_get_assemblages(categories = categories)
  }  
  
  cols <- c("locality_id", "assemblage_id")
  # query_locality_assemblage_list 
  assemblages$locality_assemblage_list <- paste(assemblages$locality_id, assemblages$assemblage_id, sep = ", ")
    # apply(assemblages[ , cols ] , 1, paste , collapse = "," )
    # sapply(assemblages["locality_id"], function(x) paste0("'", x, "'")),
    #assemblages["locality_id"],
    # assemblages["assemblage_id"]
    #sep = "--"
  #)
  query_locality_assemblage_list <- paste(
    sapply(assemblages$locality_assemblage_list, function(x) paste0("'", x, "'")),
    collapse = ", "
  )
  message(query_locality_assemblage_list)
  # select fields
  select_fields_gla <- c(
    paste0("geological_layer_age.geolayer_idlocality AS  \"", cm_locality_idlocality, "\""),
    paste0("CAST(assemblage_idassemblage AS TEXT) AS  \"", cm_assemblages_idassemblage, "\""),
    paste0("geological_layer_age.geolayer_name AS \"", cm_geolayer_geolayer_name, "\""),
    paste0("archlayer_name AS \"", cm_archlayer_archlayer_name, "\""),
    paste0("age AS \"", cm_age, "\""),
    paste0("negative_standard_deviation AS \"", cm_negative_standard_deviation, "\""),
    paste0("positive_standard_deviation AS \"", cm_positive_standard_deviation, "\""),
    paste0("material_dated AS \"", cm_material_dated, "\""),
    paste0("dating_method AS \"", cm_dating_method, "\""),
    paste0("laboratory_idlaboratory AS \"", cm_laboratory_idlaboratory, "\""),
    paste0("technocomplex as technocomplex")
  )
  
  select_fields_ala <- c(
    paste0("archaeological_layer_age.archlayer_idlocality AS \"", cm_locality_idlocality, "\""),
    paste0("CAST(assemblage_idassemblage AS TEXT) AS \"", cm_assemblages_idassemblage, "\""),
    paste0("archlayer_correl_geolayer.geolayer_name AS \"", cm_geolayer_geolayer_name, "\""),
    paste0("archaeological_layer_age.archlayer_name AS \"", cm_archlayer_archlayer_name, "\""),
    paste0("age AS \"", cm_age, "\""),
    paste0("negative_standard_deviation AS \"", cm_negative_standard_deviation, "\""),
    paste0("positive_standard_deviation AS \"", cm_positive_standard_deviation, "\""),
    paste0("material_dated AS \"", cm_material_dated, "\""),
    paste0("dating_method AS \"", cm_dating_method, "\""),
    paste0("laboratory_idlaboratory AS \"", cm_laboratory_idlaboratory, "\""),
    paste0("technocomplex as technocomplex")
  )
  
  select_fields_asa <- c(
    paste0("assemblage_age.assemblage_idlocality AS \"", cm_locality_idlocality, "\""),
    paste0("CAST(assemblage_age.assemblage_idassemblage AS TEXT) AS  \"", cm_assemblages_idassemblage, "\""),
    paste0("assemblage_in_geolayer.geolayer_name AS \"", cm_geolayer_geolayer_name, "\""),
    paste0("archlayer_name AS \"", cm_archlayer_archlayer_name, "\""),
    paste0("age AS \"", cm_age, "\""),
    paste0("negative_standard_deviation AS \"", cm_negative_standard_deviation, "\""),
    paste0("positive_standard_deviation AS \"", cm_positive_standard_deviation, "\""),
    paste0("material_dated AS \"", cm_material_dated, "\""),
    paste0("dating_method AS \"", cm_dating_method, "\""),
    paste0("laboratory_idlaboratory AS \"", cm_laboratory_idlaboratory, "\""),
    paste0("technocomplex as technocomplex")
  )
  
  # 
  # queAux <- paste0("SELECT locality_idlocality, name 
  #             FROM  archaeological_layer, archaeological_stratigraphy 
  #             WHERE archstratigraphy_idarchstrat = idarchstrat",
  #             query_check_intersection("AND", technocomplex, "technocomplex"))
  # 
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
            query_check_intersection("AND ", dating_methods, "dating_method"),
            query_check_intersection("AND ", material_dated, "material_dated"),
            parameter_to_query("AND age  <= ", age_max, " "),
            parameter_to_query("AND age  >= ", age_min, " "),
            query_check_intersection("AND ", technocomplex, "technocomplex"),
            " ORDER BY ", cm_locality_idlocality, ", ", cm_geolayer_geolayer_name, 
            ", ", cm_archlayer_archlayer_name)
  
  data <- road_run_query(query)
  
  return(data)
  
}