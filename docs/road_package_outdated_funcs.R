# Get human remains from ROAD database
#
# `road_get_human_remains` fetches data of human remains from ROAD database.
#
# Human remains are always part of an assemblage which means the function needs a list of
# assemblages (return value of function `road_get_assemblages`) as its first parameter.
# The parameter `genus_species` can't be used in combination with `genus' or `species`. Use this function
# in one of the two modes depending on which parameters you use:
# Mode 1: either one or both of `genus` and `species` is used (not NULL), then `genus_species` can't be used and has to be set to NULL.
# Mode 2: `genus_species` is used (not NULL), then `genus` and `species` can't be used and have to be set to NULL.
#
# @param continents string (one item) or vector of strings (one or more items); defaults to NULL.
# @param subcontinents string (one item) or vector of strings (one or more items); defaults to NULL.
# @param countries string (one item) or vector of strings (one or more items); defaults to NULL.
# @param locality_types string (one item) or vector of strings (one or more items); defaults to NULL.
# @param cultural_periods string (one item) or vector of strings (one or more items); defaults to NULL.
# @param categories string (one item) or vector of strings (one or more items).
# @param age_min integer; minimum age of assemblage.
# @param age_max integer; maximum age of assemblage.
# @param assemblages list of assemblages; return value from function `road_get_assemblages`.
# @param genus string (one item) or vector of strings (one or more items); can not be used in combination with `genus_species`.
# @param species string (one item) or vector of strings (one or more items); can not be used in combination with `genus_species`.
# @param genus_species string (one item) or vector of strings (one or more items); can not be used in combination with `genus` or `species`.
# 
# @return Database search result as list of human remains.
#
# @examples road_get_human_remains(genus = 'Homo', species = 'neanderthalensis')
# @examples road_get_human_remains(genus = 'Homo')
# @examples road_get_human_remains(species = 'neanderthalensis')
# @examples road_get_human_remains(genus_species = 'Homo neanderthalensis')
road_get_human_remains <- function(
    continents = NULL, 
    subcontinents = NULL, 
    countries = NULL, 
    locality_types = NULL, 
    cultural_periods = NULL, 
    categories = NULL, 
    age_min = NULL, 
    age_max = NULL, 
    genus = NULL, 
    species = NULL, 
    genus_species = NULL, 
    assemblages = NULL
)
{
  # calculate assemblage_condition
  # To do: !is.null(categories) AND !is.null(assemblages)  ---> Warnung an den Benutzer
  if (is.null(assemblages)) assemblages <- road_get_assemblages(continents = continents, 
                                                                subcontinents = subcontinents, 
                                                                countries = countries, 
                                                                locality_types = locality_types, 
                                                                cultural_periods = cultural_periods,
                                                                categories = categories, 
                                                                age_min = age_min, 
                                                                age_max = age_max)
  assemblage_condition <- get_assemblage_condition(query_start = "AND ", assemblages = assemblages)
  
  if (!is.null(genus_species) && (!is.null(genus) || !is.null(species)))
    stop("Parameter 'genus_species' can't be used in combination with 'genus' or 'species'.")
  
  # build genus/species selection
  genus_species_condition = ""
  if (!is.null(genus_species))
  {
    genus_species_condition <- parameter_to_query("AND genus_species_str IN (", genus_species, ")")
  }
  else
  {
    species_conjucton <- "AND"
    if (!is.null(genus))
    {
      genus_species_condition <- parameter_to_query("AND ( genus IN (", genus, ")")
      species_conjucton <- "OR"
    }
    else
      if (!is.null(species)) 
      {
        genus_species_condition <- paste(
          genus_species_condition,
          species_conjucton,
          parameter_to_query("species IN (", species, ")")
        )
      }
    if (!is.null(genus)) genus_species_condition <- paste(genus_species_condition, ")")
  }
  
  # select fields
  select_fields <- c(
    paste0("humanremains_idlocality AS ", cm_locality_idlocality),
    paste0("humanremains_idassemblage AS ", cm_assemblages_idassemblage),
    paste0("genus || ' ' || species AS ", cm_humanremains_genus_species_str),
    paste0("genus AS ", cm_humanremains_genus),
    paste0("species AS ", cm_humanremains_species),
    paste0("age AS ", cm_humanremains_age),
    paste0("sex AS ", cm_humanremains_sex),
    paste0("humanremains_idhumanremains AS ", cm_humanremains_idhumanremains)
  )
  
  # combine query parts
  query <- paste(
    "SELECT DISTINCT * FROM ( SELECT ",
    paste(select_fields, collapse = ", "), 
    " FROM publication_desc_humanremains) as foo  
    WHERE TRUE ",
    assemblage_condition,
    genus_species_condition,
    "ORDER BY ", cm_locality_idlocality, ", ", cm_assemblages_idassemblage 
  )
  
  data <- road_run_query(query)
  
  data <- add_locality_columns(data, assemblages = assemblages)
  
  return(data)
}


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

# Get archaeology from ROAD database
#
# `road_get_archaeology_` fetches data of archaeological finds from ROAD database.
#
# Archaeological finds are often part of an assemblage which means the function needs a list of
# assemblages (return value of function `road_get_assemblages`) as its parameter.
#
# @param continents string (one item) or vector of strings (one or more items); defaults to NULL.
# @param subcontinents string (one item) or vector of strings (one or more items); defaults to NULL.
# @param countries string (one item) or vector of strings (one or more items); defaults to NULL.
# @param locality_types string (one item) or vector of strings (one or more items); defaults to NULL.
# @param cultural_periods string (one item) or vector of strings (one or more items); defaults to NULL.
# @param localities list of localities; return value from function `road_get_localities`.
# @param categories string (one item) or vector of strings (one or more items).
# @param age_min integer; minimum age of assemblage.
# @param age_max integer; maximum age of assemblage.
# @param assemblages list of assemblages; return value from function `road_get_assemblages`.
# 
# @param archaeological_category string (one item) or 
#        vector of strings ('lithics', 'organic tools', 'symbolic artefacts', 'miscellaneous finds', 'feature')
# @param tool_list string (one item) or vector of strings
# @param raw_material_list string (one item) or vector of strings
# @param organic_tools_interpretation string (one item) or vector of strings
# @param symbolic_artefacts_interpretation string (one item) or vector of strings
# @param feature_interpretation string (one item) or vector of strings
# @param miscellaneous_finds_material string (one item) or vector of strings
# 
# @return Database search result as list of archaeological finds.
#
# @examples road_get_archaeology(continents = "Europe")
# @examples road_get_archaeology(continents = "Europe", archaeological_category = "feature")
# @examples road_get_archaeology(continents = "Europe", archaeological_category = c("feature", "symbolic artefacts"))
road_get_archaeology_ <- function(continents = NULL, subcontinents = NULL, countries = NULL, 
                                 locality_types = NULL, cultural_periods = NULL, 
                                 categories = NULL, age_min = NULL, age_max = NULL, 
                                 archaeological_category = NULL, tool_list = NULL,
                                 raw_material_list = NULL, organic_tools_interpretation = NULL,
                                 feature_interpretation = NULL, miscellaneous_finds_material = NULL, 
                                 assemblages = NULL, localities = NULL)
{
  # calculate locality_condition
  # To do: !is.null(one of localities parameters) AND !is.null(localities)  ---> Warnung an den Benutzer
  ###if (is.null(localities)) localities <- road_get_localities(continents = continents, 
  ###                                                        subcontinents = subcontinents, 
  ###                                                         countries = countries, 
  ###                                                         locality_types = locality_types, 
  ###                                                        cultural_periods = cultural_periods)
  # locality_condition <- get_locality_condition(localities = localities)
  ###query_localities <- paste(
  ###  sapply(localities$locality_id, function(x) paste0("'", x, "'")),
  ###  collapse = ", "
  ###)
  # calculate output extention
  ###locality_info_for_output <- get_output_extention_locality(localities=localities)
  
  # calculate assemblage_condition
  # To do: !is.null(categories) AND !is.null(assemblages)  ---> Warnung an den Benutzer
  if (is.null(assemblages)) assemblages <- road_get_assemblages(continents = continents,
                                                                subcontinents = subcontinents,
                                                                countries = countries,
                                                                categories = categories, 
                                                                locality_types = locality_types,
                                                                cultural_periods = cultural_periods,
                                                                age_min = age_min, age_max = age_max) 
                                                                #localities = localities)
  assemblage_condition <- get_assemblage_condition(" AND ", assemblages = assemblages)
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

# Get paleofauna finds from ROAD database
#
# `road_get_paleofauna_` fetches data of paleofauna finds from ROAD database.
#
# Paleofauna finds are often part of an assemblage which means the function needs a list of
# assemblages (return value of function `road_get_assemblages`) as its parameter.
#
# @param continents string (one item) or vector of strings (one or more items); defaults to NULL.
# @param subcontinents string (one item) or vector of strings (one or more items); defaults to NULL.
# @param countries string (one item) or vector of strings (one or more items); defaults to NULL.
# @param locality_types string (one item) or vector of strings (one or more items); defaults to NULL.
# @param cultural_periods string (one item) or vector of strings (one or more items); defaults to NULL.
# @param categories string (one item) or vector of strings (one or more items).
# @param age_min integer; minimum age of assemblage.
# @param age_max integer; maximum age of assemblage.
# @param fauna_genus string (one item) or vector of strings (one or more items); defaults to NULL.
# @param fauna_species string (one item) or vector of strings (one or more items); defaults to NULL.
# @param assemblages list of assemblages; return value from function `road_get_assemblages`.
# @param localities list of localities; return value from function `road_get_localities`.
# 
# @return Database search result as list of archaeological finds.
#
# @examples road_get_paleofauna(continents = "Europe")
# @examples road_get_paleofauna(continents = "Europe", archaeological_category = "feature")
# @examples road_get_paleofauna(continents = "Europe", archaeological_category = c("feature", "symbolic artefacts"))
road_get_paleofauna_ <- function(
    continents = NULL, 
    subcontinents = NULL, 
    countries = NULL, 
    locality_types = NULL,
    cultural_periods = NULL, 
    categories = NULL, 
    age_min = NULL, 
    age_max = NULL,
    fauna_genus = NULL,
    fauna_species = NULL,
    assemblages = NULL,
    localities = NULL
) {
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
                                                                age_min = age_min, age_max = age_max)
  #localities = localities)
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
  
  data <- add_locality_columns(data, assemblages = assemblages)
  
  return(data)
  
}

get_output_extention_locality <- function(localities = NULL)
{
  if (is.null(localities)) return(NULL)
  
  locality_info_for_output <- list()
  locality_info_for_output$locality_id <- localities$locality_id
  locality_info_for_output$continent <- localities$continent
  locality_info_for_output$subcontinent <- localities$subcontinent
  locality_info_for_output$country <- localities$country
  locality_info_for_output$locality_types <- localities$locality_types
  locality_info_for_output$coord_x <- localities$coord_x
  locality_info_for_output$coord_y <- localities$coord_y
  locality_info_for_output$cultural_period <- localities$cultural_period
  
  return(locality_info_for_output)
}


get_output_extention_assemblage <- function(assemblages = NULL)
{
  if (is.null(assemblages)) return(NULL)
  
  assemblage_info_for_output <- list()
  
  assemblage_info_for_output$locality_id <- assemblages$locality_id
  assemblage_info_for_output$assemblage_id <- assemblages$assemblage_id
  assemblage_info_for_output$categories <- assemblages$categories
  assemblage_info_for_output$age_min <- assemblages$age_min
  assemblage_info_for_output$age_max <- assemblages$age_max
  assemblage_info_for_output$continent <- assemblages$continent.y
  assemblage_info_for_output$subcontinent <- assemblages$subcontinent.y
  assemblage_info_for_output$country <- assemblages$country.y
  assemblage_info_for_output$locality_types <- assemblages$locality_types.y
  assemblage_info_for_output$cultural_period <- assemblages$cultural_period
  assemblage_info_for_output$coord_x <- assemblages$coord_x.y
  assemblage_info_for_output$coord_y <- assemblages$coord_y.y
  
  return(assemblage_info_for_output)
}
