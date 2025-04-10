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
