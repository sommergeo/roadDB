source("./R/login.R")

# column names
cm_locality_idlocality <- "locality_id"
cm_locality_types <- "locality_types"
cm_geopolitical_units_continent <- "continent"
cm_geopolitical_units_continent_region <- "subcontinent"
cm_locality_country <- "country"
cm_locality_x <- "coord_x"
cm_locality_y <- "coord_y"
cm_cultural_periods <- "cultural_periods"
cm_technocomplexes <- "technocomplexes"
cm_assemblages_locality_idlocality <- "locality_id"
cm_assemblages_idassemblage <- "assemblage_id"
cm_assemblages_name <- "assemblage_name"
cm_assemblages_categories <- "categories"
cm_geological_stratigraphy_age_min <- "age_min"
cm_geological_stratigraphy_age_max <- "age_max"

cm_assemblage_in_geolayer_geolayer_name <- "geolayers"
cm_geolayer_geolayer_name <- "geolayer"
cm_assemblage_in_archlayer_archlayer_name <- "archlayers"
cm_archlayer_archlayer_name <- "archlayer"
cm_age <- "age"
cm_negative_standard_deviation <- "negative_standard_deviation"
cm_positive_standard_deviation <- "positive_standard_deviation"
cm_material_dated <- "material_dated"
cm_dating_method <- "dating_method"
cm_laboratory_idlaboratory <- "laboratory_idlaboratory"
#cm_humanremains_genus_species_str <- "genus_species_str"
cm_humanremains_genus <- "genus"
cm_humanremains_species <- "species"
cm_humanremains_age <- "age"
cm_humanremains_sex <- "sex"
cm_humanremains_idhumanremains <- "humanremains_id"
cm_archaeological_category <- "archaeological_category"
cm_paleoflora_plant_remains <- "plant_remains"
cm_plant_taxonomy_family <- "plant_family"
cm_plant_taxonomy_genus <- "plant_genus"
cm_plant_taxonomy_species <- "plant_species"
cm_fauna_genus <- "fauna_genus"
cm_fauna_species <- "fauna_species"
cm_tool_list <- "tool_list"
cm_typology <- "typology"
cm_raw_material_list <- "raw_material_list"
cm_transport_distance <- "transport_distance"
cm_organic_tools_interpretation <- "organic_tools_interpretation"
cm_feature_interpretation <- "feature_interpretation"
cm_miscellaneous_finds_material <- "miscellaneous_finds_material"
cm_organic_tools_interpretation <- "organic_tools_interpretation"
cm_organic_raw_material <- "organic_raw_material"
cm_organic_tools_technology <- "organic_tools_technology"
cm_symbolic_artifacts_interpretation <- "symbolic_artifacts_interpretation"
cm_symbolic_artifacts_category <- "symbolic_artifacts_category"
cm_symbolic_artifacts_material <- "symbolic_artifacts_material"
cm_symbolic_artifacts_technology <- "symbolic_artifacts_technology"
cm_symbolic_artifacts_raw_material_source <- "symbolic_artifacts_raw_material_source"
cm_feature_interpretation <- "feature_interpretation"
cm_miscellaneous_finds_material <- "miscellaneous_finds_material"
cm_miscellaneous_finds_raw_material_source <- "miscellaneous_finds_raw_material_source"

#' run query in ROAD database
#' 
#' @param query specifies the SQl query.
#'
#' @return Database search result as a data frame.
#' @keywords internal

road_run_query <- function(query)
{
  query <- trimws(query)

  if (query == "") {
    stop("Query can not be empty.")
  }

  # con <- dbConnect(RPostgres::Postgres(), dbname = "roceeh", host="134.2.216.14", 
  #                  port=5432, user=rstudioapi::askForPassword("Database username"), 
  #                  password=rstudioapi::askForPassword("Database password"))
  con <- dbConnect(RPostgres::Postgres(), dbname = "roceeh", host = "134.2.216.14", 
  port = 5432, user = user_name, password = user_password)

  # run query
  result <- dbGetQuery(con, query)

  #message(str(result$transport_distance))
  #message(str(result))
  
  # replace all possible "NULL" values with NA
  result[result == ""] <- NA
  result[result == -1] <- NA
  
  #'unknown' is a correct value of 'transport_distance', we dont want replace it.
  if ("transport_distance" %in% colnames(result))
    result['transport_distance'][result['transport_distance'] == 'unknown'] <- 'unknownunknown'
  
  result[result == "undefined"] <- NA
  result[result == "unknown"] <- NA
  
  if ("transport_distance" %in% colnames(result))
    result['transport_distance'][result['transport_distance'] == 'unknownunknown'] <- 'unknown'

  return(result)
}


# convert string parameter to vector
parameter_to_query <- function(query_start = "", parameter, query_end = "")
{
  query <- ""
  if (!is.null(parameter))
  {
    parameter <- parameter_to_vector(parameter)

    if (is.vector(parameter))
    {
      query <- paste0(
        query_start,
        paste(
          sapply(parameter, function(x) paste0("'", x, "'")),
          collapse = ", "
        ),
        query_end
      )
    }
    else
      stop(paste("Wrong input for '", deparse(substitute(parameter)), "'."))
  }

  return(query)
}


# build query to check if parameters intersect with comma separated database values
query_check_intersection <- function(query_start = "", parameter, column)
{
  query <- ""
  if (!is.null(parameter))
  {
    parameter <- parameter_to_vector(parameter)

    if (is.vector(parameter))
    {
      query <- paste(
        #sapply(parameter, function(x) paste0("OR '", x, "' = ANY(STRING_TO_ARRAY(", column, ", ', '))")),
        sapply(parameter, function(x) paste0("OR '", x, "' = ANY(regexp_split_to_array(", column, ", ',\\s*'))")),
        collapse = " "
      )
      query <- paste0(
        query_start,
        "(",
        sub("OR ", "", query),
        ")"
      )
    }
    else
      stop(paste("Wrong input for '", deparse(substitute(parameter)), "'."))
  }

  return(query)
}


# build query to check if parameters occur in database string value
query_values_in_string <- function(query_start = "", parameter, column)
{
  query <- ""
  if (!is.null(parameter))
  {
    parameter <- parameter_to_vector(parameter)

    if (is.vector(parameter))
    {
      query <- paste(
        sapply(parameter, function(x) paste0("OR ", column, " LIKE '%", x, "%'")),
        collapse = " "
      )
      query <- paste0(
        query_start,
        "(",
        sub("OR ", "", query),
        ")"
      )
    }
    else
      stop(paste("Wrong input for '", deparse(substitute(parameter)), "'."))
  }

  return(query)
}


# convert non-vector parameter to vector
parameter_to_vector <- function(parameter)
{
  # convert string to vector
  if (is.string(parameter) && parameter != "")
    parameter <- c(parameter)

  # convert integer to vector
  if (is.integer(parameter) && parameter != 0)
    parameter <- c(parameter)

  return(parameter)
}


add_locality_columns <- function(data, localities = NULL, assemblages = NULL)
{
  if (is.null(localities) && is.null(assemblages))
    stop("Either 'localities' or 'assemblages' have to be set.")

  if (!is.null(localities))
  {
    column_selection <- c(
      cm_locality_idlocality,
      cm_geopolitical_units_continent,
      cm_geopolitical_units_continent_region,
      cm_locality_country,
      cm_locality_types,
      cm_locality_x,
      cm_locality_y
    )
    localities <- localities[, column_selection]
    data <- merge(x = localities, y = data, by = cm_locality_idlocality, all.y = TRUE)
  }
  else
  {
    column_selection <- c(
      cm_locality_idlocality,
      cm_assemblages_idassemblage,
      cm_geopolitical_units_continent,
      cm_geopolitical_units_continent_region,
      cm_locality_country,
      cm_locality_types,
      cm_locality_x,
      cm_locality_y,
      cm_assemblages_name,
      cm_assemblages_categories,
      cm_geological_stratigraphy_age_min,
      cm_geological_stratigraphy_age_max,
      cm_cultural_periods,
      cm_technocomplexes
    )
    assemblages <- assemblages[, column_selection]
    data <- merge(x = assemblages, y = data, by = c(cm_locality_idlocality, 
                                                    cm_assemblages_idassemblage), all.y = TRUE)
  }

  return(data)
}


# calculate assemblage_condition
get_assemblage_condition <- function(query_start = "", assemblages = NULL, locality_id_column_name = cm_locality_idlocality, assemblage_id_column_name = cm_assemblages_idassemblage)
{
  # I am not sure, if it is better to do the assemblage search hier or in the caller function
  # so this comments is an reminder
  # To do: !is.null(categories) AND !is.null(assemblages)  ---> Warnung an den Benutzer
  #if (is.null(assemblages)) assemblages <- road_get_assemblages(categories = categories, 
  #                                                             age_min = age_min, age_max = age_max, localities = localities)

  if (nrow(assemblages) == 0) return(paste0(query_start, " FALSE "))
  
  locality_assemblage_list <- paste(assemblages$locality_id, assemblages$assemblage_id, sep = ", ")

  query_locality_assemblage_list_str <- ""
  query_locality_assemblage_list_str <- paste(
    sapply(locality_assemblage_list, function(x) paste0("'", x, "'")),
    collapse = ", "
  )

  assemblage_condition <- ""
  if (!is.null(query_locality_assemblage_list_str) && query_locality_assemblage_list_str != "")
  {
    assemblage_condition <- paste0(
      query_start,
      locality_id_column_name,
      " || ', ' || ",
      assemblage_id_column_name,
      " IN (",
      query_locality_assemblage_list_str,
      ")"
    )
  }
  
  return(assemblage_condition)
}

# 
print_null_result_message <- function(
    continents = NULL,
    subcontinents = NULL,
    countries = NULL,
    locality_types = NULL,
    cultural_periods = NULL,
    technocomplexes = NULL,
    categories = NULL,
    age_min = NULL,
    age_max = NULL,
    tool_list = NULL,
    raw_material_list = NULL,
    transport_distance = NULL,
    organic_tools_interpretation = NULL,
    symbolic_artifacts_interpretation = NULL,
    feature_interpretation = NULL,
    miscellaneous_finds_material = NULL,
    genus = NULL,
    species = NULL,
    plant_remains = NULL,
    plant_family = NULL,
    plant_genus = NULL,
    plant_species = NULL,
    fauna_genus = NULL,
    fauna_species = NULL
)
{
  continents_str <- ifelse(is.null(continents), "", paste("continents = (", toString(continents), ")"))
  subcontinents_str <- ifelse(is.null(subcontinents), "", paste("subcontinents = (", toString(subcontinents), ")"))
  countries_str <- ifelse(is.null(countries), "", paste("countries = (", toString(countries), ")"))
  locality_types_str <- ifelse(is.null(locality_types), "", paste("locality_types = (", toString(locality_types), ")"))
  cultural_periods_str <- ifelse(is.null(cultural_periods), "", paste("cultural_periods = (", toString(cultural_periods), ")"))
  
  technocomplexes_str <- ifelse(is.null(technocomplexes), "", paste("technocomplexes = (", toString(technocomplexes), ")"))
  categories_str <- ifelse(is.null(categories), "", paste("categories = (", toString(categories), ")"))
  age_min_str <- ifelse(is.null(age_min), "", paste("age_min = (", age_min, ")"))
  age_max_str <- ifelse(is.null(age_max), "", paste("age_max = (", age_max, ")"))
  
  tool_list_str <- ifelse(is.null(tool_list), "", paste("tool_list = (", toString(tool_list), ")"))
  transport_distance_str <- ifelse(is.null(transport_distance), "", paste("transport_distance = (", toString(transport_distance), ")"))
  raw_material_list_str <- ifelse(is.null(raw_material_list), "", paste("raw_material_list = (", toString(raw_material_list), ")"))
  organic_tools_interpretation_str <- ifelse(is.null(organic_tools_interpretation), "", paste("organic_tools_interpretation = (", toString(organic_tools_interpretation), ")"))
  symbolic_artifacts_interpretation_str <- ifelse(is.null(symbolic_artifacts_interpretation), "", paste("symbolic_artifacts_interpretation = (", toString(symbolic_artifacts_interpretation), ")"))
  feature_interpretation_str <- ifelse(is.null(feature_interpretation), "", paste("feature_interpretation = (", toString(feature_interpretation), ")"))
  miscellaneous_finds_material_str <- ifelse(is.null(miscellaneous_finds_material), "", paste("miscellaneous_finds_material = (", toString(miscellaneous_finds_material), ")"))
  
  genus_str <- ifelse(is.null(genus), "", paste("genus = (", toString(genus), ")"))
  species_str <- ifelse(is.null(species), "", paste("species = (", toString(species), ")"))

  plant_remains_str <- ifelse(is.null(plant_remains), "", paste("plant_remains = (", toString(plant_remains), ")"))
  plant_family_str <- ifelse(is.null(plant_family), "", paste("plant_family = (", toString(plant_family), ")"))
  plant_genus_str <- ifelse(is.null(plant_genus), "", paste("plant_genus = (", toString(plant_genus), ")"))
  plant_species_str <- ifelse(is.null(plant_species), "", paste("plant_species = (", toString(plant_species), ")"))
  
  fauna_genus_str <- ifelse(is.null(fauna_genus), "", paste("fauna_genus = (", toString(fauna_genus), ")"))
  fauna_species_str <- ifelse(is.null(fauna_species), "", paste("fauna_species = (", toString(fauna_species), ")"))
  
  message(paste("One or more of the following arguments caused the empty result set:
                  ",
                continents_str,
                subcontinents_str,
                countries_str,
                locality_types_str,
                cultural_periods_str,
                technocomplexes_str,
                categories_str,
                age_min_str,
                age_max_str,
                tool_list_str,
                raw_material_list_str,
                transport_distance_str,
                organic_tools_interpretation_str,
                symbolic_artifacts_interpretation_str,
                feature_interpretation_str,
                miscellaneous_finds_material_str,
                genus_str,
                species_str,
                plant_remains_str,
                plant_family_str,
                plant_genus_str,
                plant_species_str,
                fauna_genus_str,
                fauna_species_str,
                "
      Please keep in mind, the data search needs for most arguments exact argument value. To get exact value for a given argument 'p' you can use the function road_list_argument_values('p')."))
  
  if (is.vector(raw_material_list) && is.vector(transport_distance))
  {
    cp <- expand.grid(raw_material_list = raw_material_list, transport_distance = transport_distance)
    
    cp <- cp %>% mutate(raw_material_list_transport_distance = paste(raw_material_list, transport_distance, sep = " "))
    s <- paste(cp$raw_material_list_transport_distance, collapse = "; ")
    message(paste("
      Please keep in mind at least one of the the following combinations ( raw_material_list transport_distance) have to be in the database:
                  ", s))
  }
  
  if (is.vector(genus) && is.vector(species))
  {
    cp <- expand.grid(genus = genus, species = species)
    
    cp <- cp %>% mutate(genus_species = paste(genus, species, sep = " "))
    s <- paste(cp$genus_species, collapse = "; ")
    message(paste("
      Please keep in mind at least one of the the following combinations (human_genus human_species) have to be in the database:
                  ", s))
  }

  if ((is.vector(plant_remains) && is.vector(plant_family) && is.vector(plant_genus) && is.vector(plant_species))
      || (is.vector(plant_remains) && is.vector(plant_family) && is.vector(plant_genus))
      || (is.vector(plant_family) && is.vector(plant_genus) && is.vector(plant_species))
      || (is.vector(plant_remains) && is.vector(plant_family) && is.vector(plant_species))
      || (is.vector(plant_remains) && is.vector(plant_genus) && is.vector(plant_species))
      || (is.vector(plant_remains) && is.vector(plant_family))
      || (is.vector(plant_family) && is.vector(plant_genus))
      || (is.vector(plant_genus) && is.vector(plant_species))
      || (is.vector(plant_remains) && is.vector(plant_genus))
      || (is.vector(plant_family) && is.vector(plant_species))
      || (is.vector(plant_remains) && is.vector(plant_species))
  )
  {
    if (is.null(plant_remains))
    {
      plant_remains_out <- c("")
      pr <- ""
    }
    else
    {
      pr <- "plant_remains "
      plant_remains_out <- plant_remains
    }
    if (is.null(plant_family))
    {
      plant_family_out <- c("")
      pf <- ""
    }
    else
    {
      plant_family_out <- plant_family
      pf <- "plant_family "
    }
    if (is.null(plant_genus))
    {
      plant_genus_out <- c("")
      pg <- ""
    }
    else
    {
      plant_genus_out <- plant_genus
      pg <- "plant_genus "
    }
    if (is.null(plant_species))
    {
      plant_species_out <- c("")
      ps <- ""
    }
    else
    {
      plant_species_out <- plant_species
      ps <- "plant_species"
    }
    cp <- expand.grid(remains = plant_remains_out, family = plant_family_out, genus = plant_genus_out, species = plant_species_out)
    
    cp <- cp %>% mutate(remains_family_genus_species = paste(remains, family, genus, species, sep = " "))
    s <- paste(cp$remains_family_genus_species, collapse = "); (")
    message(paste0("
      Please keep in mind at least one of the the following combinations (", pr, pf, pg, ps, ")"," have to be in the database:
                  ", "(", s, ")"))
  }
  
  if (is.vector(fauna_genus) && is.vector(fauna_species))
  {
    genus <- ""
    cp <- expand.grid(genus = fauna_genus, species = fauna_species)
    
    cp <- cp %>% mutate(genus_species = paste(genus, species, sep = " "))
    s <- paste(cp$genus_species, collapse = "; ")
    message(paste("
      Please keep in mind at least one of the the following combinations (fauna_genus fauna_species) have to be in the database:
                  ", s))
  }
  
}
  
