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
cm_humanremains_genus_species_str <- "genus_species_str"
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

# run query in ROAD db
road_run_query <- function(query)
{
  query <- trimws(query)

  if (query == "") {
    stop("Query can not be empty.")
  }

  #con <- dbConnect(RPostgres::Postgres(), dbname = "roceeh", host="134.2.216.14", port=5432, user=rstudioapi::askForPassword("Database username"), password=rstudioapi::askForPassword("Database password"))
  con <- dbConnect(RPostgres::Postgres(), dbname = "roceeh", host = "134.2.216.14", port = 5432, user = user_name, password = user_password)

  # run query
  result <- dbGetQuery(con, query)

  # replace all possible "NULL" values with NA
  result[result == ""] <- NA
  result[result == -1] <- NA
  result[result == "undefined"] <- NA
  result[result == "unknown"] <- NA

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
      cm_cultural_periods
    )
    assemblages <- assemblages[, column_selection]
    data <- merge(x = assemblages, y = data, by = c(cm_locality_idlocality, cm_assemblages_idassemblage), all.y = TRUE)
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

  locality_assemblage_list <- paste(assemblages$locality_id, assemblages$assemblage_id, sep = ", ")

  # selected_cols <- c(1, 2)
  # locality_assemblage_list <- do.call(paste, c(assemblages2[selected_cols], sep = ", "))

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

