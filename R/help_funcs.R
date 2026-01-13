#source("./R/login.R")

# column names
cm_locality_idlocality <- "locality_id"
cm_locality_type <- "locality_type"
cm_geopolitical_units_continent <- "continent"
cm_geopolitical_units_continent_region <- "subcontinent"
cm_locality_country <- "country"
cm_locality_x <- "coord_x"
cm_locality_y <- "coord_y"
cm_coordinate_source <- "coordinate_source"
cm_cultural_period <- "cultural_period"
cm_technocomplex <- "technocomplex"
cm_assemblages_locality_idlocality <- "locality_id"
cm_assemblages_idassemblage <- "assemblage_id"
cm_assemblages_name <- "assemblage_name"
cm_is_systematic <- "is_systematic"
cm_assemblages_category <- "category"
cm_geological_stratigraphy_age_min <- "age_min"
cm_geological_stratigraphy_age_max <- "age_max"

cm_assemblage_in_geolayer_geolayer_name <- "geolayer"
cm_geolayer_geolayer_name <- "geolayer"
cm_assemblage_in_archlayer_archlayer_name <- "archlayer"
cm_archlayer_archlayer_name <- "archlayer"
cm_comments <- "comment"
cm_age <- "age"
cm_negative_standard_deviation <- "negative_standard_deviation"
cm_positive_standard_deviation <- "positive_standard_deviation"
cm_material_dated <- "material_dated"
cm_dating_method <- "dating_method"
cm_laboratory_idlaboratory <- "laboratory_idlaboratory"
#cm_human_remains_genus_species_str <- "genus_species_str"
cm_human_remains_genus <- "genus"
cm_human_remains_species <- "species"
cm_human_remains_age <- "age"
cm_human_remains_sex <- "sex"
cm_human_remains_skeletal_element <- "skeletal_element"
cm_human_remains_category <- "human_remains_category"
cm_human_remains_idhuman_remains <- "human_remains_id"
cm_archaeological_category <- "archaeological_category"
cm_paleoflora_plant_remains <- "plant_remains"
cm_paleoflora_element <- "element"
cm_paleoflora_abundance <- "abundance"
cm_paleoflora_relative_abundance <- "relative_abundance"
cm_plant_taxonomy_family <- "plant_family"
cm_plant_taxonomy_genus <- "plant_genus"
cm_plant_taxonomy_species <- "plant_species"
cm_fauna_genus <- "fauna_genus"
cm_fauna_species <- "fauna_species"
cm_fauna_mni <- "mni"
cm_fauna_mni_method <- "mni_method"
cm_fauna_nisp <- "nisp" 
cm_tool_list <- "tool_list"
cm_typology <- "typology"
cm_raw_material_list <- "raw_material_list"
cm_transport_distance <- "transport_distance"
cm_organic_tool_interpretation <- "organic_tool_interpretation"
cm_feature_interpretation <- "feature_interpretation"
cm_organic_raw_material <- "organic_raw_material"
cm_organic_tool_technology <- "organic_tool_technology"
cm_symbolic_artifact_interpretation <- "symbolic_artifact_interpretation"
cm_symbolic_artifact_category <- "symbolic_artifact_category"
cm_symbolic_artifact_material <- "symbolic_artifact_material"
cm_symbolic_artifact_technology <- "symbolic_artifact_technology"
cm_symbolic_artifact_raw_material_source <- "symbolic_artifact_raw_material_source"
cm_feature_interpretation <- "feature_interpretation"
cm_miscellaneous_find_material <- "miscellaneous_find_material"
cm_miscellaneous_find_raw_material_source <- "miscellaneous_find_raw_material_source"

# run query in ROAD database
# 
# param query specifies the SQl query.
# 
# return Database search result as a data frame.
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
  
  max_attempts <- 5
  attempt <- 1
  result <- NULL
  
  while (attempt <= max_attempts && is.null(result)) {
  
    con <- dbConnect(RPostgres::Postgres(), dbname = "road", host = "134.2.216.13", 
                     port = 5432, user = "road_user", password = "road")

    # run query
    result <- tryCatch({dbGetQuery(conn = con, statement = query) #, keepalives = 1, keepalives_idle = 1200)
              }, error = function(e) {
                           if (attempt == max_attempts) {
                             stop("Final attempt failed: ", e$message)
                           }
                           # Wait 2^attempt seconds (2, 4, 8, 16...)
                           wait <- 2^attempt
                           message(paste("Attempt", attempt, "of", max_attempts, "..."))
                           # message(sprintf("Attempt %d failed. Retrying in %d seconds...", attempt, wait))
                           Sys.sleep(wait)
                           return(NULL)
             })
  
    attempt <- attempt + 1
    #message(paste("Attempt", attempt, "of", max_attempts, "..."))
  }

  # replace all possible "NULL" values with NA
  result[result == ""] <- NA
  result[result == -1] <- NA
  result[result == "undefined"] <- NA

  # "unknown" is a correct value of 'transport_distance', we dont want replace it.
  # if ("transport_distance" %in% colnames(result))
  #  result['transport_distance'][result['transport_distance'] == 'unknown'] <- 'unknownunknown'
  
  # result[result == "unknown"] <- NA

  # if ("transport_distance" %in% colnames(result))
  #  result['transport_distance'][result['transport_distance'] == 'unknownunknown'] <- 'unknown'

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

# this function adds locality or assemblage columns
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
      cm_locality_type,
      cm_locality_x,
      cm_locality_y,
      cm_coordinate_source
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
      cm_locality_type,
      cm_locality_x,
      cm_locality_y,
      cm_assemblages_name,
      cm_assemblages_category,
      cm_geological_stratigraphy_age_min,
      cm_geological_stratigraphy_age_max,
      cm_assemblage_in_geolayer_geolayer_name,
      cm_assemblage_in_archlayer_archlayer_name,
      cm_cultural_period,
      cm_technocomplex
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
  # so this comment is an reminder
  # To do: !is.null(category) AND !is.null(assemblages)  ---> Warnung an den Benutzer
  #if (is.null(assemblages)) assemblages <- road_get_assemblages(category = category, 
  #                                                             age_min = age_min, age_max = age_max, localities = localities)

  if (nrow(assemblages) == 0) return(paste0(query_start, " FALSE "))
  
  assemblages$locality_assemblage_cols <- paste(assemblages$locality_id, assemblages$assemblage_id, sep = ", ")
  #locality_assemblage_cols <- paste(assemblages$locality_id, assemblages$assemblage_id, sep = ", ")
  
  query_locality_assemblage_list_str <- ""
  query_locality_assemblage_list_str <- paste(
    sapply(assemblages$locality_assemblage_cols, function(x) paste0("'", x, "'")),
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

# calculate geolayer_condition
get_geolayer_condition <- function(query_start = "", assemblages = NULL, locality_id_column_name = cm_locality_idlocality, geolayer_column_name = cm_geolayer_geolayer_name)
{
  if (nrow(assemblages) == 0) return(paste0(query_start, " FALSE "))
  
  assemblages$locality_geolayer_cols <- ''
  
  for (r in 1:nrow(assemblages))   
  {
    tt <- unlist(strsplit(assemblages[r,'geolayer'], ', '))
    assemblages[r,'locality_geolayer_cols'] <- paste(assemblages[r,'locality_id'], tt, sep = ', ', collapse = '%%')
  }

  locality_geolayer_str <- paste(assemblages$locality_geolayer_cols, collapse = '%%')
  locality_geolayer_vec <- unlist(strsplit(locality_geolayer_str, '%%'))

  query_locality_geolayer_list_str <- ""
  query_locality_geolayer_list_str <- paste(
    sapply(locality_geolayer_vec, function(x) paste0("'", x, "'")),
    collapse = ", "
  )
  
  geolayer_condition <- ""
  if (!is.null(query_locality_geolayer_list_str) && query_locality_geolayer_list_str != "")
  {
    geolayer_condition <- paste0(
      query_start,
      locality_id_column_name,
      " || ', ' || ",
      geolayer_column_name,
      " IN (",
      query_locality_geolayer_list_str,
      ")"
    )
  }
  #message(geolayer_condition)
  return(geolayer_condition)
}

# calculate archlayer_condition
get_archlayer_condition <- function(query_start = "", assemblages = NULL, locality_id_column_name = cm_locality_idlocality, archlayer_column_name = cm_archlayer_archlayer_name)
{
  if (nrow(assemblages) == 0) return(paste0(query_start, " FALSE "))
  
  assemblages$locality_archlayer_cols <- ''
  
  for (r in 1:nrow(assemblages))   
  {
    tt <- unlist(strsplit(assemblages[r,'archlayer'], ', '))
    assemblages[r,'locality_archlayer_cols'] <- paste(assemblages[r,'locality_id'], tt, sep = ', ', collapse = '%%')
  }
  
  locality_archlayer_str <- paste(assemblages$locality_archlayer_cols, collapse = '%%')
  locality_archlayer_vec <- unlist(strsplit(locality_archlayer_str, '%%'))
  
  query_locality_archlayer_list_str <- ""
  query_locality_archlayer_list_str <- paste(
    sapply(locality_archlayer_vec, function(x) paste0("'", x, "'")),
    collapse = ", "
  )
  
  archlayer_condition <- ""
  if (!is.null(query_locality_archlayer_list_str) && query_locality_archlayer_list_str != "")
  {
    archlayer_condition <- paste0(
      query_start,
      locality_id_column_name,
      " || ', ' || ",
      archlayer_column_name,
      " IN (",
      query_locality_archlayer_list_str,
      ")"
    )
  }
  #message(archlayer_condition)
  return(archlayer_condition)
}

# 
print_null_result_message <- function(
    continent = NULL,
    subcontinent = NULL,
    country = NULL,
    locality_type = NULL,
    cultural_period = NULL,
    technocomplex = NULL,
    category = NULL,
    age_min = NULL,
    age_max = NULL,
    tool_list = NULL,
    raw_material_list = NULL,
    transport_distance = NULL,
    organic_tool_interpretation = NULL,
    symbolic_artifact_interpretation = NULL,
    feature_interpretation = NULL,
    miscellaneous_find_material = NULL,
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
  continent_str <- ifelse(is.null(continent), "", paste("continent = (", toString(continent), ")"))
  subcontinent_str <- ifelse(is.null(subcontinent), "", paste("subcontinent = (", toString(subcontinent), ")"))
  country_str <- ifelse(is.null(country), "", paste("country = (", toString(country), ")"))
  locality_type_str <- ifelse(is.null(locality_type), "", paste("locality_type = (", toString(locality_type), ")"))
  cultural_period_str <- ifelse(is.null(cultural_period), "", paste("cultural_period = (", toString(cultural_period), ")"))
  
  technocomplex_str <- ifelse(is.null(technocomplex), "", paste("technocomplex = (", toString(technocomplex), ")"))
  category_str <- ifelse(is.null(category), "", paste("category = (", toString(category), ")"))
  age_min_str <- ifelse(is.null(age_min), "", paste("age_min = (", age_min, ")"))
  age_max_str <- ifelse(is.null(age_max), "", paste("age_max = (", age_max, ")"))
  
  tool_list_str <- ifelse(is.null(tool_list), "", paste("tool_list = (", toString(tool_list), ")"))
  transport_distance_str <- ifelse(is.null(transport_distance), "", paste("transport_distance = (", toString(transport_distance), ")"))
  raw_material_list_str <- ifelse(is.null(raw_material_list), "", paste("raw_material_list = (", toString(raw_material_list), ")"))
  organic_tool_interpretation_str <- ifelse(is.null(organic_tool_interpretation), "", paste("organic_tool_interpretation = (", toString(organic_tool_interpretation), ")"))
  symbolic_artifact_interpretation_str <- ifelse(is.null(symbolic_artifact_interpretation), "", paste("symbolic_artifact_interpretation = (", toString(symbolic_artifact_interpretation), ")"))
  feature_interpretation_str <- ifelse(is.null(feature_interpretation), "", paste("feature_interpretation = (", toString(feature_interpretation), ")"))
  miscellaneous_find_material_str <- ifelse(is.null(miscellaneous_find_material), "", paste("miscellaneous_find_material = (", toString(miscellaneous_find_material), ")"))
  
  human_genus_str <- ifelse(is.null(genus), "", paste("human_genus = (", toString(genus), ")"))
  human_species_str <- ifelse(is.null(species), "", paste("human_species = (", toString(species), ")"))

  plant_remains_str <- ifelse(is.null(plant_remains), "", paste("plant_remains = (", toString(plant_remains), ")"))
  plant_family_str <- ifelse(is.null(plant_family), "", paste("plant_family = (", toString(plant_family), ")"))
  plant_genus_str <- ifelse(is.null(plant_genus), "", paste("plant_genus = (", toString(plant_genus), ")"))
  plant_species_str <- ifelse(is.null(plant_species), "", paste("plant_species = (", toString(plant_species), ")"))
  
  fauna_genus_str <- ifelse(is.null(fauna_genus), "", paste("fauna_genus = (", toString(fauna_genus), ")"))
  fauna_species_str <- ifelse(is.null(fauna_species), "", paste("fauna_species = (", toString(fauna_species), ")"))
  
  message(paste("One or more of the following arguments caused the empty result set:
                  ",
                continent_str,
                subcontinent_str,
                country_str,
                locality_type_str,
                cultural_period_str,
                technocomplex_str,
                category_str,
                age_min_str,
                age_max_str,
                tool_list_str,
                raw_material_list_str,
                transport_distance_str,
                organic_tool_interpretation_str,
                symbolic_artifact_interpretation_str,
                feature_interpretation_str,
                miscellaneous_find_material_str,
                human_genus_str,
                human_species_str,
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
    # cp <- expand.grid(raw_material_list = raw_material_list, transport_distance = transport_distance)
    cp <- expand.grid(raw_material_list, transport_distance)
    names(cp) <- c('raw_material_list', 'transport_distance')
    
    cp <- cp %>% mutate(raw_material_list_transport_distance = paste(raw_material_list, transport_distance, sep = " "))
    s <- paste(cp$raw_material_list_transport_distance, collapse = "; ")
    message(paste("
      Please note at least one of the following combinations ( raw_material_list transport_distance) have to be in the database:
                  ", s))
  }
  
  if (is.vector(genus) && is.vector(species))
  {
    # cp <- expand.grid(genus = genus, species = species)
    cp <- expand.grid(genus, species)
    names(cp) <- c('genus', 'species')
    
    cp <- cp %>% mutate(genus_species = paste(genus, species, sep = " "))
    s <- paste(cp$genus_species, collapse = "; ")
    message(paste("
      Please note at least one of the following combinations (human_genus human_species) have to be in the database:
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
  
    #cp <- expand.grid(remains = plant_remains_out, family = plant_family_out, genus = plant_genus_out, species = plant_species_out)
    cp <- expand.grid(plant_remains_out, plant_family_out, plant_genus_out, plant_species_out)
    names(cp) <- c('remains', 'family', 'genus', 'species')
    
    # cp <- cp %>% mutate(remains_family_genus_species = paste(remains, family, genus, species, sep = " "))
    cp$remains_family_genus_species <- paste(cp$remains, cp$family, cp$genus, cp$species, sep = " ")
    s <- paste(cp$remains_family_genus_species, collapse = "); (")
    message(paste0("
      Please note at least one of the following combinations (", pr, pf, pg, ps, ")"," have to be in the database:
                  ", "(", s, ")"))
  }
  
  if (is.vector(fauna_genus) && is.vector(fauna_species))
  {
    #cp <- expand.grid(genus = fauna_genus, species = fauna_species)
    cp <- expand.grid(fauna_genus, fauna_species)
    names(cp) <- c('genus', 'species')
    
    cp <- cp %>% mutate(genus_species = paste(genus, species, sep = " "))
    s <- paste(cp$genus_species, collapse = "; ")
    message(paste("
      Please keep in mind at least one of the following combinations (fauna_genus fauna_species) have to be in the database:
                  ", s))
  }
  
}

# eliminate duplicates from a list as string
well_formed_string_to_string_without_duplicates <- function(str_with_duplikates = NULL, separator = ",[ ]*")
{
  # if (is.na(str_with_duplikates)) message(str_with_duplikates)
  l <- str_split(str_with_duplikates, separator)
  v <- unique(l[[1]])
  
  vp <- paste0(v, collapse = ", ")
  if (vp == "") return(NA) 
  else return(vp) #(paste0(v, collapse = ", "))
  
}

#  Returns the last n characters from a string x
substr_right <- function(x, n)
{
  substr(x, nchar(x) - n + 1, nchar(x))
}
