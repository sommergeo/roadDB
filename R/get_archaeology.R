#' Get lithic typology from ROAD database
#'
#' The \strong{\code{road_get_lithic_typology}} fetches data of lithic finds 
#' from ROAD databaseLithic typology refers to the classification of stone tools 
#' based on their shape, technology, and function.This function enables you to 
#' query lithic typology data from the ROAD database using various parameters such as
#' geographical location, cultural periods, tool types, and assemblages. Use the 
#' parameters to filter the results according to your research needs, or omit them 
#' to retrieve a broader dataset.
#'
#' @param continent specifies the continent(s) (e.g. Africa, Europe, Asia).
#' Run \code{road_list_argument_values("continent")} to display possible values.
#' The argument \code{continent} is a string (one item) or vector of strings 
#' (one or more items); defaults to NULL.
#' @param subcontinent specifies the continental region(s) (e.g. Southern Europe). 
#' Run \code{road_list_argument_values("subcontinent")} to display possible values.
#' The argument \code{subcontinent} is a string (one item) or vector of strings 
#' (one or more items); defaults to NULL.
#' @param country specifies the name of the country where a locality is situated 
#' (e.g. Germany, Kenya, Saudi Arabia). Run \code{road_list_argument_values("country")} 
#' to display possible values.
#' The argument \code{country} is a string (one item) or vector of strings 
#' (one or more items); defaults to NULL.
#' @param locality_type specifies the type of locality (e.g. cave, rockshelter, open air).
#' Run \code{road_list_argument_values("locality_type")} to display possible values.
#' The argument \code{locality_type} is a string (one item) or vector of strings 
#' (one or more items); defaults to NULL.
#' @param cultural_periods specifies the main cultural epoch(s) and includes the 
#' Eurasian Paleolithic (Lower, Middle, Upper, Epi-) and the African Stone Age 
#' (Earlier, Middle, Later). Run \code{road_list_argument_values("cultural_periods")} 
#' to display possible values. The argument \code{cultural_periods} is a string 
#' (one item) or vector of strings (one or more items); defaults to NULL.
#' @param technocomplexes specifies an archaeological culture or named stone tool 
#' industry (e.g. Oldowan, Acheulean, Mousterian).
#' Run \code{road_list_argument_values("technocomplexes")} to display possible values.
#' The argument \code{technocomplexes} is a string (one item) or vector of strings 
#' (one or more items); defaults to NULL.
#' @param category specifies the assemblage category with the classes 
#' human remains, raw material, typology, technology, function, organic tools, 
#' symbolic artifacts, feature, miscellaneous finds, paleofauna, animal remains, 
#' plant remains. The argument \code{category} is a string (one item) or 
#' vector of strings (one or more items); defaults to NULL.
#' @param age_min specifies the minimum age in years before present, using 1950 CE
#' as the baseline. If possible the argument \code{age_min} will be converted to an integer; defaults to NULL.
#' @param age_max specifies the maximum age in years before present, using 1950 CE
#' as the baseline. If possible the argument \code{age_max} will be converted to an integer; defaults to NULL.
#' @param assemblages specifies a data frame necessarily containing columns 
#' locality_id, assemblage_id. It can be  generated as return value of the 
#' function 'road_get_assemblages'. It can be used instead of the locality 
#' and assemblage search parameters to filter the results.
#' @param tool_list specifies values that can be entered for various tool types. 
#' Tool types can contain 1) chipped tool types like scraper end, scraper side, 
#' scraper, carinated, burin, handaxe, chopper, cleaver, point, point unifacial, 
#' segment, unknown; 2) non-chipped tool types like grindstone upper, hammerstone, 
#' anvil, retoucher; 3) non-tools like core, debitage, flake, point; 4) unknown 
#' like cobble, block, manuport. Run \code{road_list_argument_values("tool_list")} 
#' to display possible values. The argument \code{tool_list} is a string (one item) 
#' or vector of strings (one or more items); defaults to NULL.
#'
#' @return Database search result as a data frame with the information about 
#' lithic finds. Rows represent lithic finds and contain standard information 
#' like locality name, geographic information, cultural period, locality type, 
#' assemblage category, dating and lithic typology-related details:
#' @return \code{typology}: Description of the group of tool type present in the 
#' given lithic record. Following four values are possible: chipped tool, non-chipped 
#' tool, non-tool and unknown.
#' @return \code{percentage}: Percentage of the typology type of the given record
#' in the corresponding assemblage. 
#' @return \code{tool list}: List of tool types in the given lithic record.
#' @export
#'
#' @examples
#' road_get_lithic_typology(continent = c("Asia"), tool_list = "adze")
#' road_get_lithic_typology(subcontinent = "Eastern Europe", 
#'                          tool_list = c("bladelet burin spall"))
road_get_lithic_typology <- function(
    continent = NULL,
    subcontinent = NULL,
    country = NULL,
    locality_type = NULL,
    cultural_periods = NULL,
    technocomplexes = NULL,
    category = NULL,
    age_min = NULL,
    age_max = NULL,
    assemblages = NULL, 
    tool_list = NULL
)
{
  # calculate assemblage_condition
  # To do: !is.null(category) AND !is.null(assemblages)  ---> Warnung an den Benutzer
  if (is.null(assemblages)) assemblages <- road_get_assemblages(continent = continent, 
                                                                subcontinent = subcontinent, 
                                                                country = country, 
                                                                locality_type = locality_type, 
                                                                cultural_periods = cultural_periods, 
                                                                technocomplexes = technocomplexes, 
                                                                category = category, 
                                                                age_min = age_min, 
                                                                age_max = age_max)
  assemblage_condition <- get_assemblage_condition(assemblages = assemblages, locality_id_column_name = "typology.assemblage_idlocality", assemblage_id_column_name = "typology.assemblage_idassemblage")

  # select fields
  select_fields <- c(
    paste0("assemblage_idlocality AS ", cm_locality_idlocality),
    paste0("assemblage_idassemblage AS ", cm_assemblages_idassemblage),
    paste0("tool_list AS ", cm_tool_list),
    paste0("typology AS ", cm_typology),
    "percentage",
    "comments"
  )

  query <- paste(
    "SELECT DISTINCT",
    paste(select_fields, collapse = ", "),
    "FROM typology",
    "WHERE",
    assemblage_condition,
    query_values_in_string("AND ", tool_list, "tool_list")
  )

  data <- road_run_query(query)
  
  if (nrow(data) == 0 && nrow(assemblages) > 0)
  {
    
    print_null_result_message(continent = continent,
                              subcontinent = subcontinent,
                              country = country,
                              locality_type = locality_type,
                              cultural_periods = cultural_periods,
                              technocomplexes = technocomplexes,
                              category = category,
                              age_min = age_min,
                              age_max = age_max,
                              tool_list = tool_list)
  }
  
  data <- add_locality_columns(data, assemblages = assemblages)

  return(data)
}


#' Get lithic raw material from ROAD database
#'
#' The \strong{\code{road_get_lithic_raw_material}} fetches data of lithic finds 
#' from ROAD database. Lithic raw material refers to the types of stone used for 
#' tool production in archaeological contexts. This function allows you to query 
#' lithic raw material data from the ROAD database using parameters such as 
#' geographical location, cultural periods, raw material types, and assemblages. 
#' Use the parameters to filter the results or omit them to retrieve a broader 
#' dataset.
#' 
#' @param continent specifies the continent(s) (e.g. Africa, Europe, Asia).
#' Run \code{road_list_argument_values("continent")} to display possible values.
#' The argument \code{continent} is a string (one item) or vector of strings 
#' (one or more items); defaults to NULL.
#' @param subcontinent specifies the continental region(s) (e.g. Southern Europe). 
#' Run \code{road_list_argument_values("subcontinent")} to display possible values.
#' The argument \code{subcontinent} is a string (one item) or vector of strings 
#' (one or more items); defaults to NULL.
#' @param country specifies the name of the country where a locality is situated 
#' (e.g. Germany, Kenya, Saudi Arabia). Run \code{road_list_argument_values("country")} 
#' to display possible values.
#' The argument \code{country} is a string (one item) or vector of strings 
#' (one or more items); defaults to NULL.
#' @param locality_type specifies the type of locality (e.g. cave, rockshelter, open air).
#' Run \code{road_list_argument_values("locality_type")} to display possible values.
#' The argument \code{locality_type} is a string (one item) or vector of strings 
#' (one or more items); defaults to NULL.
#' @param cultural_periods specifies the main cultural epoch(s) and includes the 
#' Eurasian Paleolithic (Lower, Middle, Upper, Epi-) and the African Stone Age 
#' (Earlier, Middle, Later). Run \code{road_list_argument_values("cultural_periods")} 
#' to display possible values. The argument \code{cultural_periods} is a string 
#' (one item) or vector of strings (one or more items); defaults to NULL.
#' @param technocomplexes specifies an archaeological culture or named stone tool 
#' industry (e.g. Oldowan, Acheulean, Mousterian).
#' Run \code{road_list_argument_values("technocomplexes")} to display possible values.
#' The argument \code{technocomplexes} is a string (one item) or vector of strings 
#' (one or more items); defaults to NULL.
#' @param category specifies the assemblage category with the classes 
#' human remains, raw material, typology, technology, function, organic tools, 
#' symbolic artifacts, feature, miscellaneous finds, paleofauna, animal remains, 
#' plant remains. The argument \code{category} is a string (one item) or 
#' vector of strings (one or more items); defaults to NULL.
#' @param age_min specifies the minimum age in years before present, using 1950 CE
#' as the baseline. If possible the argument \code{age_min} will be converted to an integer; defaults to NULL.
#' @param age_max specifies the maximum age in years before present, using 1950 CE
#' as the baseline. If possible the argument \code{age_max} will be converted to an integer; defaults to NULL.
#' @param assemblages specifies a data frame necessarily containing columns 
#' locality_id, assemblage_id. It can be  generated as return value of the 
#' function 'road_get_assemblages'. It can be used instead of the locality 
#' and assemblage search parameters to filter the results.
#' @param raw_material_list specifies lithic raw materials (e.g. quartz, chert, flint). 
#' Consider the function \code{road_get_organic_tools()} for non-lithic raw materials.
#' Run \code{road_list_argument_values("raw_material_list")} to display possible values.
#' The argument \code{raw_material_list} is a string (one item) or vector of strings 
#' (one or more items); defaults to NULL.
#' @param transport_distance specifies one of the five category, each 
#' distinguished by specific intervals of transport for the raw materials present 
#' in the assemblage.
#' The five classes of transport distance are:
#' \itemize{
#'   \item local (0-5 km) 
#'   \item regional (6-20 km)
#'   \item supra-regional (21-100 km)
#'   \item distant (>100 km)
#'   \item unknown
#' }
#' Run \code{road_list_argument_values("transport_distance")} to display possible values.
#' The argument \code{transport_distance} is a string (one item) or vector of strings; 
#' defaults to NULL.
#'
#' @return Database search result as a data frame with the information about 
#' lithic finds. Rows represent lithic finds and contain standard information 
#' like locality name, geographic information, cultural period, locality type, 
#' assemblage category, dating and raw material-related details:
#' @return \code{transport distance}: Specific interval of transport for the 
#' raw materials in the given record.
#' @return \code{percentage}: Percentage of the raw materials of the given record
#' in the corresponding assemblage.
#' @return \code{raw material list}: List of raw materials of the given record. 
#' 
#' @export
#'
#' @examples
#' road_get_lithic_raw_material(subcontinent = "South Asia", raw_material_list = c("limestone"))
#' road_get_lithic_raw_material(subcontinent = c("Caucasus"), locality_type = "cave",
#'                              raw_material_list = c("chalcedony", "limestone"))
road_get_lithic_raw_material <- function(
    continent = NULL,
    subcontinent = NULL,
    country = NULL,
    locality_type = NULL,
    cultural_periods = NULL,
    technocomplexes = NULL,
    category = NULL,
    age_min = NULL,
    age_max = NULL,
    raw_material_list = NULL,
    transport_distance = NULL,
    assemblages = NULL
)
{
  # calculate assemblage_condition
  # To do: !is.null(category) AND !is.null(assemblages)  ---> Warnung an den Benutzer
  if (is.null(assemblages)) assemblages <- road_get_assemblages(continent = continent, 
                                                                subcontinent = subcontinent, 
                                                                country = country, 
                                                                locality_type = locality_type, 
                                                                cultural_periods = cultural_periods, 
                                                                technocomplexes = technocomplexes, 
                                                                category = category, 
                                                                age_min = age_min, 
                                                                age_max = age_max)
  assemblage_condition <- get_assemblage_condition(assemblages = assemblages, locality_id_column_name = "raw_material.assemblage_idlocality", assemblage_id_column_name = "raw_material.assemblage_idassemblage")

  # select fields
  select_fields <- c(
    paste0("assemblage_idlocality AS ", cm_locality_idlocality),
    paste0("assemblage_idassemblage AS ", cm_assemblages_idassemblage),
    paste0("raw_material_list AS ", cm_raw_material_list),
    paste0("transport_distance AS ", cm_transport_distance),
    "percentage",
    "comments"
  )

  query <- paste(
    "SELECT DISTINCT",
    paste(select_fields, collapse = ", "),
    "FROM raw_material",
    "WHERE",
    assemblage_condition,
    query_values_in_string("AND ", raw_material_list, "raw_material_list"),
    parameter_to_query("AND transport_distance IN (", transport_distance, ")")
    # query_values_in_string("AND ", transport_distance, "transport_distance")
  )

  data <- road_run_query(query)
  
  if (nrow(data) == 0 && nrow(assemblages) > 0)
  {
    
    print_null_result_message(continent = continent,
                              subcontinent = subcontinent,
                              country = country,
                              locality_type = locality_type,
                              cultural_periods = cultural_periods,
                              technocomplexes = technocomplexes,
                              category = category,
                              age_min = age_min,
                              age_max = age_max,
                              raw_material_list = raw_material_list,
                              transport_distance = transport_distance)
  }
  
  data <- add_locality_columns(data, assemblages = assemblages)

  return(data)
}


#' Get organic tools from ROAD database
#'
#' The \strong{\code{road_get_organic_tools}} fetches data of organic tools from 
#' ROAD database.Organic tools are artifacts made from organic materials such as 
#' bone, antler, or wood, found in archaeological contexts. This function enables 
#' you to query organic tool data from the ROAD database based on parameters like 
#' geographical location, cultural periods, tool interpretation, and assemblages. 
#' Use the parameters to filter the results or omit them for broader results.
#'
#' @param continent specifies the continent(s) (e.g. Africa, Europe, Asia).
#' Run \code{road_list_argument_values("continent")} to display possible values.
#' The argument \code{continent} is a string (one item) or vector of strings 
#' (one or more items); defaults to NULL.
#' @param subcontinent specifies the continental region(s) (e.g. Southern Europe). 
#' Run \code{road_list_argument_values("subcontinent")} to display possible values.
#' The argument \code{subcontinent} is a string (one item) or vector of strings 
#' (one or more items); defaults to NULL.
#' @param country specifies the name of the country where a locality is situated 
#' (e.g. Germany, Kenya, Saudi Arabia). Run \code{road_list_argument_values("country")} 
#' to display possible values.
#' The argument \code{country} is a string (one item) or vector of strings 
#' (one or more items); defaults to NULL.
#' @param locality_type specifies the type of locality (e.g. cave, rockshelter, open air).
#' Run \code{road_list_argument_values("locality_type")} to display possible values.
#' The argument \code{locality_type} is a string (one item) or vector of strings 
#' (one or more items); defaults to NULL.
#' @param cultural_periods specifies the main cultural epoch(s) and includes the 
#' Eurasian Paleolithic (Lower, Middle, Upper, Epi-) and the African Stone Age 
#' (Earlier, Middle, Later). Run \code{road_list_argument_values("cultural_periods")} 
#' to display possible values. The argument \code{cultural_periods} is a string 
#' (one item) or vector of strings (one or more items); defaults to NULL.
#' @param technocomplexes specifies an archaeological culture or named stone tool 
#' industry (e.g. Oldowan, Acheulean, Mousterian).
#' Run \code{road_list_argument_values("technocomplexes")} to display possible values.
#' The argument \code{technocomplexes} is a string (one item) or vector of strings 
#' (one or more items); defaults to NULL.
#' @param category specifies the assemblage category with the classes 
#' human remains, raw material, typology, technology, function, organic tools, 
#' symbolic artifacts, feature, miscellaneous finds, paleofauna, animal remains, 
#' plant remains. The argument \code{category} is a string (one item) or 
#' vector of strings (one or more items); defaults to NULL.
#' @param age_min specifies the minimum age in years before present, using 1950 CE
#' as the baseline. If possible the argument \code{age_min} will be converted to an integer; defaults to NULL.
#' @param age_max specifies the maximum age in years before present, using 1950 CE
#' as the baseline. If possible the argument \code{age_max} will be converted to an integer; defaults to NULL.
#' @param assemblages specifies a data frame necessarily containing columns 
#' locality_id, assemblage_id. It can be  generated as return value of the 
#' function 'road_get_assemblages'. It can be used instead of the locality 
#' and assemblage search parameters to filter the results.
#' @param organic_tools_interpretation specifies interpreted organic tool types 
#' (e.g. lance/spear, point, retoucher). 
#' Run \code{road_list_argument_values("organic_tools_interpretation")} 
#' to display possible values. The argument \code{organic_tools_interpretation} 
#' is a string (one item) or vector of strings; defaults to NULL.
#'
#' @return Database search result as a data frame with the information about 
#' organic tool finds. Rows represent organic tool finds and contain standard 
#' information like locality name, geographic information, cultural period, 
#' locality type, assemblage category, dating and organic tools-related details:
#' @return \code{organic tools interpretation}: List of organic tool types of 
#' the given record.
#' @return \code{organic raw material}: Material from which an organic tool is made.
#' @return \code{organic tools technology}: List of actions used to manufacture 
#' organic tools of the given record.
#' @return \code{number}: Number of individual pieces for the organic raw 
#' material in the assemblage.
#' @export
#'
#' @examples
#' road_get_organic_tools(continent = c("Europe"), organic_tools_interpretation = "fishhook")
#' road_get_organic_tools(continent = "Africa", organic_tools_interpretation = c("fishhook"))
road_get_organic_tools <- function(
    continent = NULL,
    subcontinent = NULL,
    country = NULL,
    locality_type = NULL,
    cultural_periods = NULL,
    technocomplexes = NULL,
    category = NULL,
    age_min = NULL,
    age_max = NULL,
    organic_tools_interpretation = NULL,
    assemblages = NULL
)
{

  # calculate assemblage_condition
  # To do: !is.null(category) AND !is.null(assemblages)  ---> Warnung an den Benutzer
  if (is.null(assemblages)) assemblages <- road_get_assemblages(continent = continent, 
                                                                subcontinent = subcontinent, 
                                                                country = country, 
                                                                locality_type = locality_type, 
                                                                cultural_periods = cultural_periods, 
                                                                technocomplexes = technocomplexes, 
                                                                category = category, 
                                                                age_min = age_min, 
                                                                age_max = age_max)
  assemblage_condition <- get_assemblage_condition(assemblages = assemblages, locality_id_column_name = "organic_tools.assemblage_idlocality", assemblage_id_column_name = "organic_tools.assemblage_idassemblage")

  # select fields
  select_fields <- c(
    paste0("assemblage_idlocality AS ", cm_locality_idlocality),
    paste0("assemblage_idassemblage AS ", cm_assemblages_idassemblage),
    paste0("interpretation AS ", cm_organic_tools_interpretation),
    paste0("organic_raw_material AS ", cm_organic_raw_material),
    paste0("technology AS ", cm_organic_tools_technology),
    "number",
    "comments"
  )

  query <- paste(
    "SELECT DISTINCT",
    paste(select_fields, collapse = ", "),
    "FROM organic_tools",
    "WHERE",
    assemblage_condition,
    query_values_in_string("AND ", organic_tools_interpretation, "interpretation")
  )

  data <- road_run_query(query)
  
  if (nrow(data) == 0 && nrow(assemblages) > 0)
  {
    
    print_null_result_message(continent = continent,
                              subcontinent = subcontinent,
                              country = country,
                              locality_type = locality_type,
                              cultural_periods = cultural_periods,
                              technocomplexes = technocomplexes,
                              category = category,
                              age_min = age_min,
                              age_max = age_max,
                              organic_tools_interpretation = organic_tools_interpretation)
  }

  data <- add_locality_columns(data, assemblages = assemblages)

  return(data)
}


#' Get symbolic artifacts from ROAD database
#'
#' The \strong{\code{road_get_symbolic_artifacts}} fetches data of symbolic artifacts from ROAD database.
#' Symbolic artifacts are objects interpreted as having symbolic or cultural significance
#' in archaeological contexts. This function allows you to query symbolic artifact
#' data from the ROAD database using parameters such as geographical location,
#' cultural periods, artifact interpretation, and assemblages. Use the parameters
#' to filter the results or omit them to retrieve a broader dataset.
#'
#' @param continent specifies the continent(s) (e.g. Africa, Europe, Asia).
#' Run \code{road_list_argument_values("continent")} to display possible values.
#' The argument \code{continent} is a string (one item) or vector of strings 
#' (one or more items); defaults to NULL.
#' @param subcontinent specifies the continental region(s) (e.g. Southern Europe). 
#' Run \code{road_list_argument_values("subcontinent")} to display possible values.
#' The argument \code{subcontinent} is a string (one item) or vector of strings 
#' (one or more items); defaults to NULL.
#' @param country specifies the name of the country where a locality is situated 
#' (e.g. Germany, Kenya, Saudi Arabia). Run \code{road_list_argument_values("country")} 
#' to display possible values.
#' The argument \code{country} is a string (one item) or vector of strings 
#' (one or more items); defaults to NULL.
#' @param locality_type specifies the type of locality (e.g. cave, rockshelter, open air).
#' Run \code{road_list_argument_values("locality_type")} to display possible values.
#' The argument \code{locality_type} is a string (one item) or vector of strings 
#' (one or more items); defaults to NULL.
#' @param cultural_periods specifies the main cultural epoch(s) and includes the 
#' Eurasian Paleolithic (Lower, Middle, Upper, Epi-) and the African Stone Age 
#' (Earlier, Middle, Later). Run \code{road_list_argument_values("cultural_periods")} 
#' to display possible values. The argument \code{cultural_periods} is a string 
#' (one item) or vector of strings (one or more items); defaults to NULL.
#' @param technocomplexes specifies an archaeological culture or named stone tool 
#' industry (e.g. Oldowan, Acheulean, Mousterian).
#' Run \code{road_list_argument_values("technocomplexes")} to display possible values.
#' The argument \code{technocomplexes} is a string (one item) or vector of strings 
#' (one or more items); defaults to NULL.
#' @param category specifies the assemblage category with the classes 
#' human remains, raw material, typology, technology, function, organic tools, 
#' symbolic artifacts, feature, miscellaneous finds, paleofauna, animal remains, 
#' plant remains. The argument \code{category} is a string (one item) or 
#' vector of strings (one or more items); defaults to NULL.
#' @param age_min specifies the minimum age in years before present, using 1950 CE
#' as the baseline. If possible the argument \code{age_min} will be converted to an integer; defaults to NULL.
#' @param age_max specifies the maximum age in years before present, using 1950 CE
#' as the baseline. If possible the argument \code{age_max} will be converted to an integer; defaults to NULL.
#' @param assemblages specifies a data frame necessarily containing columns 
#' locality_id, assemblage_id. It can be  generated as return value of the 
#' function 'road_get_assemblages'. It can be used instead of the locality 
#' and assemblage search parameters to filter the results.
#' @param symbolic_artifacts_interpretation specifies the interpretation of 
#' symbolic artifacts (e.g. abstract, anthropomorphic, zoomorphic, instrument, 
#' ornament). Run \code{road_list_argument_values("symbolic_artifacts_interpretation")} 
#' to display possible values. The argument \code{symbolic_artifacts_interpretation} 
#' is a string (one item) or vector of strings; defaults to NULL.
#'
#' @return Database search result as a data frame with the information about 
#' symbolic artifact finds. Rows represent symbolic artifact finds and contain standard 
#' information like locality name, geographic information, cultural period, 
#' locality type, assemblage category, dating and symbolic artifacts-related details:
#' interpretation
#' @return \code{symbolic artifacts category}: List of symbolic categories 
#' (art, music, ornament).  
#' @return \code{symbolic artifacts technology}: List of technologies such as: 
#' painting, engraving, carving, molding, polishing, imprinting, etc.
#' @return \code{symbolic artifacts material}: Material of the symbolic artifact, 
#' for example: antler, bone, cave wall, clay, ivory, ochre, ostrich eggshell.
#' @return \code{symbolic artifacts raw material source}: List with specific 
#' intervals of transport for the raw material used to manufacture the symbolic 
#' artifact. Five fixed types of raw material source are possible:
#' \itemize{
#'   \item local (0-5 km) 
#'   \item regional (6-20 km)
#'   \item supra-regional (21-100 km)
#'   \item distant (>100 km)
#'   \item unknown
#' }
#' @export
#'
#' @examples
#' road_get_symbolic_artifacts(symbolic_artifacts_interpretation = c("instrument"))
#' road_get_symbolic_artifacts(continent = "Africa", symbolic_artifacts_interpretation = "zoomorphic")
road_get_symbolic_artifacts <- function(
    continent = NULL,
    subcontinent = NULL,
    country = NULL,
    locality_type = NULL,
    cultural_periods = NULL,
    technocomplexes = NULL,
    category = NULL,
    age_min = NULL,
    age_max = NULL,
    symbolic_artifacts_interpretation = NULL,
    assemblages = NULL
) {
  # calculate assemblage_condition
  # To do: !is.null(category) AND !is.null(assemblages)  ---> Warnung an den Benutzer
  if (is.null(assemblages)) assemblages <- road_get_assemblages(continent = continent, 
                                                                subcontinent = subcontinent, 
                                                                country = country, 
                                                                locality_type = locality_type, 
                                                                cultural_periods = cultural_periods, 
                                                                technocomplexes = technocomplexes, 
                                                                category = category, 
                                                                age_min = age_min, 
                                                                age_max = age_max)
  assemblage_condition <- get_assemblage_condition(assemblages = assemblages, 
                                                   locality_id_column_name = "symbolic_artifacts.assemblage_idlocality", 
                                                   assemblage_id_column_name = "symbolic_artifacts.assemblage_idassemblage")

  # select fields
  select_fields <- c(
    paste0("assemblage_idlocality AS ", cm_locality_idlocality),
    paste0("assemblage_idassemblage AS ", cm_assemblages_idassemblage),
    paste0("interpretation AS ", cm_symbolic_artifacts_interpretation),
    paste0("category AS ", cm_symbolic_artifacts_category),
    paste0("material AS ", cm_symbolic_artifacts_material),
    paste0("technology AS ", cm_symbolic_artifacts_technology),
    paste0("raw_material_source AS ", cm_symbolic_artifacts_raw_material_source),
    "comments"
  )

  query <- paste(
    "SELECT DISTINCT",
    paste(select_fields, collapse = ", "),
    "FROM symbolic_artifacts",
    "WHERE",
    assemblage_condition,
    query_values_in_string("AND ", symbolic_artifacts_interpretation, "interpretation")
  )

  data <- road_run_query(query)
  
  if (nrow(data) == 0 && nrow(assemblages) > 0)
  {
    
    print_null_result_message(continent = continent,
                              subcontinent = subcontinent,
                              country = country,
                              locality_type = locality_type,
                              cultural_periods = cultural_periods,
                              technocomplexes = technocomplexes,
                              category = category,
                              age_min = age_min,
                              age_max = age_max,
                              symbolic_artifacts_interpretation = symbolic_artifacts_interpretation
    )
  }

  data <- add_locality_columns(data, assemblages = assemblages)

  return(data)
}


#' Get feature assemblages from ROAD database
#'
#' The \strong{\code{road_get_feature}} fetches data of feature finds from ROAD database. Feature assemblages refer to archaeological features such as hearths, pits, or structures found at a site.
#' This function enables you to query feature data from the ROAD database using parameters like geographical location,
#' cultural periods, feature interpretation, and assemblages. Use the parameters to filter the results or omit them for broader results.
#'
#' @param continent specifies the continent(s) (e.g. Africa, Europe, Asia).
#' Run \code{road_list_argument_values("continent")} to display possible values.
#' The argument \code{continent} is a string (one item) or vector of strings 
#' (one or more items); defaults to NULL.
#' @param subcontinent specifies the continental region(s) (e.g. Southern Europe). 
#' Run \code{road_list_argument_values("subcontinent")} to display possible values.
#' The argument \code{subcontinent} is a string (one item) or vector of strings 
#' (one or more items); defaults to NULL.
#' @param country specifies the name of the country where a locality is situated 
#' (e.g. Germany, Kenya, Saudi Arabia). Run \code{road_list_argument_values("country")} 
#' to display possible values.
#' The argument \code{country} is a string (one item) or vector of strings 
#' (one or more items); defaults to NULL.
#' @param locality_type specifies the type of locality (e.g. cave, rockshelter, open air).
#' Run \code{road_list_argument_values("locality_type")} to display possible values.
#' The argument \code{locality_type} is a string (one item) or vector of strings 
#' (one or more items); defaults to NULL.
#' @param cultural_periods specifies the main cultural epoch(s) and includes the 
#' Eurasian Paleolithic (Lower, Middle, Upper, Epi-) and the African Stone Age 
#' (Earlier, Middle, Later). Run \code{road_list_argument_values("cultural_periods")} 
#' to display possible values. The argument \code{cultural_periods} is a string 
#' (one item) or vector of strings (one or more items); defaults to NULL.
#' @param technocomplexes specifies an archaeological culture or named stone tool 
#' industry (e.g. Oldowan, Acheulean, Mousterian).
#' Run \code{road_list_argument_values("technocomplexes")} to display possible values.
#' The argument \code{technocomplexes} is a string (one item) or vector of strings 
#' (one or more items); defaults to NULL.
#' @param category specifies the assemblage category with the classes 
#' human remains, raw material, typology, technology, function, organic tools, 
#' symbolic artifacts, feature, miscellaneous finds, paleofauna, animal remains, 
#' plant remains. The argument \code{category} is a string (one item) or 
#' vector of strings (one or more items); defaults to NULL.
#' @param age_min specifies the minimum age in years before present, using 1950 CE
#' as the baseline. If possible the argument \code{age_min} will be converted to an integer; defaults to NULL.
#' @param age_max specifies the maximum age in years before present, using 1950 CE
#' as the baseline. If possible the argument \code{age_max} will be converted to an integer; defaults to NULL.
#' @param assemblages specifies a data frame necessarily containing columns 
#' locality_id, assemblage_id. It can be  generated as return value of the 
#' function 'road_get_assemblages'. It can be used instead of the locality 
#' and assemblage search parameters to filter the results.
#' @param feature_interpretation specifies archaeological features present in 
#' the archaeological assemblage (e.g. bedding, burial, butchering event).
#' Run \code{road_list_argument_values("feature_interpretation")} to display 
#' possible values. The argument \code{feature_interpretation} is a string 
#' (one item) or vector of strings; defaults to NULL.
#'
#' @return Database search result as a data frame with the information about 
#' archaeological finds. Rows represent archaeological finds and contain standard 
#' information like locality name, geographic information, cultural period, 
#' locality type, assemblage category, dating and feature-related details:
#' @return \code{interpretations}: Interpretation of the feature present in the 
#' archaeological assemblage. Interpretations can be bedding, burial, butchering 
#' event, combustion feature, cupule, dumping area.
#' @export
#'
#' @examples
#' road_get_feature(continent = c("Europe", "Africa"), feature_interpretation = "textile imprints")
#' road_get_feature(continent = "Africa", locality_type = c("cave"),
#'                  feature_interpretation = "bedding")
road_get_feature <- function(
    continent = NULL,
    subcontinent = NULL,
    country = NULL,
    locality_type = NULL,
    cultural_periods = NULL,
    technocomplexes = NULL,
    category = NULL,
    age_min = NULL,
    age_max = NULL,
    feature_interpretation = NULL,
    assemblages = NULL
) {
  # calculate assemblage_condition
  # To do: !is.null(category) AND !is.null(assemblages)  ---> Warnung an den Benutzer
  if (is.null(assemblages)) assemblages <- road_get_assemblages(continent = continent, 
                                                                subcontinent = subcontinent, 
                                                                country = country, 
                                                                locality_type = locality_type, 
                                                                cultural_periods = cultural_periods, 
                                                                technocomplexes = technocomplexes, 
                                                                category = category, 
                                                                age_min = age_min, 
                                                                age_max = age_max)
  assemblage_condition <- get_assemblage_condition(assemblages = assemblages, locality_id_column_name = "feature.assemblage_idlocality", assemblage_id_column_name = "feature.assemblage_idassemblage")

  # select fields
  select_fields <- c(
    paste0("assemblage_idlocality AS ", cm_locality_idlocality),
    paste0("assemblage_idassemblage AS ", cm_assemblages_idassemblage),
    paste0("interpretation AS ", cm_feature_interpretation),
    "comments"
  )

  query <- paste(
    "SELECT DISTINCT",
    paste(select_fields, collapse = ", "),
    "FROM feature",
    "WHERE",
    assemblage_condition,
    parameter_to_query("AND interpretation IN (", feature_interpretation, ")")
  )

  data <- road_run_query(query)
  
  if (nrow(data) == 0 && nrow(assemblages) > 0)
  {
    
    print_null_result_message(continent = continent,
                              subcontinent = subcontinent,
                              country = country,
                              locality_type = locality_type,
                              cultural_periods = cultural_periods,
                              technocomplexes = technocomplexes,
                              category = category,
                              age_min = age_min,
                              age_max = age_max,
                              feature_interpretation = feature_interpretation)
  }
  
  data <- add_locality_columns(data, assemblages = assemblages)

  return(data)
}

#' Get miscellaneous finds from ROAD database
#'
#' The \strong{\code{road_get_miscellaneous_finds}} fetches data of feature finds from ROAD database.
#' Miscellaneous finds are archaeological objects that do not fit into other specific categories.
#' Miscellaneous finds are classified by their material.
#' This function allows you to query miscellaneous finds data from the ROAD database
#' using parameters such as geographical location,
#' cultural periods, material types, and assemblages. Use the parameters to filter the results
#' or omit them to retrieve a broader dataset.
#'
#' @param continent specifies the continent(s) (e.g. Africa, Europe, Asia).
#' Run \code{road_list_argument_values("continent")} to display possible values.
#' The argument \code{continent} is a string (one item) or vector of strings 
#' (one or more items); defaults to NULL.
#' @param subcontinent specifies the continental region(s) (e.g. Southern Europe). 
#' Run \code{road_list_argument_values("subcontinent")} to display possible values.
#' The argument \code{subcontinent} is a string (one item) or vector of strings 
#' (one or more items); defaults to NULL.
#' @param country specifies the name of the country where a locality is situated 
#' (e.g. Germany, Kenya, Saudi Arabia). Run \code{road_list_argument_values("country")} 
#' to display possible values.
#' The argument \code{country} is a string (one item) or vector of strings 
#' (one or more items); defaults to NULL.
#' @param locality_type specifies the type of locality (e.g. cave, rockshelter, open air).
#' Run \code{road_list_argument_values("locality_type")} to display possible values.
#' The argument \code{locality_type} is a string (one item) or vector of strings 
#' (one or more items); defaults to NULL.
#' @param cultural_periods specifies the main cultural epoch(s) and includes the 
#' Eurasian Paleolithic (Lower, Middle, Upper, Epi-) and the African Stone Age 
#' (Earlier, Middle, Later). Run \code{road_list_argument_values("cultural_periods")} 
#' to display possible values. The argument \code{cultural_periods} is a string 
#' (one item) or vector of strings (one or more items); defaults to NULL.
#' @param technocomplexes specifies an archaeological culture or named stone tool 
#' industry (e.g. Oldowan, Acheulean, Mousterian).
#' Run \code{road_list_argument_values("technocomplexes")} to display possible values.
#' The argument \code{technocomplexes} is a string (one item) or vector of strings 
#' (one or more items); defaults to NULL.
#' @param category specifies the assemblage category with the classes 
#' human remains, raw material, typology, technology, function, organic tools, 
#' symbolic artifacts, feature, miscellaneous finds, paleofauna, animal remains, 
#' plant remains. The argument \code{category} is a string (one item) or 
#' vector of strings (one or more items); defaults to NULL.
#' @param age_min specifies the minimum age in years before present, using 1950 CE
#' as the baseline. If possible the argument \code{age_min} will be converted to an integer; defaults to NULL.
#' @param age_max specifies the maximum age in years before present, using 1950 CE
#' as the baseline. If possible the argument \code{age_max} will be converted to an integer; defaults to NULL.
#' @param assemblages specifies a data frame necessarily containing columns 
#' locality_id, assemblage_id. It can be  generated as return value of the 
#' function 'road_get_assemblages'. It can be used instead of the locality 
#' and assemblage search parameters to filter the results.
#' @param miscellaneous_finds_material specifies material of the miscellaneous 
#' finds (e.g. shell, ochre, ostrich eggshell).
#' Run \code{road_list_argument_values("miscellaneous_finds_material")} to 
#' display possible values. The argument \code{miscellaneous_finds_material} is 
#' a string (one item) or vector of strings; defaults to NULL.
#'
#' @return Database search result as a data frame with the information about 
#' miscellaneous finds. Rows represent miscellaneous finds and contain 
#' standard information like locality name, geographic information, cultural 
#' period, locality type, assemblage category, dating and miscellaneous 
#' finds-related details
#' @return \code{miscellaneous finds material}: Material of the miscellaneous 
#' find. Some examples: beeswax, bitumen, clay, flax fiber, fossil, mineral 
#' diverse (materials such as crystals), mineral pigment (colorants such as 
#' ochre, hematite, limonite, goethite, specularite, etc.) ostrich eggshell
#' @return \code{miscellaneous finds material source}: List with specific 
#' intervals of transport for the raw material used to manufacture the 
#' miscellaneous finds. Five fixed types of material source are possible:
#' \itemize{
#'   \item local (0-5 km) 
#'   \item regional (6-20 km)
#'   \item supra-regional (21-100 km)
#'   \item distant (>100 km)
#'   \item unknown
#' }
#' @return \code{number}: Number of individual pieces of a given material present
#'  in an assemblage with miscellaneous finds.
#' @export
#'
#' @examples
#' road_get_miscellaneous_finds(miscellaneous_finds_material = "wood fossil")
#' road_get_miscellaneous_finds(continent = c("Africa"), locality_type = "open air",
#'                              miscellaneous_finds_material = "shell")
road_get_miscellaneous_finds <- function(
    continent = NULL,
    subcontinent = NULL,
    country = NULL,
    locality_type = NULL,
    cultural_periods = NULL,
    technocomplexes = NULL,
    category = NULL,
    age_min = NULL,
    age_max = NULL,
    miscellaneous_finds_material = NULL,
    assemblages = NULL
) {
  # calculate assemblage_condition
  # To do: !is.null(category) AND !is.null(assemblages)  ---> Warnung an den Benutzer
  if (is.null(assemblages)) assemblages <- road_get_assemblages(continent = continent, 
                                                                subcontinent = subcontinent, 
                                                                country = country, 
                                                                locality_type = locality_type, 
                                                                cultural_periods = cultural_periods, 
                                                                technocomplexes = technocomplexes, 
                                                                category = category, 
                                                                age_min = age_min, 
                                                                age_max = age_max)
  assemblage_condition <- get_assemblage_condition(assemblages = assemblages, locality_id_column_name = "miscellaneous_finds.assemblage_idlocality", assemblage_id_column_name = "miscellaneous_finds.assemblage_idassemblage")

  # select fields
  select_fields <- c(
    paste0("assemblage_idlocality AS ", cm_locality_idlocality),
    paste0("assemblage_idassemblage AS ", cm_assemblages_idassemblage),
    paste0("material AS ", cm_miscellaneous_finds_material),
    paste0("raw_material_source AS ", cm_miscellaneous_finds_raw_material_source),
    "number",
    "comments"
  )

  query <- paste(
    "SELECT DISTINCT",
    paste(select_fields, collapse = ", "),
    "FROM miscellaneous_finds",
    "WHERE",
    assemblage_condition,
    parameter_to_query("AND material IN (", miscellaneous_finds_material, ")")
  )

  data <- road_run_query(query)
  
  if (nrow(data) == 0 && nrow(assemblages) > 0)
  {
    
    print_null_result_message(continent = continent,
                              subcontinent = subcontinent,
                              country = country,
                              locality_type = locality_type,
                              cultural_periods = cultural_periods,
                              technocomplexes = technocomplexes,
                              category = category,
                              age_min = age_min,
                              age_max = age_max,
                              miscellaneous_finds_material = miscellaneous_finds_material)
  }
  
  data <- add_locality_columns(data, assemblages = assemblages)

  return(data)
}
