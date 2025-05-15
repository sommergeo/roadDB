#' Get assemblages from ROAD database
#'
#' `road_get_assemblages` fetches data of archeological assemblages from ROAD database.
#'
#' Assembalges are articulated archeological finds inside in a locality. One locality
#' can host multiple assemblages which can for example be associated with certain
#' geological layers or historical time periods.
#'
#' @param continents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param subcontinents string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param countries string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param locality_types string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param cultural_periods string (one item) or vector of strings (one or more items); defaults to NULL.
#' @param categories string (one item) or vector of strings (one or more items).
#' @param age_min integer; minimum age of assemblage.
#' @param age_max integer; maximum age of assemblage.
#'
#' @return Database search result as list of assemblages.
#' @export
#'
#' @examples 
#' road_get_assemblages(countries = c("Germany", "France"), age_min = 100000L)
#' road_get_assemblages(categories = "human remains", age_max = 100000L)
#' road_get_assemblages(subcontinents = "Central Asia", cultural_periods = "Middle Paleolithic")
road_get_assemblages <- function(
    continents = NULL,
    subcontinents = NULL,
    countries = NULL,
    locality_types = NULL,
    cultural_periods = NULL,
    categories = NULL,
    age_min = NULL,
    age_max = NULL
)
{
  if ((!is.null(age_min) && !is.integer(age_min)) || (!is.null(age_max) && !is.integer(age_max)))
    stop("Parameters 'min_age' and 'max_age' have to be integers.")

  if (!is.null(age_min) && !is.null(age_max) && age_min > age_max)
    stop("Parameter 'min_age' can not be bigger than 'max_age'.")

  localities <- road_get_localities(continents, subcontinents, countries, locality_types, cultural_periods)

  query_localities <- paste(
    sapply(localities[cm_locality_idlocality], function(x) paste0("'", x, "'")),
    collapse = ", "
  )

  # select fields
  select_fields <- c(
    paste0("assemblage.locality_idlocality AS ", cm_assemblages_locality_idlocality),
    paste0("assemblage.idassemblage AS ", cm_assemblages_idassemblage),
    paste0("assemblage.name AS ", cm_assemblages_name),
    paste0("assemblage.category AS ", cm_assemblages_categories),
    paste0("MIN(geological_stratigraphy.age_min) AS ", cm_geological_stratigraphy_age_min),
    paste0("MAX(geological_stratigraphy.age_max) AS ", cm_geological_stratigraphy_age_max),
    paste0("STRING_AGG(DISTINCT assemblage_in_geolayer.geolayer_name, ', ') AS ", cm_assemblage_in_geolayer_geolayer_name),
    paste0("STRING_AGG(DISTINCT assemblage_in_archlayer.archlayer_name, ', ') AS ", cm_assemblage_in_archlayer_archlayer_name),
    paste0("STRING_AGG(DISTINCT archaeological_stratigraphy.cultural_period, ', ') AS ", cm_cultural_periods)
  )

  # combine query parts
  query <- paste(
    # SELECT
    "SELECT DISTINCT",
    paste(select_fields, collapse = ", "),
    ",",
    "CASE",
    "WHEN (assemblage.locality_idlocality, assemblage.idassemblage) IN (SELECT assemblage_idlocality, assemblage_idassemblage FROM humanremains) 
       THEN true",
    "ELSE false",
    "END AS humanremains,",
    "CASE",
    "WHEN category LIKE '%paleofauna%' THEN true",
    "ELSE false",
    "END AS paleofauna,",
    "CASE",
    "WHEN category ~ 'raw material|symbolic artifacts|technology|typology|miscellaneous finds|feature|organic tools|function' THEN true",
    "ELSE false",
    "END AS archaeology,",
    "CASE",
    "WHEN category LIKE '%plant remains%' THEN true",
    "ELSE false",
    "END AS plantremains",
    # FROM
    "FROM assemblage",
    "LEFT JOIN assemblage_in_geolayer ON",
    "assemblage.locality_idlocality = assemblage_in_geolayer.assemblage_idlocality",
    "AND assemblage.idassemblage = assemblage_in_geolayer.assemblage_idassemblage",
    "LEFT JOIN geostrat_desc_geolayer ON",
    "assemblage.locality_idlocality = geostrat_desc_geolayer.geolayer_idlocality",
    "AND geostrat_desc_geolayer.geolayer_name = assemblage_in_geolayer.geolayer_name",
    "LEFT JOIN geological_stratigraphy ON",
    "geostrat_desc_geolayer.geostrat_idgeostrat = geological_stratigraphy.idgeostrat",
    "LEFT JOIN assemblage_in_archlayer ON",
    "assemblage.locality_idlocality = assemblage_in_archlayer.assemblage_idlocality",
    "AND assemblage.idassemblage = assemblage_in_archlayer.assemblage_idassemblage",
    "LEFT JOIN archaeological_layer ON",
    "assemblage_in_archlayer.archlayer_idlocality = archaeological_layer.locality_idlocality",
    "AND assemblage_in_archlayer.archlayer_name = archaeological_layer.name",
    "LEFT JOIN archaeological_stratigraphy ON",
    "archaeological_layer.archstratigraphy_idarchstrat = archaeological_stratigraphy.idarchstrat",
    # WHERE
    "WHERE assemblage.locality_idlocality IN (", query_localities, ")",
    query_check_intersection("AND ", categories, "assemblage.category"),
    parameter_to_query("AND ", age_min, " <= geological_stratigraphy.age_max"),
    parameter_to_query("AND ", age_max, " >= geological_stratigraphy.age_min"),
    parameter_to_query(
      "AND (assemblage.locality_idlocality, assemblage.idassemblage) IN (
        SELECT DISTINCT assemblage_in_archlayer.assemblage_idlocality, assemblage_in_archlayer.assemblage_idassemblage
        FROM archaeological_layer
        LEFT JOIN assemblage_in_archlayer ON
          assemblage_in_archlayer.archlayer_idlocality = archaeological_layer.locality_idlocality
          AND archaeological_layer.name = assemblage_in_archlayer.archlayer_name
        LEFT JOIN archaeological_stratigraphy ON
          archaeological_layer.archstratigraphy_idarchstrat = archaeological_stratigraphy.idarchstrat
        WHERE archaeological_stratigraphy.cultural_period IN (", cultural_periods, "))"
    ),
    # GROUP and ORDER
    "GROUP BY assemblage.locality_idlocality, assemblage.idassemblage, assemblage.name, assemblage.category, geological_stratigraphy.age_min, geological_stratigraphy.age_max",
    "ORDER BY assemblage.locality_idlocality ASC, assemblage.idassemblage ASC"
  )

  data <- road_run_query(query)
  
  if (nrow(data) == 0 & nrow(localities) > 0)
  {
    categories_str <- ifelse(is.null(categories), "", paste("categories =", toString(categories)))
    age_min_str <- ifelse(is.null(age_min), "", paste("age_min =", age_min))
    age_max_str <- ifelse(is.null(age_max), "", paste("age_max =", age_max))
    
    message(paste("One or more of the following used parameters caused the empty result set
                  :",
                  categories_str,
                  age_min_str,
                  age_max_str,
                  "
Please keep in mind, the data search needs exact parameter values. To get exact values for a given parameter 'p' you can use the function road_list_parameter_values('p')."))
  }
  
  data <- add_locality_columns(data, localities = localities)

  return(data)
}