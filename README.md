# roadDB: An R package to query ROAD data with R

## Structure

## Functions
- road_get_localities()
- road_get_assemblages()
	- road_get_human_remains()
	- road_get_archaeology()
	- road_get_paleofauna()
	- road_get_paleobotany()
- road_get_dates()

### Arguments
The following arguments are optional and can be used with every road_get_* function to constraining queries.

| Argument                  | Type      | ROAD table / attribute                               | Example                                       |
| ------------------------- | --------- | ---------------------------------------------------- | --------------------------------------------- |
| `locality`                  | character | locality / idlocality                                | "Sibhudu Cave"                                |
| `continent`                 | character | country_continent / continent                        | "Africa"                                      |
| `subcontinent`              | character | country_continent / region                           | "Southern Africa"                             |
| `country`                   | character | locality / country                                   | "South Africa"                                |
| `region`                    | character | locality / region                                    | "KwaZulu-Natal"                               |
| `locality_type`             | character | locality / type                                      | "rock shelter"                                |
| `assemblage_name`           | character | assemblage / name                                    | "Sibhudu Cave DMou Archaeological Assemblage" |
| `assemblage_id`             | character | assemblage / idassemblage                            | "154"                                         |
| `geolayer`                  | character | geostrat_describes_geolayer / geolayer_name          | "OMOD"                                        |
| `archaeological_layer`      | character | archaeological_stratigraphy / name                   | "LBG"                                         |
| `age_min`                   | numeric   | assemblage_age / age and negative_standard_deviation | 20000                                         |
| `age_max`                   | numeric   | assemblage_age / age and positive_standard_deviation | 3000000                                       |
| `culture`                   | character | archaeological_layer / archstratigraphy_idarchstrat  | "Post-Howiesonspoort"                         |
| `cultural_period`           | character |                                                      | "Middle Stone Age"                            |
| `archaeological_technology` | character |                                                      |                                               |
| `archaeological_typology`   | character |                                                      |                                               |
| `raw_materials`             | character |                                                      |                                               |
| `archaeological_function`   | character |                                                      |                                               |
| `miscalaneous_finds`        | character |                                                      |                                               |
| `organic_tools`             | character |                                                      |                                               |
| `symbolic_artifacts`        | character |                                                      |                                               |
| `archaeological_feature`    | character |                                                      |                                               |
| `plant_remains`             | boolean   |                                                      |                                               |
| `human_remains`             | boolean   |                                                      |                                               |
| human_genus?              | character |                                                      |                                               |
| human species?            | character |                                                      |                                               |
| `plant_remains`             | boolean   |                                                      |                                               |
| plant_species?            | character |                                                      |                                               |
| `paleofauna`                | boolean   |                                                      |                                               |
| faunal_genus?             | character |                                                      |                                               |
| faunal_species?           | character |                                                      |                                               |
| `animal_remains`            | boolean   |                                                      |                                               |
| further arguments?        |           |                                                      |                                               |


## Function: road_locality
	* LOD: Locality
	* Arguments:
		* country (string list)
		* continent (string list)
		* subcontinent (string list)
		* locality_type (string list)
	* Output:
		* locality (string)
		* country (string)
		* continent (string)
		* subcontinent (string)
		* locality_type (string)
		* x (numeric)
		* y (numeric)

## Function: road_assemblage
	* LOD: Assemblage
	* Arguments:
		* country (string list)
		* continent (string list)
		* subcontinent (string list)
		* locality_type (string list)
		* continent (string list)
		* age_min (numeric)
		* age_max (numeric)
	* Output:
		* locality (string)
		* country (string)
		* continent (string)
		* subcontinent (string)
		* locality_type (string)
		* x (numeric)
		* y (numeric)
		* assemblage_name (string)
		* assemblage_id (numeric)
		* technology (boolean)
		* typology (boolean)
		* raw-material (boolean)
		* function (boolean)
		* miscelaneous_finds (boolean)
		* organic_tools (boolean)
		* symbolic_artifacts (boolean)
		* feature (boolean)
		* animal_remains (boolean)
		* paleofauna (boolean)
		* plant_remains (boolean)
		* assemblage_min_aggregated (numeric): minimum of all age_min associated with an assemblage
		* assemblage_max_aggregated (numeric): maximum of all age_max associated with an assemblage
		* assemblage_ages (string list)
		* geolayer (string list)


