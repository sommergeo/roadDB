# An R package for ROAD

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


# Ideas for future functions:
* road_ages is similar to road_assemblages, but contains a row for each date and information on assemblage details are duplicated 
* road_human_remains includes attributes on taxa/genus and Species
* road_archaeology
* road_paleofauna
* road_paleobotany
* Search for locality name and geolayer and return information on assemblage level