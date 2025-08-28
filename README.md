# roadDB  <a href="http://roceeh.net"><img src="docs/logo.png" align="right" height="138" /></a>
The aim of the package `roadDB` is to provide access to the [ROCEEH Out of Africa Database (ROAD)](https://www.roceeh.uni-tuebingen.de/roadweb/smarty_road_simple_search.php) and supply users with dataframes for further analysis in the R ecosystem.

## Structure
The roadDB package has three main levels of detail (LOD) that follow a hierarchical order: Locality, Assemblage and Date. A locality can have multiple assemblages, and each assemblage can have multiple dates associated with it.

<p align="center">
<img src="docs/levels_of_detail.svg" alt="Illustration of the three levels of the roadDB R-package from top to bottom: Locality, Assemblage and Date" height="250">
</p>

Users can query information at different LODs using dedicated functions that follow the `road_get_*` naming convention. These return dataframes where each row represents an item at the requested granularity and includes attribute columns relevant to those items.

An extensive set of arguments can be applied to all `road_get_*` functions, allowing users to refine their queries and tailor the results to their needs.

As the ROAD database is exceptionally rich in information at the assemblage level, there are subordinate functions for querying human remains, archaeology, palaeofauna and palaeobotany.


## Functions
- road_get_localities()
- road_get_assemblages()
	- road_get_human_remains()
	- road_get_paleofauna()
	- road_get_paleobotany()
	- archaeology-related:
		- road_get_lithic_typology()
		- road_get_lithic_raw_material()
		- road_get_organic_tools()
		- road_get_symbolic_artifacts()
		- road_get_feature()
		- road_get_miscellaneous_finds()
- road_get_dates()

### Arguments
The following arguments are optional and can be used with every `road_get_*` function to constrain queries.

| Argument                            | Type      | ROAD table / attribute                               | Example                                       |
| ----------------------------------- | --------- | ---------------------------------------------------- | --------------------------------------------- |
| `continents`                        | character | country_continent / continent                        | "Africa"                                      |
| `subcontinents`                     | character | country_continent / region                           | "Southern Africa"                             |
| `countries`                         | character | locality / country                                   | "South Africa"                                |
| `locality_types`                    | character | locality / type                                      | "rock shelter"                                |
| `categories`                        | character | assemblage / category                                | "Sibhudu Cave DMou Archaeological Assemblage" |
| `age_min`                           | numeric   | archaeological_stratigraphy / age_min                | 20000                                         |
| `age_max`                           | numeric   | archaeological_stratigraphy / age_max                | 3000000                                       |
| `technocomplexes`                   | character | archaeological_stratigraphy /technocomplex           | "ESA/ Early Acheulean"                         |
| `cultural_periods`                  | character | archaeological_stratigraphy / cultural_period        | "Middle Stone Age"                            |


The following arguments are optional and can be used with the corresponding `road_get_*` function to constrain queries.

| Argument                            | Type      | ROAD table / attribute                               | Example                                       |
| ----------------------------------- | --------- | ---------------------------------------------------- | --------------------------------------------- |
| `tool_list`                         | character |                                                      |                                               |
| `raw_material_list`                 | character |                                                     |                                               |
| `transport_distance`                | character | transport_distance                                   |  "regional (6-20 km)"                         |
| `organic_tools_interpretation`      | character |                                                      |                                               |
| `symbolic_artifacts_interpretation` | character |                                                      |                                               |
| `feature_interpretation`            | character |                                                      |                                               |
| 'human_genus'                       | character |                                                      |                                               |
| 'human_species'                     | character |                                                      |                                               |
| 'plant_remains'                     | character |                                                      |                                               |
| 'plant_family'                      | character |                                                      |                                               |
| 'plant_genus'                       | character |                                                      |                                               |
| 'plant_species'                     | character |                                                      |                                               |
| 'fauna_genus'                       | character |                                                      |                                               |
| 'fauna_species'                     | character |                                                      |                                               |
| xxxxxxxxxxxxxxx                     |           |                                                      |                                               |
