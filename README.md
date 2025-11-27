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
### Core functions
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

### Helper functions
- road_list_argument_values()
- road_summarize_archaeology()

### Arguments
The following arguments are optional and can be used with every `road_get_*` function to constrain queries.

| Argument                            | Type      | ROAD table / attribute                               | Search type | Example                                       |
| ----------------------------------- | --------- | ---------------------------------------------------- | ----------- | --------------------------------------------- |
| `continent`                         | character | country_continent / continent                        | exact       | "Africa"                                      |
| `subcontinent`                      | character | country_continent / region                           | exact       | "Southern Africa"                             |
| `country`                           | character | locality / country                                   | exact       | "South Africa"                                |
| `locality_type`                     | character | locality / type                                      | exact       | "rock shelter"                                |
| `category`                          | character | assemblage / category                                | exact       | "symbolic artifacts, typology"                |
| `age_min`                           | numeric   | archaeological_stratigraphy / age_min                | exact       | 20000                                         |
| `age_max`                           | numeric   | archaeological_stratigraphy / age_max                | exact       | 3000000                                       |
| `technocomplex`                     | character | archaeological_stratigraphy /technocomplex           | exact       | "ESA/ Early Acheulean"                        |
| `cultural_period`                   | character | archaeological_stratigraphy / cultural_period        | exact       | "Middle Stone Age"                            |


The following arguments are optional and can be used with the corresponding `road_get_*` function to constrain queries.

| Argument                            | Type      | ROAD table / attribute                               | Search type | Example(s)                                    |
| ----------------------------------- | --------- | ---------------------------------------------------- | ----------- | --------------------------------------------- |
| `tool_list`                         | character | typology / tool_list                                 | contains    |  "core 29, bladelet 136, blade 1090"          |  
| `raw_material_list`                 | character | raw_material / raw_material_list                     | contains    |  "ironstone banded"                           |
| `transport_distance`                | character | raw_material / transport_distance                    | exact       |  "regional (6-20 km)"                         |
| `organic_tool_interpretation`       | character | organic_tools / interpretation                       | contains    |  "harpoon", "worked", "retoucher"             |
| `symbolic_artifact_interpretation`  | character | symbolic_artifacts / interpretation                  | contains    |  "anthropomorphic"                            |
| `feature_interpretation`            | character | feature / interpretation                             | exact       |  "stone construction"                         |
| `miscellaneous_finds_material`      | character | miscellaneous_finds / material                       | exact       |  "ostrich egg shell", "metal"                 |
| `human_genus`                       | character | publication_desc_humanremains / genus                | exact       |  "Homo", "Paranthropus"                       |
| `human_species`                     | character | publication_desc_humanremains / species              | exact       |  "sapiens rhodesiensis", "cf. sapiens"        |
| `plant_remains`                     | character | plantremains / plant_remains                         | exact       |  "phytoliths", "plant macroremains"           |
| `plant_family`                      | character | plant_taxonomy / family                              | exact       |  "Anarcadiaceae",    "Phyllanthaceae"         |
| `plant_genus`                       | character | plant_taxonomy / genus                               | exact       |  "Jasione", "Larix/Picea"                     |
| `plant_species`                     | character | plant_taxonomy / species                             | exact       |  "Potamogeton gramineus L."                   |
| `fauna_genus`                       | character | taxonomical_classification / genus                   | exact       |  "Lemniscomys", "Hipposideros"                |
| `fauna_species`                     | character | paleofauna / species                                 | exact       |  "cf. germanicus", "atapuerquensis"           |


The following table provides an overview of return attributes.

| Attribute                                 | Funktion                                     | Type                 |
| ----------------------------------------- | -------------------------------------------- | -------------------- |
| `locality_id`                             |  `road_get_*`                                | character            |
| `continent`                               |  `road_get_*`                                | character            |
| `subcontinent`                            |  `road_get_*`                                | character            |
| `country`                                 |  `road_get_*`                                | character            |
| `coord_x`                                 |  `road_get_*`                                | number               |
| `coord_y`                                 |  `road_get_*`                                | number               |
| `locality_type`                           |  `road_get_*`                                | character            |
| `category`                                |  `road_get_*`                                | character            |
| `cultural_period `                        |  `road_get_*`                                | character            |
| `technocomplex`                           |  `road_get_*`                                | character            |
| `subset_age_min`                          |  `road_get_localities`                       | number               |
| `subset_age_max`                          |  `road_get_localities`                       | number               |
| `locality_age_min`                        |  `road_get_localities`                       | number               |
| `locality_age_max`                        |  `road_get_localities`                       | number               |
| `human_remains`                           |  `road_get_assemblages`                      | boolean              |
| `archaeology`                             |  `road_get_assemblages`                      | boolean              |
| `plant_remains`                           |  `road_get_assemblages`                      | boolean              |
| `paleofauna`                              |  `road_get_assemblages`                      | boolean              |
| `age_min`                                 |  `road_get_*` (except `road_get_localities`) | number               |
| `age_max`                                 |  `road_get_*` (except `road_get_localities`) | number               |
| `assemblage_name`                         |  `road_get_*` (except `road_get_localities`) | character            |
| `comment`                                 |   archaeological `road_get_*`                | character            |
| `tool_list`                               |  `road_get_lithic_typology`                  | character            |
| `typology`                                |  `road_get_lithic_typology`                  | character            |
| `percentage`                              |  `road_get_lithic_typology`                  | character            |
| `raw_material_list`                       |  `road_get_lithic_raw_material`              | character            |
| `transport_distance`                      |  `road_get_lithic_raw_material`              | character            |
| `percentage`                              |  `road_get_lithic_raw_material`              | character            |
| `organic_tool_interpretation`             |  `road_get_organic_tools`                    | character            |
| `organic_raw_material`                    |  `road_get_organic_tools`                    | character            |
| `organic_tool_technology`                 |  `road_get_organic_tools`                    | character            |
| `number`                                  |  `road_get_organic_tools`                    | character            |
| `symbolic_artifact_interpretation`        |  `road_get_symbolic_artifacts`               | character            |
| `symbolic_artifact_category`              |  `road_get_symbolic_artifacts`               | character            |
| `symbolic_artifact_material`              |  `road_get_symbolic_artifacts`               | character            |
| `symbolic_artifact_raw_material_source`   |  `road_get_symbolic_artifacts`               | character            |
| `symbolic_artifact_technology`            |  `road_get_symbolic_artifacts`               | character            |
| `feature_interpretation`                  |  `road_get_feature`                          | character            |
| `miscellaneous_find_material`             |  `road_get_miscellaneous_finds`              | character            |
| `miscellaneous_find_raw_material_source`  |  `road_get_miscellaneous_finds`              | character            |
| `number`                                  |  `road_get_miscellaneous_finds`              | number               |
| `human_remains_id`                        |  `road_get_human_remains`                    | number               |
| `human_remains_category`                  |  `road_get_human_remains`                    | character            |
| `genus`                                   |  `road_get_human_remains`                    | character            |
| `species`                                 |  `road_get_human_remains`                    | character            |
| `age`                                     |  `road_get_human_remains`                    | character            |
| `sex`                                     |  `road_get_human_remains`                    | character            |
| `skeletal_element`                        |  `road_get_human_remains`                    | character            |
| `fauna_genus`                             |  `road_get_paleofauna`                       | character            |
| `fauna_species`                           |  `road_get_paleofauna`                       | character            |
| `plant_remains`                           |  `road_get_plantremains`                     | character            |
| `plant_family`                            |  `road_get_plantremains`                     | character            |
| `plant_genus`                             |  `road_get_plantremains`                     | character            |
| `plant_species`                           |  `road_get_plantremains`                     | character            |

