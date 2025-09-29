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

| Argument                            | Type      | ROAD table / attribute                               |Search type| Example                                       |
| ----------------------------------- | --------- | ---------------------------------------------------- | --------- | --------------------------------------------- |
| `continents`                        | character | country_continent / continent                        |exact      | "Africa"                                      |
| `subcontinents`                     | character | country_continent / region                           || "Southern Africa"                             |
| `countries`                         | character | locality / country                                   || "South Africa"                                |
| `locality_types`                    | character | locality / type                                      || "rock shelter"                                |
| `categories`                        | character | assemblage / category                                || "symbolic artifacts, typology"                |
| `age_min`                           | numeric   | archaeological_stratigraphy / age_min                || 20000                                         |
| `age_max`                           | numeric   | archaeological_stratigraphy / age_max                || 3000000                                       |
| `technocomplexes`                   | character | archaeological_stratigraphy /technocomplex           || "ESA/ Early Acheulean"                        |
| `cultural_periods`                  | character | archaeological_stratigraphy / cultural_period        || "Middle Stone Age"                            |


The following arguments are optional and can be used with the corresponding `road_get_*` function to constrain queries.

| Argument                            | Type      | ROAD table / attribute                               | Search type | Example(s)                                    |
| ----------------------------------- | --------- | ---------------------------------------------------- | ----------- | --------------------------------- |
| `dating_methods`                    | character | assemblage_age / dating_method,                      ||  "U series (uranium-thorium) dating",         |
|                                     |           | archaeological_layer_age / dating_method,            ||  "ESR (electron spin resonance) dating",      |
|                                     |           | geological_layer_age / dating_method                 ||  "14C (radiocarbon) dating"                   |
| `material_dated`                    | character | assemblage_age / material_dated,                     ||  "flint", "limestone", "tephra"               |       
|                                     |           | archaeological_layer_age / material_dated,           ||  "lithic burnt",                              |
|                                     |           | geological_layer_age / material_dated                ||  "bone"                                       |
| `tool_list`                         | character | typology / tool_list                                 ||  "core 29, bladelet 136, blade 1090"          |  
| `raw_material_list`                 | character | raw_material / raw_material_list                     ||  "ironstone banded"                           |
| `transport_distance`                | character | raw_material / transport_distance                    ||  "regional (6-20 km)"                         |
| `organic_tools_interpretation`      | character | organic_tools / interpretation                       ||  "harpoon", "worked", "retoucher"             |
| `symbolic_artifacts_interpretation` | character | symbolic_artifacts / interpretation                  ||  "anthropomorphic"                            |
| `feature_interpretation`            | character | feature / interpretation                             ||  "stone construction"                         |
| `miscellaneous_finds_material`      | character | miscellaneous_finds / material                       ||  "ostrich egg shell", "metal"                 |
| `human_genus`                       | character | publication_desc_humanremains / genus                ||  "Homo", "Paranthropus"                       |
| `human_species`                     | character | publication_desc_humanremains / species              ||  "sapiens rhodesiensis", "cf. sapiens"        |
| `plant_remains`                     | character | plantremains / plant_remains                         ||  "phytoliths", "plant macroremains"           |
| `plant_family`                      | character | plant_taxonomy / family                              ||  "Anarcadiaceae",    "Phyllanthaceae"         |
| `plant_genus`                       | character | plant_taxonomy / genus                               ||  "Jasione", "Larix/Picea"                     |
| `plant_species`                     | character | plant_taxonomy / species                             ||  "Potamogeton gramineus L."                   |
| `fauna_genus`                       | character | taxonomical_classification / genus                   ||  "Lemniscomys", "Hipposideros"                |
| `fauna_species`                     | character | paleofauna / species                                 ||  "cf. germanicus", "atapuerquensis"           |


| Attribute                                 | Funktion                                     | Type                 |
| ----------------------------------------- | -------------------------------------------- | -------------------- |
| `locality_id`                             |  `road_get_*`                                | character            |
| `continent`                               |  `road_get_*`                                | character            |
| `subcontinent`                            |  `road_get_*`                                | character            |
| `country`                                 |  `road_get_*`                                | character            |
| `coord_x`                                 |  `road_get_*`                                | number               |
| `coord_y`                                 |  `road_get_*`                                | number               |
| `locality_types`                          |  `road_get_*`                                | character            |
| `categories`                              |  `road_get_*`                                | character            |
| `cultural_periods`                        |  `road_get_*`                                | character            |
| `technocomplexes`                         |  `road_get_*`                                | character            |
| `subset_min_age`                          |  `road_get_localities`                       | number               |
| `subset_max_age`                          |  `road_get_localities`                       | number               |
| `locality_min_age`                        |  `road_get_localities`                       | number               |
| `locality_max_age`                        |  `road_get_localities`                       | number               |
| `humanremains`                            |  `road_get_assemblages*`                     | boolean              |
| `archaeology`                             |  `road_get_assemblages*`                     | boolean              |
| `plantremains`                            |  `road_get_assemblages*`                     | boolean              |
| `paleofauna`                              |  `road_get_assemblages*`                     | boolean              |
| `min_age`                                 |  `road_get_*` (except `road_get_localities`) | number               |
| `max_age`                                 |  `road_get_*` (except `road_get_localities`) | number               |
| `assemblage_name`                         |  `road_get_*` (except `road_get_localities`) | character            |
| `comments`                                |  `road_get_*` (except `road_get_localities`) | character            |
| `tool_list`                               |  `road_get_lithic_typology`                  | character            |
| `typology`                                |  `road_get_lithic_typology`                  | character            |
| `percentage`                              |  `road_get_lithic_typology`                  | character            |
| `raw_material_list`                       |  `road_get_lithic_raw_material`              | character            |
| `transport_distance`                      |  `road_get_lithic_raw_material`              | character            |
| `percentage`                              |  `road_get_lithic_raw_material`              | character            |
| `organic_tools_interpretation`            |  `road_get_organic_tools`                    | character            |
| `organic_raw_material`                    |  `road_get_organic_tools`                    | character            |
| `organic_tools_technology`                |  `road_get_organic_tools`                    | character            |
| `number`                                  |  `road_get_organic_tools`                    | character            |
| `symbolic_artifacts_interpretation`       |  `road_get_symbolic_artifacts`               | character            |
| `symbolic_artifacts_category`             |  `road_get_symbolic_artifacts`               | character            |
| `symbolic_artifacts_material`             |   `road_get_symbolic_artifacts`              | character            |
| `symbolic_artifacts_raw_material_source`  |  `road_get_symbolic_artifacts`               | character            |
| `symbolic_artifacts_technology`           |  `road_get_symbolic_artifacts`               | character            |
| `miscellaneous_finds_material`            |  `road_get_miscellaneous_finds`              | character            |
| `miscellaneous_finds_raw_material_source` |  `road_get_miscellaneous_finds`              | character            |
| `number`                                  |  `road_get_miscellaneous_finds`              | character            |

