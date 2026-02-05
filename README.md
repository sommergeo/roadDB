# roadDB  <a href="https://www.hadw-bw.de/en/research/research-center/roceeh"><img src="docs/roadDB_logo.png" align="right" height="138" /></a>
Provides an R interface to the [ROCEEH Out of Africa Database (ROAD)](<https://www.roceeh.uni-tuebingen.de/roadweb/smarty_road_simple_search.php>), a comprehensive resource for archaeological, anthropological, paleoenvironmental and geographic data from Africa and Eurasia dating from 3,000,000 to 20,000 years BP.
The package allows users to retrieve data from the online database at different levels of detail and customize search requests.
Functions return `data frame` objects compatible with other R packages used in prehistoric and paleoenvironmental science, supporting reproducible workflows as an input provider.  

The package is maintained by [Christian Sommer](https://orcid.org/0000-0001-9062-0876), [Zara Kanaeva](https://orcid.org/0000-0002-1989-1494), [Timo Schtreicher](https://orcid.org/0009-0009-2193-4308) and [Jesper Borre Pedersen](https://orcid.org/0000-0002-3468-0986) at the Research Center [The Role of Culture in Early Expansions of Humans (ROCEEH)](https://www.hadw-bw.de/en/research/research-center/roceeh) and funded by the [Heidelberg Academy of Sciences and Humanties](https://www.hadw-bw.de/en).


## :cloud: Database status
This package provides an online connection to the ROCEEH Out of Africa Database (ROAD). Please ensure a stable internet connection when working with roadDB.

The database is currently:  :green_circle: **Online**

It uses a regularly updated snapshot of the database; the current version was updated on **2025-12-01**.


## :notebook: Tutorial
An introduction to the package with example workflows can be found at [https://www.sommergeo.com/roadDB/tutorial.html]()


## :unlock: License
This package is licensed under the **Creative Commons Attribution-ShareAlike 4.0 International ([CC BY-SA 4.0](https://creativecommons.org/licenses/by-sa/4.0/))**.
Contents retrieved from the ROAD database are published under the same license and should be cited as

> Kandel, A. W., Sommer, C., Kanaeva, Z., Bolus, M., Bruch, A. A., Groth, C., Haidle, M. N., Hertler, C., Heß, J., Malina, M., Märker, M., Hochschild, V., Mosbrugger, V., Schrenk, F., & Conard, N. J. (2023). The ROCEEH Out of Africa Database (ROAD): A large-scale research database serves as an indispensable tool for human evolutionary studies. PLOS ONE, 18(8), e0289513. https://doi.org/10.1371/journal.pone.0289513


## :computer: Functionality


### Installation
```
# CRAN installation (currently in review)
# install.packages(roadDB)

# Github installation
# Windows users are required to install Rtools: https://cran.r-project.org/bin/windows/Rtools/
install.packages("devtools")
devtools::install_github("sommergeo/roadDB")
```


### Structure
The roadDB package has three main levels of detail (LOD) that follow a hierarchical order: Locality, Assemblage and Date. A locality can have multiple assemblages, and each assemblage can have multiple dates associated with it.

<p align="center">
<img src="docs/levels_of_detail.svg" alt="Illustration of the three levels of the roadDB R-package from top to bottom: Locality, Assemblage and Date" height="250">
</p>

Users can query information at different LODs using dedicated functions that follow the `road_get_*` naming convention. These return dataframes where each row represents an item at the requested granularity and includes attribute columns relevant to those items.

An extensive set of arguments can be applied to all `road_get_*` functions, allowing users to refine their queries and tailor the results to their needs.

As the ROAD database offers exceptionally rich information at the assemblage level, there are subordinate functions for querying human remains, archaeology, palaeofauna and palaeobotany.


### Core functions
- 1st level of detail:
	- road_get_localities()
- 2nd level of detail:
	- road_get_assemblages()
	- road_get_human_remains()
	- road_get_paleofauna()
	- road_get_paleobotany()
	- archaeology-related:
		- road_get_lithic_typologies()
		- road_get_lithic_raw_materials()
		- road_get_organic_tools()
		- road_get_symbolic_artifacts()
		- road_get_features()
		- road_get_miscellaneous_finds()
- 3rd level of detail:
	- road_get_dates()
	- road_get_publications()

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
| `miscellaneous_find_material`       | character | miscellaneous_finds / material                       | exact       |  "ostrich egg shell", "metal"                 |
| `human_genus`                       | character | publication_desc_humanremains / genus                | exact       |  "Homo", "Paranthropus"                       |
| `human_species`                     | character | publication_desc_humanremains / species              | exact       |  "sapiens rhodesiensis", "cf. sapiens"        |
| `plant_remains`                     | character | plantremains / plant_remains                         | exact       |  "phytoliths", "plant macroremains"           |
| `plant_family`                      | character | plant_taxonomy / family                              | exact       |  "Anarcadiaceae",    "Phyllanthaceae"         |
| `plant_genus`                       | character | plant_taxonomy / genus                               | exact       |  "Jasione", "Larix/Picea"                     |
| `plant_species`                     | character | plant_taxonomy / species                             | exact       |  "Potamogeton gramineus L."                   |
| `fauna_genus`                       | character | taxonomical_classification / genus                   | exact       |  "Lemniscomys", "Hipposideros"                |
| `fauna_species`                     | character | paleofauna / species                                 | exact       |  "cf. germanicus", "atapuerquensis"           |
| `bibtex`                            | character | publication / edition / publication_source           | exact       |  "F", "T", "False", "TRUE         "           |


### Result tables
The following table provides an overview of returned attributes.

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
| `coordinate_source`                       |  `road_get_localities`                       | character            |
| `subset_age_min`                          |  `road_get_localities`                       | number               |
| `subset_age_max`                          |  `road_get_localities`                       | number               |
| `locality_age_min`                        |  `road_get_localities`                       | number               |
| `locality_age_max`                        |  `road_get_localities`                       | number               |
| `is_systematic`                           |  `road_get_assemblages`                      | character            |
| `geolayer`                                |  `road_get_*` (except `road_get_localities`) | character            |
| `archlayer`                               |  `road_get_*` (except `road_get_localities`) | character            |
| `human_remains`                           |  `road_get_assemblages`                      | boolean              |
| `archaeology`                             |  `road_get_assemblages`                      | boolean              |
| `plant_remains`                           |  `road_get_assemblages`                      | boolean              |
| `paleofauna`                              |  `road_get_assemblages`                      | boolean              |
| `age_min`                                 |  `road_get_*` (except `road_get_localities`) | number               |
| `age_max`                                 |  `road_get_*` (except `road_get_localities`) | number               |
| `assemblage_name`                         |  `road_get_*` (except `road_get_localities`) | character            |
| `comment`                                 |   archaeological `road_get_*`                | character            |
| `tool_list`                               |  `road_get_lithic_typologies`                | character            |
| `typology`                                |  `road_get_lithic_typologies`                | character            |
| `percentage`                              |  `road_get_lithic_typologies`                | character            |
| `raw_material_list`                       |  `road_get_lithic_raw_materials`             | character            |
| `transport_distance`                      |  `road_get_lithic_raw_materials`             | character            |
| `percentage`                              |  `road_get_lithic_raw_materials`             | character            |
| `organic_tool_interpretation`             |  `road_get_organic_tools`                    | character            |
| `organic_raw_material`                    |  `road_get_organic_tools`                    | character            |
| `organic_tool_technology`                 |  `road_get_organic_tools`                    | character            |
| `number`                                  |  `road_get_organic_tools`                    | character            |
| `symbolic_artifact_interpretation`        |  `road_get_symbolic_artifacts`               | character            |
| `symbolic_artifact_category`              |  `road_get_symbolic_artifacts`               | character            |
| `symbolic_artifact_material`              |  `road_get_symbolic_artifacts`               | character            |
| `symbolic_artifact_raw_material_source`   |  `road_get_symbolic_artifacts`               | character            |
| `symbolic_artifact_technology`            |  `road_get_symbolic_artifacts`               | character            |
| `feature_interpretation`                  |  `road_get_features`                         | character            |
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
| `mni`                                     |  `road_get_paleofauna`                       | number               |
| `mni_method`                              |  `road_get_paleofauna`                       | character            |
| `nisp`                                    |  `road_get_paleofauna`                       | number               |
| `plant_remains`                           |  `road_get_plantremains`                     | character            |
| `plant_family`                            |  `road_get_plantremains`                     | character            |
| `plant_genus`                             |  `road_get_plantremains`                     | character            |
| `plant_species`                           |  `road_get_plantremains`                     | character            |
| `element`                                 |  `road_get_plantremains`                     | character            |
| `abundance`                               |  `road_get_plantremains`                     | number               |
| `relative_abundance`                      |  `road_get_plantremains`                     | number               |
| `age`                                     |  `road_get_dates`                            | number               |
| `negative_standard_deviation`             |  `road_get_dates`                            | number               |
| `positive_standard_deviation`             |  `road_get_dates`                            | number               |
| `material_dated`                          |  `road_get_dates`                            | character            |
| `dating_method`                           |  `road_get_dates`                            | character            |
| `laboratory_idlaboratory`                 |  `road_get_dates`                            | character            |
| `publication`                             |  `road_get_publications`                     | character            |

