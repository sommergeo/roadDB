# Übersicht: Abgefragte Datenbanktabellen

Diese Liste enthält alle Tabellen der ROAD-Datenbank, die im `roadDB`-R-Paket
über SQL-Abfragen angesprochen werden. Die Abfragen
werden in den jeweiligen `R/`-Dateien als SQL-Strings zusammengebaut
und über `road_run_query()` gegen die Datenbank ausgeführt.
Allerdings erlaubt `get_sql.R` (`road_get_sql()`) beliebige SQL-Abfragen durch
Nutzer:innen, dadurch kann jede Tabelle der Datenbank abgefragt werden.

| # | Tabelle | Verwendet in (Datei) |
|---|---------|------------------------|
| 1 | `archaeological_layer` | `get_assemblages.R`, `get_localities.R` |
| 2 | `archaeological_layer_age` | `get_dates.R` |
| 3 | `archaeological_stratigraphy` | `get_assemblages.R`, `get_localities.R`, `get_info.R` |
| 4 | `archlayer_correl_geolayer` | `get_dates.R` |
| 5 | `assemblage` | `get_assemblages.R`, `get_info.R` |
| 6 | `assemblage_age` | `get_dates.R` |
| 7 | `assemblage_in_archlayer` | `get_assemblages.R` |
| 8 | `assemblage_in_geolayer` | `get_assemblages.R`, `get_dates.R` |
| 9 | `climate` | `get_publications.R` |
| 10 | `edition` | `get_dates.R`, `get_publications.R` |
| 11 | `feature` | `get_archaeology.R`, `get_info.R` |
| 12 | `function` | `get_info.R` (nur `road_summarize_archaeology`) |
| 13 | `geological_layer_age` | `get_dates.R` |
| 14 | `geological_stratigraphy` | `get_assemblages.R` |
| 15 | `geopolitical_units` | `get_localities.R`, `get_info.R` |
| 16 | `geostrat_desc_geolayer` | `get_assemblages.R`, `get_dates.R` |
| 17 | `humanremains` | `get_humanremains.R`, `get_assemblages.R` |
| 18 | `locality` | `get_localities.R`, `get_info.R` |
| 19 | `miscellaneous_finds` | `get_archaeology.R`, `get_info.R` |
| 20 | `organic_tools` | `get_archaeology.R`, `get_info.R` |
| 21 | `paleoflora` | `get_plantremains.R` |
| 22 | `paleofauna` | `get_paleofauna.R`, `get_info.R` |
| 23 | `plant_taxonomy` | `get_plantremains.R`, `get_info.R` |
| 24 | `plantremains` | `get_info.R` (Tabellen-Mapping für `plant_remains`) |
| 25 | `publication` | `get_dates.R`, `get_publications.R` |
| 26 | `publication_desc_archlayer` | `get_publications.R` |
| 27 | `publication_desc_assemblage` | `get_publications.R` |
| 28 | `publication_desc_climate` | `get_publications.R` |
| 29 | `publication_desc_geolayer` | `get_publications.R` |
| 30 | `publication_desc_geostrat` | `get_dates.R`, `get_publications.R` |
| 31 | `publication_desc_humanremains` | `get_humanremains.R`, `get_publications.R`, `get_info.R` |
| 32 | `publication_desc_locality` | `get_publications.R` |
| 33 | `publication_desc_paleofauna` | `get_paleofauna.R`, `get_publications.R` |
| 34 | `publication_desc_vegetation` | `get_publications.R` |
| 35 | `publication_source` | `get_dates.R`, `get_publications.R` |
| 36 | `raw_material` | `get_archaeology.R`, `get_info.R` |
| 37 | `symbolic_artifacts` | `get_archaeology.R`, `get_info.R` |
| 38 | `taxonomical_classification` | `get_paleofauna.R`, `get_info.R` |
| 39 | `technology` | `get_info.R` (nur `road_summarize_archaeology`) |
| 40 | `typology` | `get_archaeology.R`, `get_info.R` |
| 41 | `vegetation` | `get_publications.R` |