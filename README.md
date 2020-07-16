# eel-meta-analysis
## About
This study investigates the silver European eel (i.e. the downstream migrating eel) (*Anguilla anguilla* L.) migration behaviour on a continental scale, i.e. Europe. 

Fish migration is a poorly understood phenomenon. Traits like migration speed and timing have evolved to reach spawning and growing habitats in time under the prevailing conditions. Hence, these traits are likely population and/or region specific. For instance, European eels growing in Norway need to migrate a longer distance to the spawning area, presumably in the Sargasso Sea, than conspecifics growing in Portugal. Consequently, they will either migrate faster or earlier than the eels from Portugal. Undoubtedly, migration barriers interfere with fish migration leading to delays. To restore the historically low abundances of diadromous fish populations, we urgently need a better fundamental understanding of their migration. This project will focus on the key hypothesis that the migration speed and timing are population- and region-specific and coordinated in the life cycle of European eel; this hypothesis is subdivided in three subhypotheses: 

Migration speed and timing of European eel vary according to
1. distance between foraging and spawning habitats within a species’ distribution range;
2. size and sex within a species;
3. the presence and types of migration barriers.

For this meta-analysis, data from 19 projects/locations and 9 countries have been centralized: Belgium (5), Denmark (1), France (3), Germany (1), Lithuania (1), Norway (1), Portugal (1), The Netherlands (3), UK (3)


## Project structure

### Data

<mark>Updated on 15/06/2020</mark>

* `/raw:`
	+ `raw_detection_data.csv`: dataset containing the raw detection data
	+ `eel_meta_data.csv`: dataset containing the meta-data on the tagged eels
	+ `deployments.csv`: dataset containing the station names and positions of the receivers from ETN
	+ `/stour:`
		+ `stour_data.csv`: dataset containing the raw detection data from the River Stour, '2013_stour' project
		+ `stour_eel_meta.csv`: dataset containing the eel meta-data from the River Stour, '2013_stour' project
		+ `stour_deployments.csv`: dataset containing the deployment meta-data from the River Stour, '2013_stour' project

* `/interim:`
	+ `deployments.csv`: dataset containing the station names and positions of the receivers from ETN and Stour project
	+ `eel_meta_data.csv`: dataset containing the eel meta data from ETN and Stour project

* `/external:`


### Data-analyse

* `/src:`
1. `download_data.R`: Download data from silver eel meta-analysis from ETN database via RStudio LifeWatch server
	* obtain detection dataset `raw_detection_data.csv`
	* obtain meta-data on tagged eels `eel_meta_data.csv`
	* obtain meta-data on deployments `deployments.csv` (station names and positions)
2. `attach_stour.R`: Merge data from the River Stour to the dataset
3. `process_data.R`: Data processing by filling in missing values and removing false detections
4. `create_interactive_maps.R`: Create interactive html widget maps per project


### Figures

* `/figures:`
	+ `/html_interactive_maps:` interactive html widget maps per project






