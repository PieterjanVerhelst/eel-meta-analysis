# eel-meta-analysis
## About
This study investigates the silver European eel (i.e. the downstream migrating eel) (*Anguilla anguilla* L.) migration behaviour on a continental scale, i.e. Europe. 

Fish migration is a poorly understood phenomenon. Traits like migration speed and timing have evolved to reach spawning and growing habitats in time under the prevailing conditions. Hence, these traits are likely population and/or region specific. For instance, European eels growing in Norway need to migrate a longer distance to the spawning area, presumably in the Sargasso Sea, than conspecifics growing in Portugal. Consequently, they will either migrate faster or earlier than the eels from Portugal. Undoubtedly, migration barriers interfere with fish migration leading to delays. To restore the historically low abundances of diadromous fish populations, we urgently need a better fundamental understanding of their migration. This project will focus on the key hypothesis that the migration speed and timing are population- and region-specific and coordinated in the life cycle of European eel; this hypothesis is subdivided in three subhypotheses: 

Migration speed and timing of European eel vary according to
1. distance between foraging and spawning habitats within the species’ distribution range;
2. size and sex;
3. the presence and types of migration barriers.

For this meta-analysis, data from 20 projects/locations and 9 countries have been centralized: Belgium (5), Denmark (1), France (3), Germany (1), Lithuania (1), Norway (1), Portugal (1), The Netherlands (4), UK (3)



## Project structure

### Data

<mark>Data last updated on 23-11-2021</mark>

* `/raw:`
	+ `raw_detection_data.csv`: dataset containing the raw detection data
	+ `eel_meta_data.csv`: dataset containing the meta-data on the tagged eels
	+ `deployments.csv`: dataset containing the station names and positions of the receivers from ETN
	+ `/stour`:
		+ `stour_data.csv`: dataset containing the raw detection data from the River Stour, '2013_stour' project
		+ `stour_eel_meta.csv`: dataset containing the eel meta-data from the River Stour, '2013_stour' project
		+ `stour_deployments.csv`: dataset containing the deployment meta-data from the River Stour, '2013_stour' project
	+ `/nedap_meuse`:
		+ `/raw_nedap_data`: folder containing the raw NEDAP tracking data from the River Meuse in The Netherlands. The data is separated in files with English and Dutch headers.
		+ `nedap_meuse_eel_meta.csv`: dataset containing the eel meta-data from the River Meuse under the NEDAP tracking system in The Netherlands
		+ `nedap_meuse_deployments.csv`: dataset containing the deployment meta-data from the River Meuse under the NEDAP tracking system in The Netherlands

* `/interim:`
	+ `deployments.csv`: dataset containing the station names and positions of the receivers from ETN, Stour project and NEDAP project
	+ `eel_meta_data.csv`: cleaned dataset containing the eel meta data from ETN and Stour project
	+ `detection_data.csv`: cleaned dataset containing the detection data from ETN and Stour project
	+ `/receivernetworks`: folder containing the generated files with the receiver networks per project
	+ `/residencies`: folder containing the residency datasets, calculated via the `smooth_eel_tracks.R` code
	+ `/speed`: folder containing the speed datasets, calculated via the `calculate_speed.R` code
	+ `/migration`: folder containing the datasets with records flagged as migration or not, calculated via the `identify_migration.R` code

* `/external:`
	+ `release_locations_stations.csv`: file with the release locations and the abbreviated release station names.
	+ `/distance_matrices`: folder containing the distance matrices with the distances between detection stations per receiver network (matrices are created at https://github.com/inbo/fish-tracking).
	+ `station_order.csv`: file containing the stations upstream the release location. This file is needed in `calculate_speed.R`
	+ `habitats.csv`: file with the classification of the habitats linked to the detections stations, i.e. freshwater, tidal freshwater, tidal brackish, marine barrier and marine.
	+ `migrationbarriers.csv`: file containing whether or not a project/river has a migration barrier and if so, which type and where (coordinates).

### Scripts

* `/src:`
1. `download_data.R`: Download data from silver eel meta-analysis from ETN database via RStudio LifeWatch server
	* obtain detection dataset `raw_detection_data.csv`
	* obtain meta-data on tagged eels `eel_meta_data.csv`
	* obtain meta-data on deployments `deployments.csv` (station names and positions)
2. `attach_stour.R`: Merge data from the River Stour to the dataset
3. `attach_nedap_meuse.R`: Merge NEDAP data from the River Meuse in the Netherlands to the dataset
4. `clean_eel_metadata.R`: Eel meta-data cleaning by removing irrelevant and redundant eels, columns and make certain column values consistent
5. `clean_detection_data.R`: Data cleaning by filling in missing values and removing false detections
6. `attach_release.R`: Add eel release positions and date-time to detection dataset
7. `create_interactive_maps.R`: Create interactive html widget maps per project
8. `extract_network.R`: Extract receiver networks based on detection data
	* This serves as input to calculate the distance matrices at https://github.com/inbo/fish-tracking
9. `smooth_eel_tracks.R`: Smooths duplicates and calculates residencies per eel per station. Therefore, it calls the following two functions:
	+ 9a. `get_nearest_stations.R`: general function to extract the smoothed track for one eel (via its `transmitter ID`)
	+ 9b. `get_timeline.R`: function to get the stations which are near a given station (where near means that the distance is smaller than a certain given limit, e.g. detection range).
		- --> Generate residency datasets per project and store them in `/interim/residencies`
10. `calculate_speed.R`: Calculate movement speeds between consecutive detection stations. Also calculates swim distance, swim time, cumulative swim distance and station distance from source station.
	+ 10a. `calculate_speed_function.R`: function to calculate speed between consecutive displacements; based on a function in Hugo Flavio's `actel` package
	+ 10b. `calculate_sourcedistance_function.R`: function to calculate the station distance from a 'source' station; based on a function in Hugo Flavio's `actel` package
		- --> Generate speed datasets per project and store them in `/interim/speed`
	+ 10c. `clean_residency_data_functions.R`: functions to clean residency datasets of specific projects (false and incorrect detections that came to light via the distance plots. 
11. `identify_migration.R`: Code that identifies records as migration based on a speed and distance threshold.
	+ 11a. `identify_migration_functions.R`: functions to identify migration.
12. `create_distance_plot.R`: Create plots with travelled distance per eel and store as .pdf





### Figures

* `/figures:`
	+ `/html_interactive_maps`: interactive html widget maps per project
	+ `/distance_tracks`: pdf-files with traveled distances per project






