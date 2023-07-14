# Calculate the migration speed
# By Pieterjan Verhelst (INBO)
# pieterjan.verhelst@inbo.be


source("src/process_migration_data.R")

# Calculate overall migration speed: speed between first and last detection as 'has_migration_started == TRUE'
migration_speed <- data %>%
  group_by(animal_project_code, acoustic_tag_id) %>%
  mutate(time = max(departure)-min(arrival),
         distance = max(distance_to_source_m) - min(distance_to_source_m)) %>%
  select(animal_project_code, acoustic_tag_id, time, distance) %>%
  distinct()

migration_speed$time <- as.numeric(migration_speed$time)
migration_speed$speed_ms <- migration_speed$distance / migration_speed$time
summary(migration_speed$speed_ms)
