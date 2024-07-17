library(tidyverse)
library(sf)
library(terra)
library(biooracler)

# Load Data ---------------------------------------------------------------

## load ex occurences
## GBIF.org (11 July 2024) GBIF Occurrence Download  https://doi.org/10.15468/dl.jd5her
occurences = read_csv(here::here("data", "Raw", "GBIF_Occurance_data2.csv"))%>% 
  select(occurrenceStatus, decimalLatitude, decimalLongitude) %>% 
  filter(!is.na(as.numeric(decimalLatitude))) %>% 
  filter(!is.na(as.numeric(decimalLongitude)))  %>% 
  filter(occurrenceStatus == "PRESENT") %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = "EPSG:4326")

## load ph data for ex
dataset_id = "ph_baseline_2000_2018_depthmax"

time = c('2010-01-01T00:00:00Z', '2010-01-01T00:00:00Z')

latitude = c(-89.975, 89.975) 
longitude = c(-179.975, 179.975) 

constraints = list(time, latitude, longitude)
names(constraints) = c("time", "latitude", "longitude")

variables = "ph_mean"

ph_mean <- download_layers(dataset_id, variables = variables, constraints = constraints)

ph_mean = project(ph_mean, st_crs(occurences)[["wkt"]])

class(ph_mean)
# Buffer ------------------------------------------------------------------

buffer_occurence = st_buffer(occurences, 5000)

ph_crop = crop(ph_mean, buffer_occurence)
no_points = mask(ph_crop, buffer_occurence, inverse=TRUE)

# takes a while, but you'll want to change the number to get close to the number of occurences you have
random_points = terra::spatSample(no_points, 10000, "random", na.rm=T, as.points=TRUE) %>% 
  st_as_sf() #should be an sf dataframe

class(random_points)


random_points = random_points %>% 
  filter(ph_mean > 0)%>% 
  mutate(occurrenceStatus = "ABSENT")%>% 
  select(ph_mean, occurrenceStatus)

ph_extract = terra::extract(ph_mean, occurences) %>% 
  cbind(occurences) %>% 
  st_as_sf() %>% 
  select(ph_mean, occurrenceStatus)

model_data = rbind(random_points, ph_extract)%>% 
  st_as_sf()

ggplot(model_data) +
  geom_sf(aes(color = occurrenceStatus)) +
  theme_bw()

# Then use terra extract on rest of raster data, or combine other vectors using st_intersection