setwd("~/Library/CloudStorage/OneDrive-UniversityofLeeds/PhD Materials/Time-Series/JPS/Time-Series Workshop/data")
main_dir <- "/Users/hulyaseyidoglu/Library/CloudStorage/OneDrive-UniversityofLeeds/Phd Projects/shoplifting_paper/data"
dir.create('data')

library(dplyr)
library(data.table)
library(readr)
library(lubridate)
library(sf)
library(purrr)
library(tibble)
library(janitor)
library(tidyr)
library(stringr)

`%nin%` <- Negate(`%in%`)

process_year_data <- function(year, police_data_dir = "police_data") {
  file_pattern <- paste0(year, "*street.csv")
  csv_files <- list.files(police_data_dir, pattern = glob2rx(file_pattern), recursive = TRUE, full.names = TRUE)
  
  data <- lapply(csv_files, read_csv)
  
  full_data <- data %>%
    bind_rows() %>%
    clean_names() %>%
    mutate(year = as.character(year))
  
    sub_data <- full_data %>%
      filter(reported_by != "Greater Manchester Police", 
             reported_by != "Police Service of Northern Ireland") %>%
      drop_na(lsoa_code)

  setDT(sub_data)
  sub_data_agg <- sub_data[, .(crime_count = .N), by = .(crime_type, month, year, lsoa_code, lsoa_name, reported_by)]
  
  return(sub_data_agg)
}


police_data_dir <- "/Users/hulyaseyidoglu/Library/CloudStorage/OneDrive-UniversityofLeeds/Phd Projects/Time-Series Workshop/data/police_data"
all_years_data_1516 <- lapply(2015:2016, process_year_data, police_data_dir = police_data_dir) # function is not working so I do this for 2015 and 2016 manually

all_years_data_1719 <- lapply(2017:2019, process_year_data, police_data_dir = police_data_dir)
all_years_data_2023 <- lapply(2020:2023, process_year_data, police_data_dir = police_data_dir)



# Combine all years into dfs
full_dataset_1516 <- bind_rows(all_years_data_1516)
full_dataset_1719 <- bind_rows(all_years_data_1719)
full_dataset_2023 <- bind_rows(all_years_data_2023)

rm(all_years_data_1516, all_years_data_1719, all_years_data_2023)




### 2015-2016 police data handling manually ###

list_2015 <- list.files(path = "/Users/hulyaseyidoglu/Library/CloudStorage/OneDrive-UniversityofLeeds/Phd Projects/Time-Series Workshop/data/police_data", 
                        pattern = glob2rx("2015*street.csv"), recursive=TRUE, full.names = TRUE)
list_2016 <- list.files(path = "/Users/hulyaseyidoglu/Library/CloudStorage/OneDrive-UniversityofLeeds/Phd Projects/Time-Series Workshop/data/police_data", 
                        pattern = glob2rx("2016*street.csv"), recursive=TRUE, full.names = TRUE)

data_2015 <- lapply(list_2015, read_csv)
data_2016 <- lapply(list_2016, read_csv)

full_data_2015 <- data_2015 %>% 
  bind_rows() %>% 
  clean_names() %>% 
  mutate(year = "2015")

full_data_2016 <- data_2016 %>% 
  bind_rows() %>% 
  clean_names() %>% 
  mutate(year = "2016")

sub_data_2015 <- full_data_2015 %>% 
  filter(reported_by != "Greater Manchester Police", 
         reported_by != "Police Service of Northern Ireland")

sub_data_2016 <- full_data_2016 %>% 
  filter(reported_by != "Greater Manchester Police", 
         reported_by != "Police Service of Northern Ireland")

rm(data_2015, data_2016, full_data_2015, full_data_2016)

sub_data_2015 <- drop_na(sub_data_2015, lsoa_code)
sub_data_2016 <- drop_na(sub_data_2016, lsoa_code)

miss_lsoa_2015_df <- sub_data_2015 %>% 
  filter(is.na(lsoa_code))
sum(is.na(miss_lsoa_2015_df$longitude))
sum(is.na(miss_lsoa_2015_df$latitude))
rm(miss_lsoa_2015_df)

miss_lsoa_2016_df <- sub_data_2016 %>% 
  filter(is.na(lsoa_code))
sum(is.na(miss_lsoa_2016_df$longitude))
sum(is.na(miss_lsoa_2016_df$latitude))
rm(miss_lsoa_2016_df)

setDT(sub_data_2015)
sub_data_agg_2015 <- sub_data_2015[, .(crime_count = .N), by = .(crime_type, month, year, lsoa_code, lsoa_name, reported_by)]

setDT(sub_data_2016)
sub_data_agg_2016 <- sub_data_2016[, .(crime_count = .N), by = .(crime_type, month,year,  lsoa_code, lsoa_name, reported_by)]

full_dataset_1516 <- bind_rows(sub_data_agg_2015, sub_data_agg_2016)

rm(sub_data_2015, sub_data_2016, sub_data_agg_2015, sub_data_agg_2016)

### Manual handling for 2015 and 2016 data finishes ###

## all crimes from NI and GM are removed ##
lsoa_sf <- st_read("/Users/hulyaseyidoglu/Library/CloudStorage/OneDrive-UniversityofLeeds/PhD Materials/Time-Series/JPS/Time-Series Workshop/data/ukds_infuse_lsoa/infuse_lsoa_lyr_2011_clipped.shp")

# Subset LSOAs for E&W and then removing GM data
lsoa_ew_sf <- lsoa_sf %>% 
  mutate(country_cd = str_extract(geo_code, "^.{1}")) %>% 
  filter(country_cd == "E" | country_cd == "W",
         !str_detect(geo_label, "Manchester|Bolton|Oldham|Rochdale|Stockport|Tameside|Salford|Bury|Trafford|Wigan"))

# Check validity of the remaining LSOAs.
validity_check <- st_is_valid(lsoa_ew_sf)

# N = 4 invalid geometries.
table(validity_check) 

# Identify them
lsoa_ew_sf <- lsoa_ew_sf %>% 
  mutate(valid = validity_check)

filter(lsoa_ew_sf, valid == "FALSE")

# Resolve invalid geometries.
lsoa_ew_valid_sf <- st_make_valid(lsoa_ew_sf)

# Check. 
table(st_is_valid(lsoa_ew_valid_sf))

# Remove GM and NI geometries
gm_lsoa <- lsoa_sf %>% 
  filter(str_detect(geo_label, "Manchester|Bolton|Oldham|Rochdale|Stockport|Tameside|Salford|Bury|Trafford|Wigan")) %>% 
  select(geo_code) %>% 
  pluck(1)

ni_lsoa <- lsoa_sf %>%
  mutate(country_cd = str_extract(geo_code, "^.{1}")) %>% 
  filter(country_cd == "9") %>% 
  select(geo_code) %>% 
  pluck(1)

# Remove objects to save space if needed.
rm(lsoa_sf, lsoa_ew_sf)

# Remove the GM and NI LSOA from the crime data.
sub_data_1516 <- full_dataset_1516 %>% 
  filter(lsoa_code %nin% gm_lsoa, lsoa_code %nin% ni_lsoa)

sub_data_1719 <- full_dataset_1719 %>% 
  filter(lsoa_code %nin% gm_lsoa, lsoa_code %nin% ni_lsoa)

sub_data_2023 <- full_dataset_2023 %>% 
  filter(lsoa_code %nin% gm_lsoa, lsoa_code %nin% ni_lsoa)

length(unique(lsoa_ew_valid_sf$geo_code))
length(unique(sub_data_1516$lsoa_code))
length(unique(fsub_data_1719$lsoa_code))
length(unique(sub_data_2023$lsoa_code))

missing_lsoa <- lsoa_ew_valid_sf %>% 
  filter(lsoa_ew_valid_sf$geo_code %nin% sub_data_1516$lsoa_code) 

lsoa_ew_valid_sf <- lsoa_ew_valid_sf %>% 
  filter(geo_code %nin% missing_lsoa$geo_code)

sub_data_2023 <- sub_data_2023 %>%
  semi_join(lsoa_ew_valid_sf, by = c("lsoa_code" = "geo_code"))

length(unique(lsoa_ew_valid_sf$geo_code))
length(unique(sub_data_1516$lsoa_code))
length(unique(sub_data_1719$lsoa_code))
length(unique(sub_data_2023$lsoa_code))

rm(missing_lsoa)


sub_data_full <- bind_rows(sub_data_1516, sub_data_1719, sub_data_2023)
rm(full_dataset_1516, full_dataset_1719, full_dataset_2023, sub_data_1516,sub_data_1719,sub_data_2023)

#aggragating monthly crimes to LSOA level
sub_data_agg_full_df <- sub_data_full %>%
  group_by(crime_type, lsoa_code, lsoa_name, year, month) %>%
  summarise(crime_count = sum(crime_count))

# Save the combined dataset
write_csv(full_dataset, file = '/Users/hulyaseyidoglu/Library/CloudStorage/OneDrive-UniversityofLeeds/Phd Projects/shoplifting_paper/data/full_dataset.csv')

