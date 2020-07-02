library(cansim)
library(tidyverse)


nmvs_path <- './data/processed/nmvs.csv'
nmvr_path <- './data/processed/nmvr.csv'


download_nmvr_data <- function() {
  ## Download nmvr data using STC API from https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=2010002101
  
  # Province latitude and longitude data
  provinces_latlong <- read_csv('data/raw/provinces_latlong.csv') %>% 
    janitor::clean_names()
  
  # Download data
  nmvr_data <- get_cansim("20-10-0021-01", refresh=TRUE) %>% 
    janitor::clean_names() %>% 
    filter(str_detect(vehicle_type, '^Total')) %>% 
    filter(geo != 'Canada') %>% 
    select(year = ref_date, geo, fuel_type, amount = value) %>%
    dplyr::mutate(year = as.numeric(year)) %>% 
    replace_na(list(amount = 0)) %>% 
    dplyr::mutate(geo = recode(geo, `British Columbia and the Territories` = 'British Columbia')) %>% # Fix later
    left_join(provinces_latlong, by = c('geo' = 'province')) %>%  # Combine with provinces lat long information
    dplyr::group_by(geo, fuel_type) %>%  # Calculate cumulative sum per province and fuel type
    dplyr::mutate(cumsum = cumsum(amount)) %>% 
    dplyr::ungroup()
  
  return (nmvr_data)
}


download_nmvs_data <- function(aggregate_yearly = TRUE) {
  ## Download nmvs data using STC API from https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=2010000101
  
  # Download data
  nmvs_data <- get_cansim("20-10-0001-01", refresh=TRUE) %>% 
    janitor::clean_names() %>% 
    filter(seasonal_adjustment == 'Unadjusted') %>% 
    replace_na(list(value = 0)) %>% 
    dplyr::mutate(value = ifelse(sales == 'Units', value, value*1000)) %>% # Convert dollars in 1000 to dollars in $1 unit
    dplyr::select(ref_date, geo, vehicle_type, origin_of_manufacture, sales, value) %>% 
    separate(ref_date, c('year', 'month'), sep='-') 
  
    if (aggregate_yearly) {
      nmvs_data <- nmvs_data %>% 
        group_by(year, geo, vehicle_type, origin_of_manufacture, sales) %>%
        dplyr::summarise(value = sum(value)) %>%  # Aggregate value by year 
        ungroup()
    }
    
  return (nmvs_data)
}


update_dataset <- function(dataset) {
  ## Updates a given dataset if doesn't exist or is at least 24 hours old 
  ## and stores it in the data/processed directory
  
  if (dataset == 'nmvs') {
    data_path <- nmvs_path
    download_data <- download_nmvs_data
  }
  else if (dataset == 'nmvr') {
    data_path <- nmvr_path
    download_data <- download_nmvr_data
  }
  
  ctime <- file.info(data_path)$mtime
  current_time <- Sys.time()
  diff <- difftime(current_time, ctime) %>% as.numeric(units = 'hours')
  
  if (is.na(diff) | diff >= 24) {
    data <- download_data()
    readr::write_csv(data, data_path)
  }
}


load_dataset <- function(dataset) {
  ## Loads a given data set and upates it if necessary
  
  update_dataset(dataset)
  
  if (dataset == 'nmvs') {
    data <- readr::read_csv(nmvs_path)
  }
  else if (dataset == 'nmvr') {
    data <- readr::read_csv(nmvr_path)
  }
  
  return (data)
}


