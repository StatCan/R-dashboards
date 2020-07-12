# Script to process data 


library(rgdal)
library(rmapshaper)

source('load_data.R')


# Download nmvr and nmvs datasets -----------------------------------------
update_dataset('nmvr')
update_dataset('nmvs')



# CMA MAp ----------------------------------------------------------------------
can_cma_shapes <- 
  readOGR(
    dsn = "./data/raw/lcma000b16a_e/lcma000b16a_e.shp", 
    encoding='UTF-8',
    use_iconv = TRUE) %>%
  spTransform(CRS("+proj=longlat +datum=WGS84")) %>% 
  ms_simplify()

writeOGR(
  obj = can_cma_shapes,
  dsn = './data/processed/shapes',
  layer = 'can_cma_shapes',
  driver = 'ESRI Shapefile',
  overwrite_layer = TRUE)



# English/French variable terms -------------------------------------------
dictionary <- tibble(variable = character(), en = character(), fr = character())

nmvr_en <- get_cansim("20-10-0021-01", refresh=TRUE, language='en') %>% janitor::clean_names()
nmvr_fr <- get_cansim("20-10-0021-01", refresh=TRUE, language='fr') %>% janitor::clean_names()

fuel_types_fr <- nmvr_fr$type_de_carburant %>% unique()
fuel_types_en <- nmvr_en$fuel_type %>% unique()
fuel_types <- tibble(variable = 'fuel_type', en = fuel_types_en, fr = fuel_types_fr)
dictionary <- bind_rows(dictionary, fuel_types)

geo_fr <- nmvr_fr$geo %>% unique()
geo_en <- nmvr_en$geo %>% unique()
geo <- tibble(variable = 'geo', en = geo_en, fr = geo_fr)
dictionary <- bind_rows(dictionary, geo)

write_csv(dictionary, './data/processed/dictionary_variables.csv')







