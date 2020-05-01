
# install.packages('LaCroixColoR')
# load libraries
library(shinythemes); library(shiny); library(shiny.i18n); library(shinyWidgets)
library(tidyverse); library(magrittr)
library(leaflet); library(rgdal); library(spdplyr); library(rgeos); library(geosphere)
library(plotly); library(RColorBrewer); #require(LaCroixColoR); 
library(scales)
library(httr); library(stringi); library(reshape2); library(tools)
library(formattable); library(DT)
#install.packages('shinyWidgets')

convert_inputs_to_emissions = data.frame(
  
  commodity = c("wood","heavy_fuel_oil","diesel","total_coal",
                  "natural_gas","uranium","propane"),
  conversion_factor = c(100.11,74.58,74.08,90.87,49.88,0.01,60.61)
  
)

convert_inputs_to_tj = data.frame(

  commodity = c("wood","heavy_fuel_oil","diesel","total_coal",
                "natural_gas","uranium","propane"),
  conversion_factor_tj = c(0.018,0.0425,0.0383,0.0264,0.0383,700,0.02531)
  
)

subject_matter_3 <- list.files(pattern = "*LoadingData.csv") %>% 
  lapply(read_csv) %>% 
  bind_rows %>%
  set_colnames(gsub(' ','_',names(.))) %>%
  mutate(indicator=case_when(!is.na(Fuel_consumed_for_electric_power_generation) ~ 'input',
                             !is.na(Cost_of_fuel_for_electric_power_generation) ~ 'price',
                             !is.na(Electricity_generated_from_fuels) ~ 'output'),
         commodity=case_when(!is.na(Fuel_consumed_for_electric_power_generation) ~ Fuel_consumed_for_electric_power_generation,
                             !is.na(Cost_of_fuel_for_electric_power_generation) ~ Cost_of_fuel_for_electric_power_generation,
                             !is.na(Electricity_generated_from_fuels) ~ Electricity_generated_from_fuels)) %>%
  select(REF_DATE,GEO,VALUE,indicator,commodity) %>% 
  mutate(VALUE = replace_na(VALUE, 0)) %>% 
  mutate(commodity = ifelse(commodity == 'Methane','Natural gas',ifelse(commodity == 'Light fuel oil','Diesel',commodity))) %>% 
  group_by(REF_DATE, GEO, indicator, commodity) %>% 
  mutate(VALUE = ifelse(indicator=='output',VALUE*0.0036,VALUE)) %>% 
  summarise(VALUE = sum(VALUE)) %>% 
  data.frame() %>% 
  set_colnames(c('year','province','indicator','commodity','value')) %>%
  filter(!(commodity %in% c('Total petroleum products'))) %>%
  mutate(commodity=ifelse(commodity=='Total heavy fuel oil','heavy fuel oil',commodity),
         commodity=tolower(gsub(' ','_',commodity))) %>%
  left_join(convert_inputs_to_emissions, by='commodity') %>%
  mutate(emission = value*conversion_factor/1000) %>% 
  left_join(convert_inputs_to_tj, by='commodity') %>% 
  mutate(value = ifelse(indicator=='input',value*conversion_factor_tj,value)) %>% 
  filter(indicator=='input') %>%
  select(-indicator,-conversion_factor,-value) %>%
  mutate(indicator='emission',
         variable=str_c(commodity,'_',indicator,'_',year)) %>%
  rename(value=emission)

subject_matter_1 <- list.files(pattern = "*LoadingData.csv") %>% 
  lapply(read_csv) %>% 
  bind_rows %>%
  set_colnames(gsub(' ','_',names(.))) %>%
  mutate(indicator=case_when(!is.na(Fuel_consumed_for_electric_power_generation) ~ 'input',
                             !is.na(Cost_of_fuel_for_electric_power_generation) ~ 'price',
                             !is.na(Electricity_generated_from_fuels) ~ 'output'),
         commodity=case_when(!is.na(Fuel_consumed_for_electric_power_generation) ~ Fuel_consumed_for_electric_power_generation,
                             !is.na(Cost_of_fuel_for_electric_power_generation) ~ Cost_of_fuel_for_electric_power_generation,
                             !is.na(Electricity_generated_from_fuels) ~ Electricity_generated_from_fuels)) %>%
  select(REF_DATE,GEO,VALUE,indicator,commodity) %>% 
  mutate(VALUE = replace_na(VALUE, 0)) %>% 
  mutate(commodity = ifelse(commodity == 'Methane','Natural gas',ifelse(commodity == 'Light fuel oil','Diesel',commodity))) %>% 
  group_by(REF_DATE, GEO, indicator, commodity) %>% 
  mutate(VALUE = ifelse(indicator=='output',VALUE*0.0036,VALUE)) %>% 
  summarise(VALUE = sum(VALUE)) %>% 
  data.frame() %>% 
  set_colnames(c('year','province','indicator','commodity','value')) %>%
  filter(!(commodity %in% c('Total petroleum products'))) %>%
  mutate(commodity=ifelse(commodity=='Total heavy fuel oil','heavy fuel oil',commodity),
         commodity=tolower(gsub(' ','_',commodity))) %>% 
  left_join(convert_inputs_to_tj, by='commodity') %>% 
  mutate(value = ifelse(indicator=='input',value*conversion_factor_tj,value)) %>% 
  mutate(variable=str_c(commodity,'_',indicator,'_',year)) %>%
  rbind(.,subject_matter_3) %>%
  select(province,variable,value) %>%
  spread(variable,value)

story_frame = data.frame(province=c(rep('Ontario',4)),
                        year=c(2005,2012,2013,2014),
                        commodity=rep('total_coal',4),
                        story=c('Lakeview generating station closure (Capacity of 2,400 MWh)',
                                'Atikokan generating station closure (Capacity of 211 MWh)',
                                'Lambton and Nanticoke generating station closures (Total Capacity of 6,920 MWh)',
                                'Thunder Bay generating station closure (Capacity of 306 MWh)')) %>%
  mutate(story=as.character(story))

subject_matter_2 <- list.files(pattern = "*LoadingData.csv") %>%
  lapply(read_csv) %>%
  bind_rows %>%
  set_colnames(gsub(' ','_',names(.))) %>%
  mutate(indicator=case_when(!is.na(Fuel_consumed_for_electric_power_generation) ~ 'input',
                             !is.na(Cost_of_fuel_for_electric_power_generation) ~ 'price',
                             !is.na(Electricity_generated_from_fuels) ~ 'output'),
         commodity=case_when(!is.na(Fuel_consumed_for_electric_power_generation) ~ Fuel_consumed_for_electric_power_generation,
                             !is.na(Cost_of_fuel_for_electric_power_generation) ~ Cost_of_fuel_for_electric_power_generation,
                             !is.na(Electricity_generated_from_fuels) ~ Electricity_generated_from_fuels)) %>%
  select(REF_DATE,GEO,VALUE,indicator,commodity) %>% 
  mutate(VALUE = replace_na(VALUE, 0)) %>% 
  mutate(commodity = ifelse(commodity == 'Methane','Natural gas',ifelse(commodity == 'Light fuel oil','Diesel',commodity))) %>% 
  group_by(REF_DATE, GEO, indicator, commodity) %>% 
  mutate(VALUE = ifelse(indicator=='output',VALUE*0.0036,VALUE)) %>% 
  summarise(VALUE = sum(VALUE)) %>% 
  data.frame() %>% 
  set_colnames(c('year','province','indicator','commodity','value')) %>%
  filter(commodity != 'Total petroleum products') %>%
  mutate(commodity=ifelse(commodity=='Total heavy fuel oil','heavy fuel oil',commodity),
         commodity=tolower(gsub(' ','_',commodity))) %>%
  left_join(convert_inputs_to_tj, by='commodity') %>% 
  mutate(value = ifelse(indicator=='input',value*conversion_factor_tj,value)) %>%
  rbind(.,subject_matter_3 %>% select(-variable)) %>%
  spread(indicator,value) %>%
  left_join(story_frame,by=c('province','commodity','year')) %>%
  mutate(price=price/input) %>% 
  mutate(price = replace_na(price, 0))
  

# read map
prov_map = readOGR('Canada/Canada.shp',stringsAsFactors = FALSE)
prov_map <- spTransform(prov_map, CRS("+proj=longlat +datum=WGS84"))

prov_centroids = gCentroid(prov_map,byid=TRUE) %>% 
  data.frame() %>% 
  mutate(NAME=prov_map$NAME)

prov_centroids[which(prov_centroids$NAME=='Nunavut'),c('x','y')] = c(-97,64)
prov_centroids[which(prov_centroids$NAME=='Northwest Territories'),c('x','y')] = c(-118.5,65)

prov_map %<>% 
  left_join(subject_matter_1, by=c('NAME'='province')) %>%
  left_join(prov_centroids, by=c('NAME'))


# create input vectors
years = 2005:2018
areas = prov_map@data$NAME
indicators = c("input","output","price",'emission','efficiency')
indicator_labels = c('Fuel consumed (TJ)', 'Electricity generated (TJ)', 'Cost/unit ($/TJ)', 'Emissions (tonnes of CO2e)','Efficiency')
commodities = c("wood","heavy_fuel_oil","diesel","total_coal","natural_gas")
commodity_labels = toTitleCase(gsub('_', ' ', commodities))
province_story=c('Ontario','Prince Edward Island','Nunavut')
story_title=c("Ontario eliminates coal","PEI wind electricity","Energy in the North")
story_description=c("In 2001, Ontario had 5 coal fired generating stations with a capacity of roughly 8,800 TJ. By 2014, all coal generating stations ceased operations to be replaced with a mixture of nuclear, natural gas fired, and non-hydro renewable plants. The Atikokan and Thunder Bay generating stations are now exclusively biomass based facilities",
                    "PEI has no sources of oil, natural gas, or other fuels used traditionally for electricity generation. Instead 99% of their electricity production comes from wind mills. However, wind production only is able to meet roughly 25% of PEI's demand for electricity. The remainder is imported from New Brunswick. There is an ideal wind speed for wind generated electricity.The wind needs to be fast enough to move the wind turbine (12-14 km/h), but not too strong that the turbines need to be shut down in order to protect them (roughly 90 km/h). The ideal wind speed to for the turbines to be at full capacity is between 50 to 60 km/h.",
                    "Unlike the rest of Canada where the major fuel used (except in transportation) is natural gas, the North runs on diesel. Energy options in the North are limited because there is no infrastructure in place that allows electricity to be imported from Southern Canada. All electricity consumed must be generated locally. In Nunavut, 100% of electricity generation comes from diesel where in Yukon the main type of electricity generation is hydro with diesel making up the difference. In some communities in the North unsubsidized electricity costs are 10 times that of the Canadian average on a per KWh basis whereas consumption is twice that national average.")
about_text <- c('This product would allow Canadians to compare over time and across provinces electricity generation by fuel types. Users are able to analyse the amount of energy used to produce electricity (inputs), the amount of electricity generated (outputs), the efficiency ratios, the fuel cost and emissions. This data is based on the Annual Survey of Electric Thermal Generating Station Fuel Consumption and can be found in the tables: Electricity from fuels, annual generation by electric utility thermal plants, Electric power generation, annual cost of fuel consumed by electric utility thermal plants and Electric power generation, annual fuel consumed by electric utility thermal plants. This survey estimates fuel consumption to generate electricity by utilities. This data was coupled with emission factors published by Environment and Climate Change Canada (hyperlink). The emissions by fuel type were calculated using these emission factors.')

