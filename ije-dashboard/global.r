# setwd('/home/jovyan/ije-shiny-2020/data/')
beginy = quote(2002)
endy = quote(2017)
#install.packages( pkgs = c("classInt"),type="binary", repos = "file:////fld6filer/packagerepo-depotprogiciel/miniCRAN" )

library(shiny)
library(sf)
# library(rmapshaper)
library(leaflet)
# library(dplyr)
library(spData)
library(classInt)
library(sp)
# library(geojsonio)
library(rgeos)
# library(rgdal)
library(htmlwidgets)
# library(aws.s3)
library(RColorBrewer)
library(plotly)
library(tidyverse)

## necessary for shinyapps.io deployment
# library(BH)
# library(cpp11)
# library(Rcpp)

# setwd('~/ije-shiny-2020/ije-dashboard/')

options(scipen = 999)

simple_pr_shapefile <- readRDS('./data/simple_pr_shapefile.RDS')
table_1_2 <- readRDS('./data/table_1_2.RDS')
table_3478 <- readRDS('./data/table_3478.RDS') %>%
  mutate(industry=ifelse(industry=="Information and cultural industries; Finance and insurance;\n Real estate and rental and leasing; Management of companies and enterprise",'IFRM',industry))
table_56910 <- readRDS('./data/table_56910.RDS')
table_11 <- readRDS('./data/table_11.RDS')

## define lists for where they're necessary
provList <- c("Newfoundland and Labrador","Prince Edward Island","Nova Scotia","New Brunswick",
              "Quebec", "Ontario", "Manitoba","Saskatchewan","Alberta","British Columbia",
              "Yukon", "Northwest Territories","Nunavut")

indList <- c("Agriculture, forestry, fishing and hunting","Oil and gas extraction and support activities",
             "Mining and quarrying (excluding oil and gas)","Utilities","Construction","Manufacturing",
             "Wholesale and Retail trade","Transportation and warehousing",
             # "Information and cultural industries; Finance and insurance;\n Real estate and rental and leasing; Management of companies and enterprise",
             "IFRM",
             "Professional, scientific and technical services","Education services, health care and social assistance",
             "Accommodation and food services","Other services","Public administration","Unknown")

ageList <- c("18 to 24 years", "25 to 34 years", "35 to 44 years", "45 to 54 years",
             "55 to 64 years", "65 years and older")


createClasses <- function(data, palette, na_color, n) {
  classes <- classIntervals(na.exclude(data), n = n, style = "jenks")
  bins <- classes[["brks"]]
  pal <- colorBin(palette, domain = data, bins = bins, na.color = na_color)
  return <- list("pal" = pal, "bins" = bins)
}

pal <- colorNumeric("viridis", NULL)

# CREATE VARIABLE THAT STORES ALL DATA VALUES FOR DETERMINING BINNING AND PALETTE FOR CHOROPLETH MAP

#pal_count_PR <- createClasses(IJE_table1$count , "Blues", "transparent", 5)

## establish minio connection and list of s3_objects
# source('/home/jovyan/ije-shiny-2020/code/shiny/daaas_storage.R')
# daaas_storage.minimal()

# minio_filist <- get_bucket(bucket='shared',use_https=F,region='',prefix='david-wavrock/ije/')

## to save objects
# save_object('name-of-object',
#             bucket=minio_filist,use_https=F,region='')
