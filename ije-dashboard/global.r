# setwd('/home/jovyan/ije-shiny-2020/data/')
beginy = quote(2002)
endy = quote(2017)
#install.packages( pkgs = c("classInt"),type="binary", repos = "file:////fld6filer/packagerepo-depotprogiciel/miniCRAN" )
library(shiny)
library(tidyverse)
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

options(scipen = 999)

simple_pr_shapefile <- readRDS('./data/simple_pr_shapefile.RDS')
table_1_2 <- readRDS('./data/table_1_2.RDS')
table_3478 <- readRDS('./data/table_3478.RDS')
table_56910 <- readRDS('./data/table_56910.RDS')
table_11 <- readRDS('./data/table_11.RDS')


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
