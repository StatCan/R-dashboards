library(shiny)
library(tidyverse)
library(circlize)
library(shiny.i18n)
library(Cairo)
options(shiny.usecairo=T)
#library(shinycssloaders)


# File with translations
i18n <- Translator$new(translation_csvs_path = "dictionary/")
i18n$set_translation_language("en") # the default translation to display
