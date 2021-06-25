************************************************************************
              INTER-JURISDICTIONAL EMPLOYMENT DASHBOARD
            Prepared by Zhe (River) Yang and Dave Wavrock
************************************************************************
Note: Data is cleaned from original vintage IJE tables using a precursor
script `Prepare Data.R` which is not included in this repository.

The purpose of this dashboard is to present data on Inter-Jurisdictional
Employment in Canada, and to serve as a companion to existing data 
tables to better help federal and provincial clients interpret and 
explore the data.

This repository includes the following files:

`global.R` - Loads data and sets global parameters and libraries

`server.R` - Standard Shiny server function; includes back end script
for generating reactive and interactive data visuals based on user
selections

`ui.R` - Standard Shiny UI function; includes front end script for
laying out plots and variable selection, and provides embedded help 
and information in the form of footnotes.

`style.css` - Custom CSS styling for app; used in lieu of built-in
style formats (originally the `darkly` theme) which were found to
have inconsistent font formatting. Replacing this file with built-in
themes in the UI script is not recommended.



