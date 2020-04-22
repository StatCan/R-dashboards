library(tidyverse)

dir.create(
  "/tmp/data/air",
  recursive=TRUE,
  showWarnings = FALSE
)

BUCKET="https://datasets.covid.cloud.statcan.ca/minio/public/air-quality/"
DATA="/tmp/data/air-pollution-dashboard/"

periods <- c("2020",
             "2019Q1",
             "2019Q2",
             "2019Q3",
             "2019Q4",
             "2018H1",
             "2017H1",
             "2016H1",
             "2015H1")

# Fetch the files
for (period in periods) {
    download.file(
        paste0(BUCKET, "data/air/", period, ".csv"),
        destfile=paste0(DATA, period, ".csv")
    )
}

species_names <- list("co" = "Carbon Monoxide",
                      "no2" = "Nitrogen Dioxide",
                      "so2" = "Sulfur Dioxide")

getData <- function(periods) {
  dfs <- list()
  for (period in periods) {
    df <- read_csv(paste0("/tmp/data/air-pollution-dashboard/", period, ".csv"), skip = 4)
    dfs[[period]] <- df 
  }
  return(dfs)
}


data <- getData(periods)
sample_df <- read_csv("/tmp/data/air-pollution-dashboard/2020.csv", skip = 4)
countries <- sort(unique(sample_df[["Country"]]))
cities <- sort(unique(sample_df[sample_df[["Country"]] == "CA", ][["Cities"]]))


colorful_plot <- function(df, df_compare, species, color) {
  df <- df() %>% filter(Specie == species)
  df_compare <- df_compare() %>% filter(Specie == species)
  
  year <- lubridate::year(df[["Date"]][1])
  
  ggplot() + 
    geom_line(data = df, aes(x = Date, y = median), color = color) +
    ylim(0, max(c(max(df[["median"]]), max(df_compare[["median"]])))) +
    labs(title = species_names[[species]],
         x = paste("Months from the Year: ", year),
         y = paste(species_names[[species]], "Levels")) +
    theme(plot.title = element_text(face="bold", size=16, hjust = 0.5))
}
