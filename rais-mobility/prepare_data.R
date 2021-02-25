library(tidyverse)

table_number <- "37100154"

temp <- tempfile()
# download.file("https://www150.statcan.gc.ca/n1/tbl/csv/37100154-eng.zip",temp,mode="wb")
url <- paste("https://www150.statcan.gc.ca/n1/tbl/csv/", table_number, "-eng.zip", sep="")
download.file(url, temp, mode="wb")

orig <- unz(temp, paste(table_number, ".csv", sep = "")) %>%
  read_csv(    
    col_types = cols_only(
      REF_DATE = col_integer(),
      COORDINATE = col_character(),
      VALUE = col_number())
  ) %>%
  filter(COORDINATE != "")
unlink(temp)

# split the COORDINATE into separate variables
dims <- c("from", "dim_trad", "dim_years", "to")

df <- orig %>%
  separate(col=COORDINATE, into=dims)
df[, dims] <- sapply(df[, dims], as.numeric)

# drop indicators that are not used in the plot
# and create destination code
out_df <- df %>%
  filter(to > 4) %>%
  mutate(to = to - 4, dim_mode = 1, dim_type = 1) %>%
  filter(!is.na(VALUE) & VALUE > 0)

dims <- c("from", "dim_trad", "dim_mode", "dim_years", "dim_type", "to")

write_csv(out_df[, c("REF_DATE", "VALUE", dims)],
          file="data/mig_mat.csv")