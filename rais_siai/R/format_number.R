format_number <- function(number, locale = "en") {
  # format number: add thousand separator and decimal points based on the locale
  
  if (locale == "fr") {
    k_delim <- " "
    d_delim <- ","
  } else {
    k_delim <- ","
    d_delim <- "."
  }
  
  ifelse(is.na(number), 
         out <- NA, 
         out <- prettyNum(number, big.mark=k_delim, decimal.mark=d_delim, 
                          scientific=FALSE))
  
  return(out)
}
