#' Retrieve Company Information
#'
#' @param cik the company CIK (padded or unpadded)
#' @param useragent required by SEC: "Name email@domain.com"
#' @param cache cache directory
#' @return the company name, or NA if the CIK isn't found.
#' @export
get_company <- function(cik, useragent, cache="./.cache") {
    tickers <- file.path(cache, "tickers.csv")
    df <- if (file.exists(tickers)) {
        readr::read_csv(tickers, na = "", show_col_types = FALSE)
    } else {
        download_tickers(useragent, cache)
    }
    cik <- sub("^0+", "", cik)
    entry <- df[df$cik_str == cik,]$title
    if (length(entry) == 0) return(NA_character_)
    entry[1]
}
