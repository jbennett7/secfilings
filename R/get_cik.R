#' Retrieve the CIK for the given ticker symbol.
#'
#' @param ticker ticker symbol
#' @param useragent required by SEC: "Name email@domain.com"
#' @param cache cache directory
#' @return the 10-digit zero-padded CIK, or NA if the ticker isn't found
#' @export
get_cik <- function(ticker, useragent, cache="./.cache") {
    tickers <- file.path(cache, "tickers.csv")
    df <- if (file.exists(tickers)) {
        readr::read_csv(tickers, na = "", show_col_types = FALSE)
    } else {
        download_tickers(useragent, cache)
    }
    # Convert ticker symbol to all UPPER case characters.
    ticker <- toupper(ticker)
    # If the entry isn't in the data frame, return NA.
    entry <- df[df$ticker == ticker,]
    if (nrow(entry) == 0) return(NA_character_)
    # Return the cik with leading zeros to make 10 character string.
    return(sprintf("%010d", as.integer(entry$cik_str)))
}
