#' Retrieve the CIK for the given ticker symbol.
#'
#' @param ticker, Character. Ticker symbol.
#' @param useragent, Character. Required by SEC: "Name email@domain.com"
#' @param cache, Character. Temporary cache for storing the tickers as a
#'   csv file.
#' @return 10 character cik string.
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
