#' Download the JSON file of ticker to cik numbers from EDGAR
#'
#' @param useragent, Character. Required by SEC: "Name email@domain.com"
#' @param cache, Character. Temporary cache for storing the tickers as a
#'   csv file.
#' @return data.frame containing the downloaded list of tickers to cik.
download_tickers <- function(useragent, cache="./.cache") {
    message("Downloading tickers...")

    # Create the cache directory if it doesn't exist.
    if (!dir.exists(cache)) dir.create(cache)

    # Ticker json url
    url <- "https://www.sec.gov/files/company_tickers.json"

    # Download the JSON
    content <- sec_get(url, useragent)

    # Convert to a csv file to store and retrieve later.
    df <- do.call(dplyr::bind_rows,
                  lapply(content, as.data.frame))

    # Write to file and return data.frame.
    readr::write_csv(df, paste0(cache, "/tickers.csv"), na = "")
    return (df)
}
