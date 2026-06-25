#' Download the JSON file of ticker to cik numbers from EDGAR
#'
#' @param useragent, Character. Required by SEC: "Name email@domain.com"
#' @param cache, Character. Temporary cache for storing the tickers as a
#'   csv file.
#' @importFrom readr write_csv
#' @importFrom httr GET user_agent stop_for_status content
#' @importFrom dplyr bind_rows
#' @return data.frame containing the downloaded list of tickers to cik.
download_tickers <- function(useragent, cache="./.cache") {
    message("Downloading tickers...")

    # Create the cache directory if it doesn't exist.
    if (!dir.exists(cache)) dir.create(cache)

    # Ticker json url
    url <- "https://www.sec.gov/files/company_tickers.json"

    # Download the JSON
    response <- httr::GET(url, httr::user_agent(useragent))
    httr::stop_for_status(response)

    # Convert to a csv file to store and retrieve later.
    df <- do.call(dplyr::bind_rows,
                  lapply(httr::content(response), as.data.frame))
    readr::write_csv(df, paste0(cache, "/tickers.csv"), na = "")
    return (df)
}
