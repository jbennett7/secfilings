#' Build the URL for a filing's sgml file.
#'
#' @param cik Character. The company CIK (with or without leading zeros).
#' @param accession Character. The accession number in dashed format
#'   (e.g. "0000320193-23-000106").
#' @return A character string URL.
filing_urls <- function(cik, accession) {
    cik_clean <- sub("^0+", "", cik)
  
    paste0(
      "https://www.sec.gov/Archives/edgar/data/",
      cik_clean, "/",
      accession, ".txt"
    )
}

#' Retrieve an SGML filing from EDGAR
#'
#' @param cik, Character. The company CIK.
#' @param accession, Character. The accession number in dashed format.
#' @param useragent, Character. Required by SEC: "Name email@domain.com".
#' @param cache, Character. Temporary cache for storing the tickers as a
#'   csv file.
#' @importFrom httr GET user_agent stop_for_status content
#' @return A data frame with columns: sequence, filename, type, description, size.
#' @export
get_filings <- function(cik, accession, useragent, cache = "./.cache") {
    # Generate the urls to retrieve the sgml files.
    urls <- filing_urls(cik, accession)
    # The path to store the SGML file.
    file_paths <- paste(cache, "filings", cik, paste0(accession, ".txt"), sep="/")
    if (!dir.exists(unique(dirname(file_paths))))
        dir.create(unique(dirname(file_paths)), recursive=TRUE)
    for (i in seq_along(urls)) {
        if (!file.exists(file_paths[i])) {
            message(paste0("Downloading filing: ", accession))
            response <- httr::GET(urls[i], httr::user_agent(useragent))
            httr::stop_for_status(response)
            con <- file(file_paths[i], "wb")
            writeBin(httr::content(response, as = "raw"), con)
            close(con)
        }
    }
    return (file_paths)
}
