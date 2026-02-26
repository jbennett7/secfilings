#' Build the URL for a filing's JSON index on SEC EDGAR
#'
#' @param cik Character. The company CIK (with or without leading zeros).
#' @param accession Character. The accession number in dashed format
#'   (e.g. "0000320193-23-000106").
#' @return A character string URL.
filing_index_url <- function(cik, accession) {
  cik_clean        <- sub("^0+", "", cik)
  accession_nodash <- gsub("-", "", accession)

  paste0(
    "https://www.sec.gov/Archives/edgar/data/",
    cik_clean, "/",
    accession_nodash, "/",
    accession, "-index.json"
  )
}

#' Parse a filing index JSON response into a data frame
#'
#' @param json_text Character. The raw JSON string from the EDGAR index endpoint.
#' @return A data frame with columns: sequence, filename, type, description, size.
parse_filing_index <- function(json_text) {
  parsed <- jsonlite::fromJSON(json_text)
  docs   <- parsed$documents

  data.frame(
    sequence    = docs$sequence,
    filename    = docs$document,
    type        = docs$type,
    description = docs$description,
    size        = docs$size,
    stringsAsFactors = FALSE
  )
}

#' Retrieve the filing index for a specific SEC EDGAR submission
#'
#' @param cik Character. The company CIK.
#' @param accession Character. The accession number in dashed format.
#' @param useragent Character. Required by SEC: "Name email@domain.com".
#' @return A data frame with columns: sequence, filename, type, description, size.
#' @export
get_filing_index <- function(cik, accession, useragent) {
  url      <- filing_index_url(cik, accession)
  response <- httr::GET(url, httr::user_agent(useragent))
  httr::stop_for_status(response)
  parse_filing_index(httr::content(response, as = "text", encoding = "UTF-8"))
}
