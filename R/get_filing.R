#' Build the URL for a filing's sgml file.
#'
#' @noRd
#' @param cik the company CIK (with or without leading zeros).
#' @param accession the accession number in dashed format (e.g.
#' "0000320193-23-000106").
#' @return a character string URL.
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
#' @param cik the company CIK
#' @param accession the accession number in dashed format
#' @param useragent required by SEC: "Name email@domain.com"
#' @param cache cache directory
#' @return character vector of local file paths, one per accession
#' @export
get_filing <- function(cik, accession, useragent, cache = "./.cache") {
    # Normalize so padded and unpadded CIKs share one cache folder.
    cik  <- sub("^0+", "", cik)
    urls <- filing_urls(cik, accession)
    file_paths <- file.path(cache, "filings", cik, paste0(accession, ".txt"))
    for (d in unique(dirname(file_paths)))
        if (!dir.exists(d)) dir.create(d, recursive = TRUE)
    for (i in seq_along(urls)) {
        if (!file.exists(file_paths[i])) {
            message(paste0("Downloading filing: ", accession[i]))
            content <- sec_get(urls[i], useragent, as = "raw")
            con <- file(file_paths[i], "wb")
            writeBin(content, con)
            close(con)
        }
    }
    file_paths
}
