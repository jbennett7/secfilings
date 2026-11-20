#' Retrieve the company EDGAR submissions.
#'
#' @param cik the company CIK.
#' @param useragent required by SEC: "Name email@domain.com"
#' @param cache cache directory
#' @return a tibble with one row per filing
#' @export
get_submissions <- function(cik, useragent, cache = "./.cache") {
    if (is.na(cik)) stop("cik is NA (ticker lookup failed?)")
    cik <- pad_cik(cik)
    submissions <- paste0(cache, "/submissions/", cik, ".csv")
    if (!file.exists(submissions)) {
        return(download_submissions(cik, useragent, cache))
    } else {
        read_submissions(submissions)
    }
}

#' Read a submission file by path
#'
#' @noRd
read_submissions <- function(filepath) {
    tibble::as_tibble(readr::read_csv(filepath,
        na = "",
        show_col_types=FALSE,
        col_types = readr::cols(
            accessionNumber = readr::col_character(),
            act = readr::col_character(),
            form = readr::col_character(),
            fileNumber = readr::col_character(),
            filmNumber = readr::col_character(),
            items = readr::col_character(),
            core_type = readr::col_character(),
            primaryDocument = readr::col_character(),
            primaryDocDescription = readr::col_character(),
            filingDate = readr::col_date(),
            reportDate = readr::col_date(),
            acceptanceDateTime = readr::col_datetime(),
            size = readr::col_double(),
            isXBRL = readr::col_logical(),
            isInlineXBRL = readr::col_logical(),
            isXBRLNumeric = readr::col_logical()
        )
    ))
}

#' Zero-pad a CIK to the 10 digits SEC's submissions API requires
#'
#' @noRd
#' @param cik the company CIK (padded or unpadded)
pad_cik <- function(cik) formatC(as.numeric(cik), width = 10, flag = "0", format = "d")
