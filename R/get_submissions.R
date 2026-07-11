#' Retrieve the company EDGAR submissions.
#'
#' @param ticker, Character. Ticker symbol of the company.
#' @param useragent, Character. Required by SEC: "Name email@domain.com"
#' @param cache, Character. Temporary cache for storing the tickers as a
#'   csv file.
#' @importFrom readr write_csv read_csv cols col_character
#' @return data.frame, Character. The submission list in data frame format.
#' @export
get_submissions <- function(cik, useragent, cache = "./.cache") {
    if (is.na(cik)) stop("Could not get cik for ticker.")
    submissions <- paste0(cache, "/submissions/", cik, ".csv")
    if (!file.exists(submissions)) {
        return(download_submissions(cik, useragent, cache))
    } else {
        suppressMessages(readr::read_csv(submissions,
                        na = "",
                        show_col_types=FALSE,
                        # act = "NE" is a legitimate SEC data,
                        #   but read_csv guesses act as numeric.
                        col_types = readr::cols(act = readr::col_character())) |>
        mutate(filingDate = as.character(filingDate)))
    }
}

# Submission data frame variables
# [1] "accessionNumber"       "filingDate"            "reportDate"
# [4] "acceptanceDateTime"    "act"                   "form"
# [7] "fileNumber"            "filmNumber"            "items"
#[10] "core_type"             "size"                  "isXBRL"
#[13] "isInlineXBRL"          "isXBRLNumeric"         "primaryDocument"
#[16] "primaryDocDescription"

