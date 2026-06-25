#' Retrieve the JSON document of company filing submissions to EDGAR
#'
#' @param ticker, Character. Ticker symbol of the company.
#' @param useragent, Character. Required by SEC: "Name email@domain.com"
#' @param cache, Character. Temporary cache for storing the tickers as a
#'   csv file.
#' @importFrom httr GET user_agent stop_for_status content
#' @importFrom readr write_csv read_csv
#' @return data.frame, Character. The submission list in data frame format.
#' @export
get_submissions <- function(ticker, useragent, cache = "./.cache") {
    cik <- get_cik(ticker, useragent)
    if (is.na(cik)) stop("Could not get cik for ticker.")
    submissions <- paste0(cache, "/submissions/", cik, ".csv")
    if (!dir.exists(dirname(submissions))) dir.create(dirname(submissions), recursive=TRUE) 
    if (!file.exists(submissions)) {
        message(paste0("Downloading submissions for ", toupper(ticker), "..."))
        url <- paste0("https://data.sec.gov/submissions/CIK", cik, ".json")
        response <- httr::GET(url, httr::user_agent(useragent))
        httr::stop_for_status(response)
        content <- httr::content(response)
        df <- data.frame(lapply(content$filings$recent, function(x) {
            vapply(x, function(el)
                if (is.null(el)) NA_character_
                else as.character(el[[1]]), character(1))
        }))
        readr::write_csv(df, submissions, na = "")
        return(df)
    } else {
        df <- readr::read_csv(submissions, na = "")
        return(df)
    }
}

# Submission data frame variables
# [1] "accessionNumber"       "filingDate"            "reportDate"
# [4] "acceptanceDateTime"    "act"                   "form"
# [7] "fileNumber"            "filmNumber"            "items"
#[10] "core_type"             "size"                  "isXBRL"
#[13] "isInlineXBRL"          "isXBRLNumeric"         "primaryDocument"
#[16] "primaryDocDescription"

