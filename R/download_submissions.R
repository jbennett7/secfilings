#' Download the JSON file for the company submissions for the cik.
#'
#' @param cik, Character. The CIK of the company.
#' @param useragent, Character. Required by SEC: "Name email@domain.com"
#' @param cache, Character. Temporary cache for storing the tickers as a
#'   csv file.
#' @return A tibble with one row per filing
download_submissions <- function(cik, useragent, cache="./.cache") {
    message("Downloading submissions: ", cik, "\n")

    # Target csv
    submissions <- paste0(cache, "/submissions/", cik, ".csv")

    # Create the cache directory if it doesn't exist.
    if (!dir.exists(dirname(submissions)))
        dir.create(dirname(submissions), recursive=TRUE)

    # Submission url
    url <- paste0("https://data.sec.gov/submissions/CIK", cik, ".json")

    # Download the submission JSON.
    content <- sec_get(url, useragent)
    df <- data.frame(lapply(content$filings$recent, function(x) {
        vapply(x, function(el)
            if (is.null(el)) NA_character_
            else as.character(el[[1]]), character(1))
    }))
    extras <- dplyr::bind_rows(content$filings$files)
    suppressWarnings({for (file in extras$name) {
        url <- paste0("https://data.sec.gov/submissions/", file)
        content <- sec_get(url, useragent)
        temp <- data.frame(lapply(content,
                         function(x) {
            vapply(x, function(el)
                if (is.null(el)) NA_character_
                else as.character(el[[1]]), character(1))
        }))
        df <- dplyr::bind_rows(df, temp)
    }})
    
    readr::write_csv(df, submissions, na = "")
    return(read_submissions(submissions))
}
