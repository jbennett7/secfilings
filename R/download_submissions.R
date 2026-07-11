#' Download the JSON file for the company submissions for the cik.
#'
#' @param cik, Character. The CIK of the company.
#' @param useragent, Character. Required by SEC: "Name email@domain.com"
#' @param cache, Character. Temporary cache for storing the tickers as a
#'   csv file.
#' @importFrom tibble as.tibble
#' @importFrom httr RETRY user_agent stop_for_status content
#' @return data.frame containing the downloaded submissions.
download_submissions <- function(cik, useragent, cache="./.cache") {
    message("Downloading submissions: ", cik)

    # Target csv
    submissions <- paste0(cache, "/submissions/", cik, ".csv")

    # Create the cache directory if it doesn't exist.
    if (!dir.exists(dirname(submissions)))
        dir.create(dirname(submissions), recursive=TRUE)

    # Submission url
    url <- paste0("https://data.sec.gov/submissions/CIK", cik, ".json")

    # Download the submission JSON.
    response <- httr::RETRY("GET", url, httr::user_agent(useragent),
                             times = 5, pause_base = 1, pause_cap = 30)
    httr::stop_for_status(response)
    content <- httr::content(response)
    df <- data.frame(lapply(content$filings$recent, function(x) {
        vapply(x, function(el)
            if (is.null(el)) NA_character_
            else as.character(el[[1]]), character(1))
    }))
    extras <- bind_rows(content$filings$files)
    suppressWarnings({for (file in extras$name) {
        url <- paste0("https://data.sec.gov/submissions/", file)
        response <- httr::RETRY("GET", url, httr::user_agent(useragent),
                                times = 5, pause_cap = 30)
        httr::stop_for_status(response)
        content <- httr::content(response)
        temp <- data.frame(lapply(content,
                         function(x) {
            vapply(x, function(el)
                if (is.null(el)) NA_character_
                else as.character(el[[1]]), character(1))
        }))
        df <- bind_rows(df, temp)
    }})
    
    # Save to file and return data.frame.
    readr::write_csv(df, submissions, na = "")
    return(tibble::as_tibble(df))
}
