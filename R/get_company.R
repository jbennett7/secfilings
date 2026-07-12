#' @export
get_company <- function(cik, useragent, cache="./.cache") {
    df <- tryCatch({
        readr::read_csv(paste0(cache, "/tickers.csv"), na = "")
    }, error = function(e) {
        download_tickers(useragent, cache)
    })
    cik <- sub("^0+", "", cik)
    entry <- df[df$cik_str == cik,]$title
    if (length(entry) == 0) return(NA_character_)
    return(entry)
}
