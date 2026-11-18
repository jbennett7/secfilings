#' sec_get
#'
#' @noRd
#' @param url SEC url
#' @param useragent SEC mandatory useragent
#' @param as parsed or raw
#' @return content
sec_get <- function(url, useragent, as = "parsed") {
    response <- httr::RETRY("GET", url, httr::user_agent(useragent),
                            times = 5, pause_base = 1, pause_cap = 30)
    httr::stop_for_status(response)
    httr::content(response, as = as)
}
