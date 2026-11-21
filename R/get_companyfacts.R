#' Download company facts from SEC
#'
#' @param cik the company CIK.
#' @param useragent required by SEC: "Name email@domain.com"
#' @param cache cache directory
#' @param refresh if TRUE, download again even when a cached copy exists
#' @return a tibble with one row per fact entry. Values repeated in later
#'   filings are not deduplicated.
#' @export
get_companyfacts <- function(cik, useragent, cache = "./.cache", refresh = FALSE) {
    if (is.na(cik)) stop("cik is NA (ticker lookup failed?)")
    cik <- pad_cik(cik)
    file_path <- file.path(cache, "companyfacts", paste0("CIK", cik, ".json"))
    if (!(file.exists(file_path)) || refresh) {
        message("Downloading companyfacts: ", cik)
        dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
        url <- paste0("https://data.sec.gov/api/xbrl/companyfacts/CIK", cik, ".json")
        content <- sec_get(url, useragent, as = "raw")
        writeBin(content, file_path)
    }
    read_companyfacts(file_path)
}

#' Parse a cached companyfacts JSON file into one row per fact entry
#'
#' @noRd
#' @param path path to the cached JSON file
read_companyfacts <- function(path) {
    x <- jsonlite::fromJSON(path)
    rows <- list()
    for (taxonomy in names(x$facts))
        for (tag in names(x$facts[[taxonomy]]))
            for (unit in names(x$facts[[taxonomy]][[tag]]$units)) {
                df <- x$facts[[taxonomy]][[tag]]$units[[unit]]
                df$taxonomy <- taxonomy
                df$tag <- tag
                df$unit <- unit
                rows[[length(rows) + 1]] <- df
            }
    facts <- dplyr::bind_rows(rows)
    for (col in c("start", "frame"))
        if (!col %in% names(facts)) facts[[col]] <- NA_character_
    facts |> tibble::as_tibble() |>
        dplyr::mutate(
            start = as.Date(start),
            end   = as.Date(end),
            filed = as.Date(filed),
            fy    = as.integer(fy)
        ) |>
        dplyr::select(taxonomy, tag, unit, start, end, val, accn,
                      fy, fp, form, filed, frame)
}
