local_mock_sec <- function(env = parent.frame()) {
    local_mocked_bindings(
        sec_get = function(url, useragent, as = "parsed") {
            # companyfacts and submissions URLs share the basename
            # CIK##########.json, so companyfacts fixtures live in a subfolder.
            dir <- if (grepl("/api/xbrl/companyfacts/", url))
                test_path("fixtures", "http", "companyfacts")
            else
                test_path("fixtures", "http")
            path <- file.path(dir, basename(url))
            if (!file.exists(path)) stop("No mock fixture for ", url)
            if (as == "raw") readBin(path, "raw", file.size(path))
            else jsonlite::read_json(path)
        },
        .env = env
    )
}
