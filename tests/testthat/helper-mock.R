local_mock_sec <- function(env = parent.frame()) {
    local_mocked_bindings(
        sec_get = function(url, useragent, as = "parsed") {
            path <- test_path("fixtures", "http", basename(url))
            if (!file.exists(path)) stop("No mock fixture for ", url)
            if (as == "raw") readBin(path, "raw", file.size(path))
            else jsonlite::read_json(path)
        },
        .env = env
    )
}
