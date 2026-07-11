useragent <- "Joseph Bennett jbennett@jbennettconsulting.com"

test_that("download_submissions downloads a clean copy.", {
    skip("Working")
    cache_dir <- "./testcache"
    cik <- '0001321655'
    expected <- '0001823952-26-000016'
    csv_file <- paste0(cache_dir, "/submissions/", cik, ".csv")
    df <- download_submissions(cik, useragent, cache_dir)
    unlink(cache_dir, recursive=TRUE)
})
