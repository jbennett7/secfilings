useragent <- "Joseph Bennett jbennett@jbennettconsulting.com"

test_that("download_submissions downloads a clean copy.", {
    skip("Working")
    cache_dir <- "./testcache"
    cik <- '0001321655'
    expected <- '0001823952-26-000016'
    csv_file <- paste0(cache_dir, "/submissions/", cik, ".csv")
    df <- download_submissions(cik, useragent, cache_dir)
    expect_equal(as.character(df[1,]$accessionNumber), expected)
    expect_true(file.exists(csv_file))
    df <- readr::read_csv(csv_file)
    expect_equal(as.character(df[1,]$accessionNumber), expected)
    unlink(cache_dir, recursive=TRUE)
})
