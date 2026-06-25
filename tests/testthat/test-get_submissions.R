useragent <- "Joseph Bennett jbennett@jbennettconsulting.com"
test_that("get_submissions can get the submissions list", {
    skip("Working")
    ticker <- 'PLTR'
    cik <- '0001321655'
    test_cache <- "./testcache"
    # accessionNumber
    test_instance <- "0001823944-26-000007"
    # filingDate
    expected <- "2026-06-08"
    get_submissions(ticker, useragent, cache = test_cache)
    df <- readr::read_csv(paste0(test_cache, "/submissions/", cik, ".csv"), na = "")
    expect_equal(df[df$accessionNumber == test_instance,]$filingDate, as.Date(expected))
    unlink(test_cache, recursive=TRUE)
})
