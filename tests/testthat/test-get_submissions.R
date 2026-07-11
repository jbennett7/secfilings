useragent <- "Joseph Bennett jbennett@jbennettconsulting.com"
test_that("get_submissions can get the submissions list", {
    skip("Working")
    ticker <- 'PLTR'
    cik <- '0001321655'
    test_cache <- "./testcache"
    # accessionNumber
    test_instance <- "0001823944-26-000007"
    df <- get_submissions(cik, useragent, cache = test_cache)
    result <- names(df)
    unlink(test_cache, recursive=TRUE)
})
