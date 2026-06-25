useragent <- "Joseph Bennett jbennett@jbennettconsulting.com"
test_that("get_cik can get the correct cik", {
    skip("Working")
    cache_dir <- "./testcache"
    cik <- get_cik('PLTR', useragent, cache_dir)
    expected <- '0001321655'
    expect_equal(cik, expected)
    unlink(cache_dir, recursive=TRUE)
})
