useragent <- "Joseph Bennett jbennett@jbennettconsulting.com"
test_that("get_company can get the correct cik", {
    skip("Working")
    cache_dir <- "./testcache"
    cik <- "0001321655"
    expected <- "Palantir Technologies Inc."
    result <- get_company(cik)
    expect_equal(result, expected)
    unlink(cache_dir, recursive=TRUE)
})

