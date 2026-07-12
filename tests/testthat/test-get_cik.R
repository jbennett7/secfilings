useragent <- "Your Name yourname@example.com"
test_that("get_cik uses the cache and not a new download.", {
    skip("Working")
    cache_dir <- "fixtures/testcache"
    cik <- get_cik('NOCMP', useragent, cache_dir)
    expected <- '0001111111'
    expect_equal(cik, expected)
})

test_that("get_cik downloads a new set if cache does not exist.", {
    skip("Working")
    cache_dir <- "./testcache"
    cik <- get_cik('AAPL', useragent, cache_dir)
    expected <- '0000320193'
    expect_equal(cik, expected)
    unlink(cache_dir, recursive = TRUE)
})
