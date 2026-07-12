useragent <- "Your Name yourname@example.com"
test_that("get_company uses the cache and not a new download.", {
    skip("Working")
    cache_dir <- "fixtures/testcache"
    cik <- "1111111"
    expected <- "Test"
    result <- get_company(cik, useragent, cache_dir)
    expect_equal(result, expected)
})

test_that("get_company downloads a new sit if cache does not exist.", {
    skip("Working")
    cache_dir <- "./testcache"
    cik <- "320193"
    expected <- "Apple Inc."
    result <- get_company(cik, useragent, cache_dir)
    test_cik <- "1111111"
    test <- get_company(test_cik, useragent, cache_dir)
    expect_equal(result, expected)
    expect_true(is.na(test))
    unlink(cache_dir, recursive = TRUE)
})
