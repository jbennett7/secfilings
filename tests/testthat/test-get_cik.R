test_that("get_cik uses the cache and not a new download.", {
    cache_dir <- test_path("fixtures", "testcache")
    expect_equal(get_cik("NOCMP", "offline-test", cache_dir), "0001111111")
})

test_that("get_cik is case-insensitive", {
    cache_dir <- test_path("fixtures", "testcache")
    expect_equal(get_cik("nocmp", "offline-test", cache_dir), "0001111111")
})

test_that("get_cik returns NA for an unknown ticker", {
    cache_dir <- test_path("fixtures", "testcache")
    expect_identical(get_cik("ZZZZZ", "offline-test", cache_dir), NA_character_)
})

test_that("get_cik resolves each class of a multi-class company", {
    cache_dir <- test_path("fixtures", "testcache")
    expect_equal(get_cik("BRK-B", "offline-test", cache_dir), "0001067983")
})

test_that("get_cik downloads a new set if cache does not exist.", {
    useragent <- sec_useragent()
    cache_dir <- withr::local_tempdir()
    cik <- get_cik('AAPL', useragent, cache_dir)
    expected <- '0000320193'
    expect_equal(cik, expected)
})

