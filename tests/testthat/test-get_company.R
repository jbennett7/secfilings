test_that("get_company uses the cache and not a new download.", {
    cache_dir <- test_path("fixtures", "testcache")
    expect_equal(get_company("1111111", "offline-test", cache_dir), "Test")
})

test_that("get_company accepts a zero-padded CIK", {
    cache_dir <- test_path("fixtures", "testcache")
    expect_equal(get_company("0001111111", "offline-test", cache_dir), "Test")
})

test_that("get_company returns NA for an unknown CIK", {
    cache_dir <- test_path("fixtures", "testcache")
    expect_identical(get_company("99999999", "offline-test", cache_dir), NA_character_)
})

test_that("get_company returns one title for a multi-ticker company", {
    cache_dir <- test_path("fixtures", "testcache")
    expect_identical(get_company("0000732717", "offline-test", cache_dir), "AT&T INC.")
})

test_that("get_company downloads a new set if cache does not exist.", {
    useragent <- sec_useragent()
    cache_dir <- withr::local_tempdir()
    result <- get_company("320193", useragent, cache_dir)
    expect_equal(result, "Apple Inc.")
})

