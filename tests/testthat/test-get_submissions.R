test_that("get_submissions uses the cache and not a new download.", {
    cache_dir <- test_path("fixtures", "testcache")
    df <- get_submissions("0001111111", "offline-test", cache_dir)
    # The line with this accession number was altered so it can be unique.
    result <- df$primaryDocDescription[df$accessionNumber == "0001234567-11-000001"]
    expect_equal(result, "TEST FORM")
})

test_that("get_submissions errors on an NA CIK", {
    cache_dir <- test_path("fixtures", "testcache")
    expect_error(get_submissions(NA_character_, "offline-test", cache_dir),
                 "cik is NA \\(ticker lookup failed\\?\\)")
})

test_that("get_submissions downloads when not cached.", {
    useragent <- sec_useragent()
    cache_dir <- withr::local_tempdir()
    df <- get_submissions("0001321655", useragent, cache_dir)
    result <- df$primaryDocDescription[df$accessionNumber == "0001823944-26-000007"]
    expect_equal(result, "FORM 4")
})

test_that("get_submissions returns identical data downloaded or cached", {
    local_mock_sec()
    cache_dir <- withr::local_tempdir()
    first  <- get_submissions("0002222222", "offline-test", cache_dir)
    cached <- get_submissions("0002222222", "offline-test", cache_dir)
    expect_identical(first, cached)
})

test_that("get_submissions uses the declared column types", {
    cache_dir <- test_path("fixtures", "testcache")
    df <- get_submissions("0001111111", "offline-test", cache_dir)
    expect_s3_class(df$filingDate, "Date")
    expect_s3_class(df$acceptanceDateTime, "POSIXct")
    expect_type(df$filmNumber, "character")
    expect_type(df$items, "character")
    expect_type(df$isXBRL, "logical")
})
