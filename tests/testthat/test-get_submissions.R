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

test_that("pad_cik will always return padded cik", {
    expect_equal(pad_cik("732717"), "0000732717")
    expect_equal(pad_cik("0000732717"), "0000732717")
})

test_that("get_submissions an unpadded CIK hits the cache (offline)", {
    cache_dir <- test_path("fixtures", "testcache")
    # A padded or unpadded cik will return the same rows.
    unpadded <- get_submissions("1111111", "offline-test", cache_dir)
    padded   <- get_submissions("0001111111", "offline-test", cache_dir)
    expect_identical(unpadded, padded)
})

test_that("get_submissions an unpadded CIK downloads from the padded URL", {
    local_mock_sec()
    cache_dir <- withr::local_tempdir()
    df <- get_submissions("2222222", "offline-test", cache_dir)
    result <- df$primaryDocDescription[df$accessionNumber == "0002222222-26-000003"]
    expect_equal(result, "10-Q")
    expect_true(file.exists(file.path(cache_dir, "submissions", "0002222222.csv")))
})
