useragent <- "Your Name yourname@example.com"
test_that("get_submission uses the cache and not a new download.", {
    skip("Working")
    cik <- "0001111111"
    test_cache <- "fixtures/testcache"
    # The line with this accession number was altered so it can be unique.
    accession <- "0001234567-11-000001"
    expected <- "TEST FORM"
    df <- get_submissions(cik, useragent, cache = test_cache)
    result <- df[df$accessionNumber == accession,]$primaryDocDescription
    expect_true(result == expected)
})

test_that("get_submissions downloads when not cached.", {
    skip("Working")
    cik <- '0001321655'
    test_cache <- "./testcache"
    accession <- "0001823944-26-000007"
    expected <- "FORM 4"
    df <- get_submissions(cik, useragent, cache = test_cache)
    result <- df[df$accessionNumber == accession,]$primaryDocDescription
    unlink(test_cache, recursive=TRUE)
    expect_true(result == expected)
})
