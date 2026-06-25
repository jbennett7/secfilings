useragent <- "Joseph Bennett jbennett@jbennettconsulting.com"

test_that("get_filing downloads a clean copy.", {
    skip("Working")
    test_cache <- "./testcache"
    cik <- "1321655"
    accession <- "0001321655-23-000044"
    expected <- "20230331"
    file_path <- get_filings(cik, accession, useragent, test_cache)
    expect_true(file.exists(file_path))
    lines <- readLines(file_path)
    result <- sub("^.+([0-9]{8})$", "\\1", lines[grepl("CONFORMED PERIOD OF REPORT", lines)])
    expect_equal(result, expected)
})
