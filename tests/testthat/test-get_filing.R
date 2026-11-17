
test_that("get_filing uses the cache version.", {
    cache_dir <- test_path("fixtures", "testcache")
    file_path <- get_filing("1111111", "0001111111-11-000011", "offline-test", cache_dir)
    lines <- readLines(file_path)
    result <- grepl("COMPANY CONFORMED NAME:\t\t\tTest", lines)
    expect_true(any(result))
})

test_that("get_filing downloads a clean copy.", {
    useragent <- sec_useragent()
    cache_dir <- withr::local_tempdir()
    file_path <- get_filing("1321655", "0001321655-23-000044", useragent, cache_dir)
    expect_true(file.exists(file_path))
    lines <- readLines(file_path)
    result <- sub("^.+([0-9]{8})$", "\\1", lines[grepl("CONFORMED PERIOD OF REPORT", lines)])
    expected <- "20230331"
    expect_equal(result, expected)
})
