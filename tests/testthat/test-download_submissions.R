
test_that("download_submissions downloads a data frame with correct column names.", {
    useragent <- sec_useragent()
    cache_dir <- withr::local_tempdir()
    csv_file <- paste0(cache_dir, "/submissions/", "0001321655", ".csv")
    df <- download_submissions("0001321655", useragent, cache_dir)
    col_names <- names(df)
    expected <- c("accessionNumber", "filingDate", "reportDate",
                 "acceptanceDateTime", "act", "form", "fileNumber",
                 "filmNumber", "items", "core_type", "size",
                 "isXBRL", "isInlineXBRL", "isXBRLNumeric",
                 "primaryDocument", "primaryDocDescription")
    expect_true(all(expected %in% col_names))
})

test_that("download_submissions appends rows from every extra page", {
    local_mock_sec()
    cache_dir <- withr::local_tempdir()
    df <- download_submissions("0002222222", "offline-test", cache_dir)
    expect_equal(nrow(df), 5)
    expect_equal(df$accessionNumber,
                 c("0002222222-26-000003", "0002222222-26-000002", "0002222222-26-000001",
                   "0002222222-25-000002", "0002222222-25-000001"))
    expect_true(file.exists(file.path(cache_dir, "submissions", "0002222222.csv")))
})
