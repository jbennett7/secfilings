useragent <- "Your Name yourname@example.com"

test_that("download_submissions downloads a data frame with correct column names.", {
    skip("Working")
    cache_dir <- "./testcache"
    cik <- '0001321655'
    expected <- '0001823952-26-000016'
    csv_file <- paste0(cache_dir, "/submissions/", cik, ".csv")
    df <- download_submissions(cik, useragent, cache_dir)
    col_names <- names(df)
    expected <- c("accessionNumber", "filingDate", "reportDate",
                 "acceptanceDateTime", "act", "form", "fileNumber",
                 "filmNumber", "items", "core_type", "size",
                 "isXBRL", "isInlineXBRL", "isXBRLNumeric",
                 "primaryDocument", "primaryDocDescription")
    expect_true(all(expected %in% col_names))
    unlink(cache_dir, recursive=TRUE)
})
