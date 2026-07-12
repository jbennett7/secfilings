useragent <- "Your Name yourname@example.com"

test_that("download_tickers downloads a clean copy.", {
    skip("Working")
    cache_dir <- "./testcache"
    csv_file <- paste0(cache_dir, "/tickers.csv")
    download_tickers(useragent, cache_dir)
    df <- readr::read_csv(csv_file, na = "")
    test_ticker <- 'PLTR'
    expected <- '1321655'
    expect_true(file.exists(csv_file))
    expect_true(all(names(df) == c("cik_str", "ticker", "title")))
    expect_equal(as.character(df[df$ticker == test_ticker,]$cik_str), expected)
    unlink(cache_dir, recursive=TRUE)
})
