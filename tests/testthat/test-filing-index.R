# --- filing_index_url ---

test_that("filing_index_url builds correct URL from CIK and accession", {
  url <- filing_index_url("0000320193", "0000320193-23-000106")

  expect_equal(url, paste0(
    "https://www.sec.gov/Archives/edgar/data/",
    "320193/",
    "000032019323000106/",
    "0000320193-23-000106-index.json"
  ))
})

test_that("filing_index_url strips leading zeros from CIK", {
  url <- filing_index_url("0000012345", "0000012345-99-000001")
  expect_match(url, "/edgar/data/12345/")
})

test_that("filing_index_url strips dashes from accession for directory", {
  url <- filing_index_url("0000320193", "0000320193-23-000106")
  expect_match(url, "/000032019323000106/")
})

test_that("filing_index_url keeps dashes in accession for filename", {
  url <- filing_index_url("0000320193", "0000320193-23-000106")
  expect_match(url, "0000320193-23-000106-index\\.json$")
})

# --- parse_filing_index ---

test_that("parse_filing_index returns a data frame", {
  json <- readLines(test_path("fixtures/filing-index/index.json"))
  result <- parse_filing_index(paste(json, collapse = "\n"))

  expect_s3_class(result, "data.frame")
})

test_that("parse_filing_index has correct columns", {
  json <- readLines(test_path("fixtures/filing-index/index.json"))
  result <- parse_filing_index(paste(json, collapse = "\n"))

  expect_named(result, c("sequence", "filename", "type", "description", "size"))
})

test_that("parse_filing_index returns one row per document", {
  json <- readLines(test_path("fixtures/filing-index/index.json"))
  result <- parse_filing_index(paste(json, collapse = "\n"))

  expect_equal(nrow(result), 4L)
})

test_that("parse_filing_index correctly identifies XBRL instance row", {
  json <- readLines(test_path("fixtures/filing-index/index.json"))
  result <- parse_filing_index(paste(json, collapse = "\n"))

  xbrl_row <- result[result$type == "EX-101.INS", ]
  expect_equal(nrow(xbrl_row), 1L)
  expect_equal(xbrl_row$filename, "aapl-20230930.xml")
})
