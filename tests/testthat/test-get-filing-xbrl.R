
test_that("find_instance_doc returns the filename when exactly one EX-101.INS row is present", {
  json <- readLines(test_path("fixtures/filing-index/index.json"))
  index_df <- parse_filing_index(paste(json, collapse = "\n"))
  result <- find_instance_doc(index_df)
  expect_equal(result, "aapl-20230930.xml")
})

test_that("find_instance_doc errors when no EX-101.INS row exists", {
  json <- readLines(test_path("fixtures/filing-index/index.json"))
  index_df <- parse_filing_index(paste(json, collapse = "\n"))
  index_df <- index_df[index_df$type != "EX-101.INS", ]
  expect_error(find_instance_doc(index_df), "No EX-101.INS exist")
})

test_that("find_instance_doc errors when multiple EX-101.INS rows exist (malformed index)", {
  json <- readLines(test_path("fixtures/filing-index/index.json"))
  index_df <- parse_filing_index(paste(json, collapse = "\n"))
  index_df <- rbind(index_df, index_df[index_df$type == "EX-101.INS", ])
  expect_error(find_instance_doc(index_df), "Multiple EX-101.INS exists")
})

test_that("accession_from_link returns correct dashed accession number", {
  result <- accession_from_link("edgar/data/320193/0000320193-23-000106.txt")
  expect_equal(result, "0000320193-23-000106")
})

test_that("accession_from_link handles ciks of varying length", {
  result <- accession_from_link("edgar/data/12345/0000012345-99-000001.txt")
  expect_equal(result, "0000012345-99-000001")
})

test_that("get_filing_xbrl integration test", {
  skip_if_offline()
  useragent <- "Joseph Bennett jbennett@jbennettconsulting.com"
  result <- get_filing_xbrl(320193, 2023, "10-K", useragent)
  expect_type(result, "list")
  expect_true(all(c("fact", "context", "element", "label", "unit") %in% names(result)))
})
