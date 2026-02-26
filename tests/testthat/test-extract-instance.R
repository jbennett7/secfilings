test_that("extract_instance returns a path to an .xml file", {
  sgml <- test_path("fixtures/sgml/submission.txt")
  path <- extract_instance(sgml)

  expect_true(file.exists(path))
  expect_match(basename(path), "\\.xml$")
})

test_that("extract_instance uses the original filename from the SGML", {
  sgml <- test_path("fixtures/sgml/submission.txt")
  path <- extract_instance(sgml)

  expect_equal(basename(path), "aapl-20230930.xml")
})

test_that("extract_instance strips the TEXT wrapper", {
  sgml <- test_path("fixtures/sgml/submission.txt")
  path <- extract_instance(sgml)
  lines <- readLines(path)

  expect_false(any(grepl("^<TEXT>$", lines)))
  expect_false(any(grepl("^</TEXT>$", lines)))
})

test_that("extract_instance preserves the XBRL content", {
  sgml <- test_path("fixtures/sgml/submission.txt")
  path <- extract_instance(sgml)
  lines <- readLines(path)

  expect_true(any(grepl("<xbrl", lines)))
  expect_true(any(grepl("383285000000", lines)))
})

test_that("extract_instance errors when no XBRL instance is present", {
  sgml <- test_path("fixtures/sgml/no-xbrl.txt")

  expect_error(extract_instance(sgml), regexp = "No XBRL instance")
})
