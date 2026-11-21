helper_offline_setup <- function() {
    local_mock_sec()
    cache_dir <- withr::local_tempdir()
    get_companyfacts("2222222", "offline-test", cache_dir)
}

test_that("get_companyfacts returns the correct shape types", {
    cf <- helper_offline_setup()
    expect_named(cf, c("taxonomy", "tag", "unit", "start", "end", "val", "accn",
                       "fy", "fp", "form", "filed", "frame"))
    expect_equal(nrow(cf), 7)
    expect_s3_class(cf$start, "Date")
    expect_s3_class(cf$end, "Date")
    expect_s3_class(cf$filed, "Date")
    expect_type(cf$fy, "integer")
    expect_type(cf$val, "double")
})

test_that("get_companyfacts the cache file is written", {
    local_mock_sec()
    cache_dir <- withr::local_tempdir()
    cf <- get_companyfacts("2222222", "offline-test", cache_dir)
    expect_true(file.exists(file.path(cache_dir, "companyfacts", "CIK0002222222.json")))
})

test_that("get_companyfacts padded and unpadded CIKs agree", {
    local_mock_sec()
    cache_dir <- withr::local_tempdir()
    cf_unpadded <- get_companyfacts("2222222", "offline-test", cache_dir)
    cf_padded <- get_companyfacts("0002222222", "offline-test", cache_dir)
    expect_identical(cf_unpadded, cf_padded)
})

test_that("get_companyfacts a second call uses the cache", {
        local_mock_sec()
        cache_dir <- withr::local_tempdir()
        first <- get_companyfacts("2222222", "offline-test", cache_dir)

        local_mocked_bindings(sec_get = function(url, useragent, as = "parsed")
                                stop("should not download"))
        expect_no_message(second <- get_companyfacts("2222222", "offline-test", cache_dir))

        expect_identical(second, first)
})

test_that("get_companyfacts refresh = TRUE downloads again", {
    local_mock_sec()
    cache_dir <- withr::local_tempdir()
    cf <- get_companyfacts("2222222", "offline-test", cache_dir)
    expect_message(get_companyfacts("2222222", "offline-test", cache_dir, refresh = TRUE),
                   "Downloading companyfacts")
})

test_that("get_companyfacts instant facts have start values as NA, duration facts do not", {
    cf <- helper_offline_setup()
    expect_true(all(is.na(cf$start[cf$taxonomy == "dei" | cf$tag == "Assets"])))
    expect_false(any(is.na(cf$start[cf$tag == "Revenues"])))
})

test_that("get_companyfacts taxonomy, unit and value survive", {
    cf <- helper_offline_setup()
    expect_identical(cf$tag[cf$taxonomy == "dei"], "EntityCommonStockSharesOutstanding")
    expect_identical(cf$unit[cf$taxonomy == "dei"], "shares")
    expect_identical(cf$unit[cf$tag == "EarningsPerShareDiluted"], "USD/shares")
    expect_equal(cf$val[cf$tag == "EarningsPerShareDiluted"], 1.25)
})

test_that("get_companyfacts a missing frame becomes NA", {
    cf <- helper_offline_setup()
    expect_equal(cf$frame[cf$tag == "Revenues"], c("CY2024", NA, "CY2025", NA))
})

test_that("get_companyfacts the duplicate \"Revenues\" is kept and fy belongs to the filing", {
    cf <- helper_offline_setup()
    expect_equal(cf$fy[cf$tag == "Revenues" & cf$end == as.Date("2024-12-31")], c(2024L, 2025L))
})

test_that("get_companyfacts fields missing from the whole file get the right columns and types", {
    # The fixture can't reach these cases: it always has start somewhere, and fy
    # is never null throughout.
    path <- withr::local_tempfile(fileext = ".json")
    writeLines('{"cik": 1, "entityName": "X", "facts": {"us-gaap": {"Assets": {
               "label": "Assets", "description": "d", "units": {"USD": [
               {"end": "2025-12-31", "val": 1, "accn": "a", "fy": null, "fp": "FY",
                "form": "10-K", "filed": "2026-02-20"}]}}}}}', path)
    rf <- read_companyfacts(path)
    expect_true(is.na(rf$start))
    expect_s3_class(rf$start, "Date")
    expect_true(is.na(rf$frame))
    expect_type(rf$fy, "integer")
})

test_that("get_companyfacts an NA CIK errors", {
    expect_error(get_companyfacts(NA, "offline-test"), "cik is NA")
})

test_that("get_companyfacts live format matches what the parser expects", {
    cf <- get_companyfacts("732717", sec_useragent(), withr::local_tempdir())
    expect_contains(cf$taxonomy, c("dei", "us-gaap"))
    expect_true("Assets" %in% cf$tag)
    expect_gt(nrow(cf), 1000)
    expect_false(any(is.na(cf$end)))
    expect_false(any(is.na(cf$val)))
    expect_false(any(is.na(cf$accn)))
    expect_false(any(is.na(cf$form)))
    expect_false(any(is.na(cf$filed)))
})
