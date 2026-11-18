test_that("fixture arrays have equal lengths within each page", {
    main <- jsonlite::read_json(test_path("fixtures", "http", "CIK0002222222.json"))
    page2 <- jsonlite::read_json(
        test_path("fixtures", "http", "CIK0002222222-submissions-001.json"))

    expect_length(unique(lengths(main$filings$recent)), 1)
    expect_length(unique(lengths(page2)), 1)
})

test_that("fixture pages have the same fields", {
    main <- jsonlite::read_json(test_path("fixtures", "http", "CIK0002222222.json"))
    page2 <- jsonlite::read_json(
        test_path("fixtures", "http", "CIK0002222222-submissions-001.json"))
    expect_setequal(names(page2), names(main$filings$recent))
})

test_that("every files entry has a fixture", {
    main <- jsonlite::read_json(test_path("fixtures", "http", "CIK0002222222.json"))
    page_files <- unlist(lapply(main$filings$files, function(x) x$name))

    expect_gt(length(page_files), 0)
    expect_true(all(file.exists(test_path("fixtures", "http", page_files))))
})

test_that("fixture filingCount matches the page row count", {
    main <- jsonlite::read_json(test_path("fixtures", "http", "CIK0002222222.json"))
    page2 <- jsonlite::read_json(
        test_path("fixtures", "http", "CIK0002222222-submissions-001.json"))

    expect_equal(main$filings$files[[1]]$filingCount,
                 length(page2$accessionNumber))
})

test_that("fixtures match SEC's live submissions format", {
    main <- jsonlite::read_json(test_path("fixtures", "http", "CIK0002222222.json"))
    page2 <- jsonlite::read_json(
        test_path("fixtures", "http", "CIK0002222222-submissions-001.json"))

    ua <- sec_useragent()
    real <- sec_get("https://data.sec.gov/submissions/CIK0000732717.json", ua)
    # The main page has the fields your fixture uses
    expect_contains(names(real$filings$recent), names(main$filings$recent))

    # Pagination exists, and its entries have the fixture's keys
    expect_gt(length(real$filings$files), 0)
    expect_contains(names(real$filings$files[[1]]), names(main$filings$files[[1]]))

    real_page <- sec_get(paste0("https://data.sec.gov/submissions/",
                         real$filings$files[[1]]$name), ua)
    # An extra page keeps its arrays at the top level
    expect_contains(names(real_page), names(page2))
    expect_equal(real$filings$files[[1]]$filingCount,
                 length(real_page$accessionNumber))
})
