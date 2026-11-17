test_that("extract_instance writes every document in the filing to out_dir", {
    sgml <- test_path("fixtures/sgml/1321655_10-K_2023-02-21_0001321655-23-000011.txt")
    path <- extract_instance(sgml, out_dir = withr::local_tempdir())
  
    expect_true(file.exists(path))

    files <- c(
        "pltr-20221231_cal.xml", "pltr-20221231_def.xml", "pltr-20221231_htm.xml",
        "pltr-20221231_lab.xml", "pltr-20221231_pre.xml", "pltr-20221231.htm",
        "pltr-20221231.xsd")

    expect_contains(list.files(path), files)
})

test_that("extract_instance successfully extracts a zip file", {
    sgml <- test_path("fixtures/sgml/1321655_10-K_2023-02-21_0001321655-23-000011.txt")
    path <- extract_instance(sgml, out_dir = withr::local_tempdir())
    zip_file <- file.path(path, "0001321655-23-000011-xbrl.zip")
    expect_true(file.exists(zip_file))
    expect_contains(list.files(path), utils::unzip(zip_file, list = TRUE)$Name)
})

test_that("extract_instance stops a uuencoded document at its end line", {
    out_dir <- withr::local_tempdir()
    extract_instance(test_path("fixtures", "sgml", "0000092230-04-000082.txt"), out_dir)
    jpg <- file.path(out_dir, "bbtlogo.jpg")
    bytes <- readBin(jpg, "raw", file.size(jpg))
    expect_equal(length(bytes), 9098)
    expect_equal(head(bytes, 3), as.raw(c(0xff, 0xd8, 0xff)))
    expect_equal(tail(bytes, 2), as.raw(c(0xff, 0xd9)))
})

