test_that("extract_instance returns a path to an .xml file", {
    skip("Working")
    sgml <- test_path("fixtures/sgml/1321655_10-K_2023-02-21_0001321655-23-000011.txt")
    path <- extract_instance(sgml)
  
    expect_true(file.exists(path))

    files <- c(
        "pltr-20221231_cal.xml", "pltr-20221231_def.xml", "pltr-20221231_htm.xml",
        "pltr-20221231_lab.xml", "pltr-20221231_pre.xml", "pltr-20221231.htm",
        "pltr-20221231.xsd")

    expect_true(all(files %in% list.files(path)))
})

test_that("extract_instance successfully extracts a zip file", {
    sgml <- test_path("fixtures/sgml/1321655_10-K_2023-02-21_0001321655-23-000011.txt")
    path <- extract_instance(sgml)
    print(list.files(path, pattern=".*.zip"))
})
