test_that("filing_urls strips leading zeros from the CIK", {
  expect_equal(filing_urls("0001321655", "0001321655-23-000044"),
               "https://www.sec.gov/Archives/edgar/data/1321655/0001321655-23-000044.txt")
})

test_that("filing_urls leaves an unpadded CIK unchanged", {
    expect_equal(filing_urls("1321655", "0001321655-23-000044"),
               "https://www.sec.gov/Archives/edgar/data/1321655/0001321655-23-000044.txt")
})

test_that("filing_urls vectorized over accession", {
    expect_equal(filing_urls("0001321655", c("0001321655-23-000044", "0001321655-23-000011")),
               c("https://www.sec.gov/Archives/edgar/data/1321655/0001321655-23-000044.txt",
                 "https://www.sec.gov/Archives/edgar/data/1321655/0001321655-23-000011.txt"))
})
