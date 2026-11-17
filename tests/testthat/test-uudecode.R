test_that("uudecode decodes a known line", {
    expect_equal(rawToChar(uudecode("#0V%T")), "Cat")
})

test_that("uudecode zero padding: backtick, space, or stripped all give the same output", {
    expect_equal(rawToChar(uudecode("!00``")), "A")
    expect_equal(rawToChar(uudecode("!00  ")), "A")
    expect_equal(rawToChar(uudecode("!00")),   "A")
})

test_that("uudecode concatenates lines while skipping blank and zero-length lines", {
    expect_equal(rawToChar(uudecode(c("#0V%T", "", "`", "#0V%T"))), "CatCat")
})
