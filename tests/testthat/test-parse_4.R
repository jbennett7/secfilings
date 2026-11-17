test_that("parse_4 parses a form 4 filing file correctly", {
    filing <- test_path("fixtures/sgml/0001664272-23-000207.txt")
    result <- parse_4(filing)

    expect_type(result, "list")
    expect_named(result,
        c("filing", "owners", "nonderiv_transactions", "nonderiv_holdings",
          "deriv_transactions", "deriv_holdings", "footnotes"))

    # filing
    f <- result$filing
    expect_equal(nrow(f), 1)
    expect_equal(f$accession, "0001664272-23-000207")
    expect_equal(f$documentType, "4")
    expect_equal(f$filedDate, as.Date("2023-03-09"))
    expect_equal(f$periodOfReport, as.Date("2023-03-08"))
    expect_equal(f$issuerCik, "0000092230")
    expect_equal(f$issuerName, "TRUIST FINANCIAL CORP")
    expect_equal(f$issuerTradingSymbol, "TFC")
    expect_false(f$notSubjectToSection16)
    expect_true(is.na(f$aff10b5One))

    #  owners
    o <- result$owners
    expect_equal(nrow(o), 1)
    expect_equal(o$rptOwnerName, "Wilson Donta L")
    expect_false(o$isDirector)
    expect_true(o$isOfficer)
    expect_false(o$isTenPercentOwner)
    expect_equal(o$officerTitle, "Sr. Executive Vice President")

    # nonderiv_transactions - the one reportable event
    tx <- result$nonderiv_transactions
    expect_equal(nrow(tx), 1)
    expect_equal(tx$transactionDate, as.Date("2023-03-08"))
    expect_equal(tx$transactionCode, "G")
    expect_equal(tx$shares, 114)
    expect_equal(tx$acquiredDisposedCode, "D")
    expect_equal(tx$ownershipType, "D")
    expect_true(is.na(tx$natureOfOwnership))
    expect_equal(tx$sharesOwnedFollowing, 36666.099)

    # nonderiv_holdings - the restated 401(k) bucket
    h <- result$nonderiv_holdings
    expect_equal(nrow(h), 1)
    expect_equal(h$ownershipType, "I")
    expect_equal(h$natureOfOwnership, "By 401(k)")
    expect_equal(h$sharesOwnedFollowing, 5303.982)

    # no derivatives, no footnotes on this filing
    expect_equal(nrow(result$deriv_transactions), 0)
    expect_equal(nrow(result$deriv_holdings), 0)
    expect_equal(nrow(result$footnotes), 0)
})

test_that("parse_4 will return a warning and NULL for pre-2003 filings", {
    filing <- test_path("fixtures/sgml/0001092787-02-000003.txt")
    expect_warning(results <- parse_4(filing), regexp = "pre-2003")
    expect_equal(results, NULL)
})

test_that("parse_4 regression for real <TYPE>8-K in the same CIK cache dir", {
    filing <- test_path("fixtures/sgml/0000092230-22-000040.txt")
    results <- expect_warning(
        parse_4(filing), regexp="not a Form 4")
    expect_null(results)
})

test_that("to_lgl normalizes both boolean encodings", {
    expect_equal(to_lgl(c("1", "0", "true", "false", "TRUE", "FALSE")),
                 c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))
    expect_true(is.na(to_lgl("")))
})

test_that("parse_4 preserves distinct ownership buckets without merging them", {
    filing <- test_path("fixtures/sgml/0001182002-25-000007.txt")
    result <- parse_4(filing)
    h <- result$nonderiv_holdings

    expect_equal(nrow(h), 5)
    expect_setequal(h$natureOfOwnership,
        c(NA, "By 401(k)", "By grantor retained annuity trust", "By 2023 GRAT", "By Trust"))
    expect_equal(h$ownershipType[is.na(h$natureOfOwnership)], "D")
    expect_true(all(h$ownershipType[!is.na(h$natureOfOwnership)] == "I"))

    # zero-share bucket must survive, not get filtered as "empty"
    expect_equal(
        h$sharesOwnedFollowing[h$natureOfOwnership %in% "By grantor retained annuity trust"], 0)
})

test_that("parse_4 parses derivative transactions and holdings", {
    filing <- test_path("fixtures/sgml/0001198477-26-000002.txt")
    result <- parse_4(filing)

    f <- result$filing
    expect_equal(f$accession, "0001198477-26-000002")
    expect_equal(f$filedDate, as.Date("2026-02-26"))
    expect_equal(f$periodOfReport, as.Date("2026-02-24"))

    o <- result$owners
    expect_equal(o$rptOwnerName, "CLEMENT DALLAS S")

    # deriv_transactions - one RSU vesting event
    dt <- result$deriv_transactions
    expect_equal(nrow(dt), 1)
    expect_equal(dt$securityTitle, "Restricted Stock Unit")
    expect_equal(dt$transactionDate, as.Date("2026-02-24"))
    expect_equal(dt$transactionCode, "A")
    expect_equal(dt$shares, 4027)
    expect_equal(dt$pricePerShare, 0)
    expect_equal(dt$acquiredDisposedCode, "A")
    expect_equal(dt$underlyingSecurityTitle, "Common Stock")
    expect_equal(dt$underlyingSecurityShares, 4027)
    expect_equal(dt$sharesOwnedFollowing, 23542)
    expect_equal(dt$ownershipType, "D")

    # conversionOrExercisePrice/exerciseDate/expirationDate carry only a
    # footnoteId on this row, no <value> - must come back NA, not error
    expect_true(is.na(dt$conversionOrExercisePrice))
    expect_true(is.na(dt$exerciseDate))
    expect_true(is.na(dt$expirationDate))

    # deriv_holdings - two phantom stock unit buckets
    dh <- result$deriv_holdings
    expect_equal(nrow(dh), 2)
    expect_equal(dh$securityTitle, c("Phantom Stock Unit", "Phantom Stock Units"))
    expect_equal(dh$conversionOrExercisePrice, c(0, 0))
    expect_equal(dh$exerciseDate, as.Date(c("2000-01-01", "2000-01-01")))
    expect_equal(dh$underlyingSecurityShares, c(13726.328, 16473.691))
    expect_equal(dh$sharesOwnedFollowing, c(13726.328, 16473.691))
    expect_true(all(dh$ownershipType == "D"))
})

test_that("parse_4 preserves every same-day transaction row without mergin", {
    filing <- test_path("fixtures/sgml/0001664272-23-000123.txt")
    result <- parse_4(filing)
    tx <- result$nonderiv_transactions

    expect_equal(nrow(tx), 4)
    expect_true(all(tx$transactionDate == as.Date("2023-02-27")))
    expect_true(all(tx$ownershipType == "D"))

    # document order preserved - this is what a future "last row wins" rollup
    # would depend on, since these rows can't be ordered by transactionDate alone
    expect_equal(tx$shares, c(2306, 310, 775, 1928))
    expect_equal(tx$sharesOwnedFollowing, c(8907, 6601, 6291, 5516))
})

test_that("parse_4 rejects a pre-XBRL 8-K as not a Form 4, not as pre-2003", {
    filing <- test_path("fixtures", "sgml", "0000092230-04-000082.txt")
    expect_warning(result <- parse_4(filing), "not a Form 4")
    expect_null(result)
})

test_that("parse_4 extracts footnote ids and text", {
    filing <- test_path("fixtures", "sgml", "0001182002-26-000006.txt")
    result <- parse_4(filing)
    expect_equal(result$footnotes$footnoteId, c("F1", "F2", "F3", "F4", "F5"))
    expect_equal(unique(result$footnotes$accession), "0001182002-26-000006")
    f1 <- result$footnotes$text[result$footnotes$footnoteId == "F1"]
    expect_match(f1, '"GRAT"', fixed = TRUE)
    expect_no_match(f1, "&quot;", fixed = TRUE)
})

test_that("parse_4 footnoteIds reference existing footnotes, deduplicated", {
    filing <- test_path("fixtures", "sgml", "0001182002-26-000006.txt")
    result <- parse_4(filing)
    expect_equal(result$deriv_holdings$footnoteIds, c("F3,F2", "F4", "F5"))
    refs <- c(result$nonderiv_transactions$footnoteIds, result$nonderiv_holdings$footnoteIds,
              result$deriv_transactions$footnoteIds,    result$deriv_holdings$footnoteIds)
    refs <- unlist(strsplit(refs[!is.na(refs)], ","))
    expect_in(refs, result$footnotes$footnoteId)

    expect_equal(result$nonderiv_transactions$footnoteIds, "F1")
    expect_equal(result$nonderiv_holdings$footnoteIds, c("F1", "F2", NA))
})

test_that("parse_4 returns zero-row transaction tables that match the populated schema", {
    result  <- parse_4(test_path("fixtures", "sgml", "0001182002-25-000005.txt"))
    with_nd <- parse_4(test_path("fixtures", "sgml", "0001664272-23-000207.txt"))
    with_d  <- parse_4(test_path("fixtures", "sgml", "0001198477-26-000002.txt"))
    expect_identical(result$nonderiv_transactions, with_nd$nonderiv_transactions[0, ])
    expect_identical(result$deriv_transactions,    with_d$deriv_transactions[0, ])
    expect_equal(nrow(result$nonderiv_holdings), 5)
    expect_equal(nrow(result$deriv_holdings), 2)
})
