# transactionCode/2014 filings use "1"/"0"; 2026 filings use "true"/"false".
to_lgl <- function(x) {
    dplyr::case_when(
        tolower(x) %in% c("true", "1") ~ TRUE,
        tolower(x) %in% c("false", "0") ~ FALSE,
        TRUE ~ NA
    )
}

#' Parse a Form 4 (insider transaction) filing
#'
#' Parses one Form 4 SGML `.txt` file, as returned by
#' `secfilings::get_filing()`, into a list of 7 tibbles joined by `accession`:
#' `filing`, `owners`, `nonderiv_transactions`, `nonderiv_holdings`,
#' `deriv_transactions`, `deriv_holdings`, and `footnotes`. The split reflects
#' that a single filing can report multiple owners, transactions, and holdings,
#' each with different applicable fields.
#'
#' Rejects three cases by warning and returning `NULL` rather than attempting
#' to parse: submissions whose SGML header type isn't `4` (including `4/A`
#' amendments), pre-2003 Form 4s that predate SEC's electronic-XML mandate,
#' and, as a backstop, a Form 4 whose `<XML>` block isn't an
#' `ownershipDocument`.
#' 
#' Returns raw, per-node extraction only - it does not deduplicate
#' transactions/holdings sharing an ownership bucket, aggregate beneficial
#' ownership across buckets, or resolve owners to specific transactions on a
#' joint filing. Empty tables (e.g. `deriv_transactions` for a filing with no
#' derivative activity) still return the correct zero-row schema.
#'
#' @param filename Path to a cached Form 4 `.txt` file.
#' @return a named list of 7 tibbles, or `NULL` if `filename` isn't a parseable
#'   Form 4.
#' @export
parse_4 <- function(filename) {
    raw <- readr::read_file(filename)

    accession <- tools::file_path_sans_ext(basename(filename))
    filed_date <- stringr::str_match(raw, "FILED AS OF DATE:\\s+(\\d{8})")[, 2]
    submission_type <- stringr::str_match(raw, "CONFORMED SUBMISSION TYPE:\\s+(\\S+)")[, 2]
    if (is.na(submission_type) || submission_type != "4") {
        warning(paste0(accession, ": not a Form 4 (submission type ",
                       submission_type, ") - skipping"))
        return(NULL)
    }

    xml_content <- raw |>
        stringr::str_match(stringr::regex("<XML>\\r?\\n(.*?)\\n</XML>", dotall = TRUE))
    xml_content <- xml_content[, 2]

    if (is.na(xml_content)) {
        warning(paste0(accession, ": pre-2003 HTML-format Form 4 (no <XML> block) - skipping"))
        return(NULL)
    }

    doc <- xml2::read_xml(xml_content)
    xml2::xml_ns_strip(doc)

    # Relative xpath text lookup; xml2 returns NA for a missing node.
    txt <- function(node, xpath) {
        xml2::xml_text(xml2::xml_find_first(node, xpath), trim = TRUE)
    }

    # Backstop: the header says Form 4, but confirm the XML block really is the
    # ownership document before parsing it (otherwise every field comes back NA).
    if (xml2::xml_name(doc) != "ownershipDocument") {
        warning(paste0(accession, ": <XML> block is not an ownershipDocument - skipping"))
        return(NULL)
    }

    # Distinct footnoteId refs anywhere within a line item, comma-joined in first seen order.
    footnote_ids <- function(node) {
        ids <- xml2::xml_attr(xml2::xml_find_all(node, ".//footnoteId"), "id")
        if (length(ids) == 0) NA_character_ else paste(unique(ids), collapse = ",")
    }

    filing <- tibble::tibble(
        accession                = accession,
        filedDate                = as.Date(filed_date, "%Y%m%d"),
        schemaVersion            = txt(doc, ".//schemaVersion"),
        documentType             = txt(doc, ".//documentType"),
        periodOfReport           = as.Date(txt(doc, ".//periodOfReport")),
        dateOfOriginalSubmission = as.Date(txt(doc, ".//dateOfOriginalSubmission")),
        issuerCik                = txt(doc, ".//issuer/issuerCik"),
        issuerName               = txt(doc, ".//issuer/issuerName"),
        issuerTradingSymbol      = txt(doc, ".//issuer/issuerTradingSymbol"),
        notSubjectToSection16    = to_lgl(txt(doc, ".//notSubjectToSection16")),
        aff10b5One               = to_lgl(txt(doc, ".//aff10b5One")),
        remarks                  = txt(doc, ".//remarks"),
        signatureName            = txt(doc, ".//ownerSignature/signatureName"),
        signatureDate            = as.Date(txt(doc, ".//ownerSignature/signatureDate"))
    )

    # bind_rows(lapply(nodes, fn)) collapses to a 0x0 tibble when nodes is empty -
    # bind_rows has no data to infer columns from. Fall back to an explicit
    # zero-row template so callers can always rely on these columns existing, even
    # for a filing with no derivative (or no non-derivative) activity.
    bind_or_empty <- function(nodes, fn, template) {
        if (length(nodes) == 0) return(template)
        dplyr::bind_rows(lapply(nodes, fn))
    }

    # NB: when a filing has multiple reportingOwners (joint filing), the XML
    # schema does not tie individual transactions/holdings back to a specific
    # owner - owners and line items only share the accession, not a row key.
    owners <- bind_or_empty(
        xml2::xml_find_all(doc, ".//reportingOwner"),
        function(n) tibble::tibble(
            accession         = accession,
            rptOwnerCik       = txt(n, ".//rptOwnerCik"),
            rptOwnerName      = txt(n, ".//rptOwnerName"),
            isDirector        = to_lgl(txt(n, ".//isDirector")),
            isOfficer         = to_lgl(txt(n, ".//isOfficer")),
            isTenPercentOwner = to_lgl(txt(n, ".//isTenPercentOwner")),
            isOther           = to_lgl(txt(n, ".//isOther")),
            officerTitle      = txt(n, ".//officerTitle")
        ),
        tibble::tibble(
            accession = character(), rptOwnerCik = character(),
            rptOwnerName = character(), isDirector = logical(),
            isOfficer = logical(), isTenPercentOwner = logical(),
            isOther = logical(), officerTitle = character()
        )
    )

    nonderiv_transactions <- bind_or_empty(
        xml2::xml_find_all(doc, ".//nonDerivativeTable/nonDerivativeTransaction"),
        function(n) tibble::tibble(
            accession            = accession,
            securityTitle        = txt(n, ".//securityTitle/value"),
            transactionDate      = as.Date(txt(n, ".//transactionDate/value")),
            deemedExecutionDate  = as.Date(txt(n, ".//deemedExecutionDate/value")),
            transactionCode      = txt(n, ".//transactionCoding/transactionCode"),
            equitySwapInvolved   = to_lgl(txt(n, ".//transactionCoding/equitySwapInvolved")),
            shares               = as.numeric(txt(n,
                            ".//transactionAmounts/transactionShares/value")),
            pricePerShare        = as.numeric(txt(n,
                            ".//transactionAmounts/transactionPricePerShare/value")),
            acquiredDisposedCode = txt(n,
                            ".//transactionAmounts/transactionAcquiredDisposedCode/value"),
            sharesOwnedFollowing = as.numeric(txt(n,
                            ".//postTransactionAmounts/sharesOwnedFollowingTransaction/value")),
            ownershipType        = txt(n, ".//ownershipNature/directOrIndirectOwnership/value"),
            natureOfOwnership    = txt(n, ".//ownershipNature/natureOfOwnership/value"),
            footnoteIds          = footnote_ids(n)
        ),
        tibble::tibble(
            accession = character(), securityTitle = character(),
            transactionDate = as.Date(character()),
            deemedExecutionDate = as.Date(character()),
            transactionCode = character(), equitySwapInvolved = logical(),
            shares = double(), pricePerShare = double(),
            acquiredDisposedCode = character(),
            sharesOwnedFollowing = double(),
            ownershipType = character(), natureOfOwnership = character(),
            footnoteIds = character()
        )
    )

    nonderiv_holdings <- bind_or_empty(
        xml2::xml_find_all(doc, ".//nonDerivativeTable/nonDerivativeHolding"),
        function(n) tibble::tibble(
            accession            = accession,
            securityTitle        = txt(n, ".//securityTitle/value"),
            sharesOwnedFollowing = as.numeric(txt(n,
                            ".//postTransactionAmounts/sharesOwnedFollowingTransaction/value")),
            ownershipType        = txt(n, ".//ownershipNature/directOrIndirectOwnership/value"),
            natureOfOwnership    = txt(n, ".//ownershipNature/natureOfOwnership/value"),
            footnoteIds          = footnote_ids(n)
        ),
        tibble::tibble(
            accession = character(), securityTitle = character(),
            sharesOwnedFollowing = double(), ownershipType = character(),
            natureOfOwnership = character(), footnoteIds = character()
        )
    )

    deriv_transactions <- bind_or_empty(
        xml2::xml_find_all(doc, ".//derivativeTable/derivativeTransaction"),
        function(n) tibble::tibble(
            accession                = accession,
            securityTitle            = txt(n, ".//securityTitle/value"),
            conversionOrExercisePrice = as.numeric(txt(n, ".//conversionOrExercisePrice/value")),
            transactionDate          = as.Date(txt(n, ".//transactionDate/value")),
            deemedExecutionDate      = as.Date(txt(n, ".//deemedExecutionDate/value")),
            transactionCode          = txt(n, ".//transactionCoding/transactionCode"),
            shares                   = as.numeric(txt(n,
                            ".//transactionAmounts/transactionShares/value")),
            pricePerShare            = as.numeric(txt(n,
                            ".//transactionAmounts/transactionPricePerShare/value")),
            acquiredDisposedCode     = txt(n,
                            ".//transactionAmounts/transactionAcquiredDisposedCode/value"),
            exerciseDate             = as.Date(txt(n, ".//exerciseDate/value")),
            expirationDate           = as.Date(txt(n, ".//expirationDate/value")),
            underlyingSecurityTitle  = txt(n,
                            ".//underlyingSecurity/underlyingSecurityTitle/value"),
            underlyingSecurityShares = as.numeric(txt(n,
                            ".//underlyingSecurity/underlyingSecurityShares/value")),
            sharesOwnedFollowing     = as.numeric(txt(n,
                            ".//postTransactionAmounts/sharesOwnedFollowingTransaction/value")),
            ownershipType            = txt(n,
                            ".//ownershipNature/directOrIndirectOwnership/value"),
            natureOfOwnership        = txt(n, ".//ownershipNature/natureOfOwnership/value"),
            footnoteIds              = footnote_ids(n)
        ),
        tibble::tibble(
            accession = character(), securityTitle = character(),
            conversionOrExercisePrice = double(),
            transactionDate = as.Date(character()),
            deemedExecutionDate = as.Date(character()),
            transactionCode = character(), shares = double(),
            pricePerShare = double(), acquiredDisposedCode = character(),
            exerciseDate = as.Date(character()),
            expirationDate = as.Date(character()),
            underlyingSecurityTitle = character(),
            underlyingSecurityShares = double(),
            sharesOwnedFollowing = double(), ownershipType = character(),
            natureOfOwnership = character(),
            footnoteIds = character()
        )
    )

    deriv_holdings <- bind_or_empty(
        xml2::xml_find_all(doc, ".//derivativeTable/derivativeHolding"),
        function(n) tibble::tibble(
            accession                = accession,
            securityTitle            = txt(n, ".//securityTitle/value"),
            conversionOrExercisePrice = as.numeric(txt(n, ".//conversionOrExercisePrice/value")),
            exerciseDate             = as.Date(txt(n, ".//exerciseDate/value")),
            expirationDate           = as.Date(txt(n, ".//expirationDate/value")),
            underlyingSecurityTitle  = txt(n, 
                                ".//underlyingSecurity/underlyingSecurityTitle/value"),
            underlyingSecurityShares = as.numeric(txt(n,
                                ".//underlyingSecurity/underlyingSecurityShares/value")),
            sharesOwnedFollowing     = as.numeric(txt(n,
                                ".//postTransactionAmounts/sharesOwnedFollowingTransaction/value")),
            ownershipType            = txt(n,
                                ".//ownershipNature/directOrIndirectOwnership/value"),
            natureOfOwnership        = txt(n, ".//ownershipNature/natureOfOwnership/value"),
            footnoteIds              = footnote_ids(n)
        ),
        tibble::tibble(
            accession = character(), securityTitle = character(),
            conversionOrExercisePrice = double(),
            exerciseDate = as.Date(character()),
            expirationDate = as.Date(character()),
            underlyingSecurityTitle = character(),
            underlyingSecurityShares = double(),
            sharesOwnedFollowing = double(), ownershipType = character(),
            natureOfOwnership = character(), footnoteIds = character()
        )
    )

    footnote_nodes <- xml2::xml_find_all(doc, ".//footnotes/footnote")
    footnotes <- tibble::tibble(
        accession  = accession,
        footnoteId = xml2::xml_attr(footnote_nodes, "id"),
        text       = xml2::xml_text(footnote_nodes, trim = TRUE)
    )

    list(
        filing                = filing,
        owners                = owners,
        nonderiv_transactions = nonderiv_transactions,
        nonderiv_holdings     = nonderiv_holdings,
        deriv_transactions    = deriv_transactions,
        deriv_holdings        = deriv_holdings,
        footnotes             = footnotes
    )
}
