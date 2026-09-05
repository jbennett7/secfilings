#' Parse both Schedule 13G and 13G/A filings. This is meant to be used on a
#' range of files to build a data frame.
#' 
#' @param filename SGML filename to parse
#' @importFrom tibble tibble
#' @importFrom xml2 xml_text xml_find_all xml_ns_strip read_xml xml_find_first
#' @importFrom stringr str_match regex
#' @importFrom readr read_file
#' @return tibble row
#' @export
parse_13g <- function(filename) {
    raw <- readr::read_file(filename)

    xml_content <- raw |>
        stringr::str_match(stringr::regex("<XML>\\r?\\n(.*?)\\n</XML>", dotall = TRUE))
    xml_content <- xml_content[, 2]

    doc <- xml2::read_xml(xml_content)
    xml2::xml_ns_strip(doc)

    accession <- tools::file_path_sans_ext(basename(filename))
    filed_date <- stringr::str_match(raw, "FILED AS OF DATE:\\s+(\\d{8})")[, 2]

    types <- xml2::xml_find_all(doc,
                            ".//coverPageHeaderReportingPersonDetails/typeOfReportingPerson") |>
        xml2::xml_text() |>
        paste(collapse = ",")

    cusips <- xml2::xml_find_all(doc, ".//issuerCusip | .//issuerCusips/issuerCusipNumber") |>
        xml2::xml_text() |>
        paste(collapse = ",")

    tibble::tibble(
        accession         = accession,
        filedDate         = as.Date(filed_date, "%Y%m%d"),
        submissionType    = xml2::xml_text(xml2::xml_find_first(doc, ".//submissionType")),
        amendmentNo       = xml2::xml_text(xml2::xml_find_first(doc, ".//amendmentNo")),
        previousAccession = xml2::xml_text(xml2::xml_find_first(doc, ".//previousAccessionNumber")),
        issuerCik         = xml2::xml_text(xml2::xml_find_first(doc, ".//issuerCik")),
        issuerName        = xml2::xml_text(xml2::xml_find_first(doc, ".//issuerName")),
        issuerCusip       = cusips,
        securitiesClass   = xml2::xml_text(xml2::xml_find_first(doc, ".//securitiesClassTitle")),
        eventDate         = as.Date(xml2::xml_text(xml2::xml_find_first(doc,
                                ".//eventDateRequiresFilingThisStatement")), "%m/%d/%Y"),
        soleVoting        = as.numeric(xml2::xml_text(xml2::xml_find_first(doc,
                                ".//soleVotingPower"))),
        sharedVoting      = as.numeric(xml2::xml_text(xml2::xml_find_first(doc,
                                ".//sharedVotingPower"))),
        soleDispositive   = as.numeric(xml2::xml_text(xml2::xml_find_first(doc,
                                ".//soleDispositivePower"))),
        sharedDispositive = as.numeric(xml2::xml_text(xml2::xml_find_first(doc,
                                ".//sharedDispositivePower"))),
        aggregateShares   = as.numeric(xml2::xml_text(xml2::xml_find_first(doc,
                                ".//reportingPersonBeneficiallyOwnedAggregateNumberOfShares"))),
        classPercent      = as.numeric(xml2::xml_text(xml2::xml_find_first(doc,
                                ".//coverPageHeaderReportingPersonDetails/classPercent"))),
        reportingPersonTypes = types
    )
}
