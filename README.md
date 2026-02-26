# secfilings

An R package that combines [`edgar`](https://cran.r-project.org/package=edgar) (SEC EDGAR download) and [`XBRL`](https://github.com/jbennett7/XBRL) (structured financial data parsing) into a unified tool for retrieving and analyzing corporate SEC filings.

Personal research tool — not intended for CRAN publication.

## Overview

SEC EDGAR filings contain structured financial data in XBRL format, but extracting it requires navigating SGML containers, two different XBRL submission formats (traditional and inline), and SEC's filing index APIs. This package handles all of that so you can go from a company CIK to a parsed set of financial facts in one function call.

```r
library(secfilings)

UA <- "Your Name your@email.com"

# Retrieve Apple's FY2023 10-K as structured XBRL data
xbrl <- get_filing_xbrl(320193, 2023, "10-K", UA)

# Access the parsed data frames
xbrl$fact      # financial facts (values)
xbrl$context   # time periods and entity identifiers
xbrl$label     # human-readable concept names
xbrl$element   # element metadata (periodType, balance, etc.)
```

## Installation

```r
# edgar is available on CRAN
install.packages("edgar")

# XBRL is installed from GitHub
devtools::install_github("jbennett7/XBRL")

# Install this package from GitHub
devtools::install_github("jbennett7/secfilings")
```

For development (with live reload):

```r
devtools::load_all(".")
```

## Usage

### `get_filing_xbrl(cik, year, form_type, useragent)`

The main entry point. Given a company CIK, fiscal year, and form type, downloads the filing and returns a parsed XBRL result list.

```r
xbrl <- get_filing_xbrl(
  cik       = 320193,   # Apple Inc.
  year      = 2023,
  form_type = "10-K",
  useragent = "Your Name your@email.com"
)
```

Handles both filing formats automatically:

- **Traditional XBRL** — extracts the `EX-101.INS` instance document from the SGML container
- **Inline iXBRL** — fetches the SEC-generated `_htm.xml` instance along with the schema and linkbases

The master index is cached locally in `edgar_MasterIndex/` and XBRL schemas are cached in `xbrl.Cache/`.

**Returns** a list of up to 11 data frames: `$fact`, `$context`, `$dimension`, `$element`, `$label`, `$presentation`, `$definition`, `$calculation`, `$unit`, `$footnote`, `$role`.

### `extract_instance(sgml_path, out_dir)`

Low-level helper. Parses an SGML submission `.txt` file, extracts the embedded XBRL instance document (`EX-101.INS`), writes it to `out_dir`, and returns the path.

```r
xml_path <- extract_instance("/path/to/filing.txt")
```

### `get_filing_index(cik, accession, useragent)`

Fetches the filing document index from EDGAR's JSON API. Returns a data frame of all documents in the submission with their filename, type, description, and size. Note: this endpoint returns 404 for some modern filings (e.g. Apple FY2023); the main pipeline uses an alternative approach.

```r
index <- get_filing_index(
  cik       = "320193",
  accession = "0000320193-23-000106",
  useragent = "Your Name your@email.com"
)
```

## XBRL Data Model

| DataFrame      | Key columns                                                           |
|----------------|-----------------------------------------------------------------------|
| `$fact`        | elementId, contextId, unitId, fact (value), decimals, factId, ns     |
| `$context`     | contextId, scheme, identifier, startDate, endDate, instant           |
| `$dimension`   | contextId, dimension, value                                           |
| `$element`     | elementId, periodType, balance, abstract, nillable                   |
| `$label`       | elementId, lang, labelRole, labelString                              |
| `$unit`        | unitId, measure, unitNumerator, unitDenominator                      |

Key joins:

- `fact$contextId → context$contextId` — adds entity + period to each fact
- `fact$elementId → label$elementId` — adds human-readable concept names

Contexts come in two flavors: **instant** (balance sheet date, use `context$instant`) and **duration** (income statement period, use `context$startDate`/`endDate`).

## Working with Facts

```r
# Pull a single fact by concept and period end date
get_fact <- function(xbrl, concept, period_end) {
  facts <- xbrl$fact
  ctx   <- xbrl$context
  dims  <- unique(xbrl$dimension$contextId)

  ctx_match <- ctx[
    !ctx$contextId %in% dims &
    ((!is.na(ctx$endDate) & ctx$endDate == period_end) |
     (!is.na(ctx$instant) & ctx$instant == period_end)),
  ]

  f <- facts[facts$elementId == concept & facts$contextId %in% ctx_match$contextId, ]
  if (nrow(f) == 0) return(NA_real_)
  as.numeric(f$fact[1])
}

# Apple FY2023 revenue
get_fact(xbrl,
  "us-gaap_RevenueFromContractWithCustomerExcludingAssessedTax",
  "2023-09-30"
)
# [1] 383285000000
```

## Verification Scripts

`inst/verify/` contains cross-check scripts for four companies with known financial figures verified against the SEC EDGAR interactive viewer:

| Script                  | Company    | CIK     | Fiscal Year End  |
|-------------------------|------------|---------|------------------|
| `verify_apple.R`        | Apple      | 320193  | 2023-09-30       |
| `verify_amazon.R`       | Amazon     | 1018724 | 2023-12-31       |
| `verify_jpmorgan.R`     | JP Morgan  | 19617   | 2023-12-31       |

These are not run by `R CMD check` — run them manually to validate the pipeline against live EDGAR data.

## Notes

- `useragent` must be in the form `"Name email@domain.com"` as required by the SEC. The `edgar` package silently rejects certain email addresses.
- `edgar` rate-limits to 3 seconds between SEC requests per fair-access policy. Bulk downloads are intentionally slow.
- Kohl's and some other filers (DFIN ActiveDisclosure format) embed all linkbases inside the `.xsd` — separate `_lab.xml`, `_pre.xml`, `_cal.xml`, `_def.xml` files do not exist. The package handles this automatically.
- Kohl's fiscal year ends in early February (FY2023 ended 2024-02-03); use `year = 2024`.
- Amazon FY2023 10-K was filed in February 2024; use `year = 2024`.

## Dependencies

- [`edgar`](https://cran.r-project.org/package=edgar) — SEC EDGAR master index and filing download
- [`XBRL`](https://github.com/jbennett7/XBRL) — XBRL/iXBRL instance document parser
- [`httr`](https://cran.r-project.org/package=httr) — HTTP requests
- [`jsonlite`](https://cran.r-project.org/package=jsonlite) — JSON parsing
