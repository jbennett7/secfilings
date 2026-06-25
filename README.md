# secfilings

An R package that will manage downloaded SGML EDGAR filings.

Personal research tool — not intended for CRAN publication.

## Overview

SEC EDGAR filings contain structured financial data in XBRL format, but
extracting it requires navigating SGML containers, two different XBRL
submission formats (traditional and inline), and SEC's filing index APIs. This
package handles all of that so you can go from a company CIK to a parsed set of
financial facts in one function call.

```r
library(secfilings)
# from https://github.com/jbennett7/XBRL
library(XBRL)

UA <- "Your Name your@email.com"

# Retrieve Apple's Filing 
sgml <- get_filings("320193", "0000320193-26-000013", UA)
dir.create("./sgml_extracted")
extract_instance(sgml, out_dir="./sgml_extracted")
inst <- list.files("./sgml_extracted", pattern="_htm.xml", full.names=TRUE)
dir.create("./db/320193")

xbrl <- xbrlDoAll(inst, prefix.out="./db/320193/0000320193-26-000013_")

# Access the parsed data frames
xbrl$fact      # financial facts (values)
xbrl$context   # time periods and entity identifiers
xbrl$label     # human-readable concept names
xbrl$element   # element metadata (periodType, balance, etc.)
```

## Installation

```r
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

### `get_filings(cik, accessionNumber, useragent)

Downloads the sgml filing.

### `extract_instance(sgml_path, out_dir)`

Parses an SGML submission `.txt` file, extracts the embedded XBRL instance
document, writes it to `out_dir`, and returns the path.

```r
xml_path <- extract_instance("/path/to/filing.txt")
```

### `get_cik(ticker)'

Returns the cik for the company represented by the ticker symbol.

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

## Notes

- `useragent` must be in the form `"Name email@domain.com"` as required by the SEC. The `edgar` package silently rejects certain email addresses.
- `edgar` rate-limits to 3 seconds between SEC requests per fair-access policy. Bulk downloads are intentionally slow.
- Kohl's and some other filers (DFIN ActiveDisclosure format) embed all linkbases inside the `.xsd` — separate `_lab.xml`, `_pre.xml`, `_cal.xml`, `_def.xml` files do not exist. The package handles this automatically.
- Kohl's fiscal year ends in early February (FY2023 ended 2024-02-03); use `year = 2024`.
- Amazon FY2023 10-K was filed in February 2024; use `year = 2024`.

## Dependencies

- [`XBRL`](https://github.com/jbennett7/XBRL) - XBRL/iXBRL instance document parser. This package is not explicitly dependent on this package. However, in order to parse the XBRL document this is needed.
- [`httr`](https://cran.r-project.org/package=httr) - HTTP requests.
- [`jsonlite`](https://cran.r-project.org/package=jsonlite) - JSON parsing.
- [`readr`](https://cran.r-project.org/package=readr) - provide fast and friendly way to read and write rectangular data (csv, tsv, etc...).
- [`dplyr`](https://cran.r-project.org/package=dplyr) - Data frame tools.
