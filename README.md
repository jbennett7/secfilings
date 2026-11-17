# secfilings

Retrieve and Parse SEC EDGAR Filings.

secfilings looks up company identifiers, filing histories and filing documents
from the SEC EDGAR system, caching everything locally to limit requests. It
extracts the documents bundled in a filing (including XBRL instance files and
uuencoded attachments) and parses Form 4 insider transactions and Schedule 13G
beneficial ownership reports.

It does not parse XBRL itself: hand the extracted instance file to
[XBRL](https://github.com/jbennett7/XBRL), or build full financial statements
with [xbrlstatements](https://github.com/jbennett7/xbrlstatements).

Personal research tool - not intended for CRAN publication.

## Installation

```r
devtools::install_github("jbennett7/secfilings")
```

## Quick start

```r
library(secfilings)
library(dplyr)

ua <- "Your Name your@email.com"   # SEC requires a name and email

cik <- get_cik("T", ua)            # "0000732717"
get_company(cik, ua)               # "AT&T INC."

# Every filing the company has made, newest first
filings <- get_submissions(cik, ua) |>
    filter(form == "10-Q", isXBRL) |>
    arrange(desc(reportDate))

# Download the latest 10-Q and unpack its documents
sgml <- get_filing(cik, filings$accessionNumber[1], ua)
dir  <- extract_instance(sgml, out_dir = file.path(tempdir(), "t-10q"))

# The XBRL instance: inline (_htm.xml) or traditional (YYYYMMDD.xml)
inst <- list.files(dir, pattern = "(?:\\d{8}|_htm)\\.xml$", full.names = TRUE)
xbrl <- XBRL::xbrlDoAll(inst)
```

### Insider transactions

```r
form4s <- get_submissions(cik, ua) |> filter(form == "4")
paths  <- get_filing(cik, form4s$accessionNumber, ua)
parsed <- lapply(paths, parse_4)          # NULL (with a warning) for unparseable filings

transactions <- bind_rows(lapply(parsed, `[[`, "nonderiv_transactions"))
```

## Functions

| Function | Returns |
|---|---|
| `get_cik(ticker, useragent, cache)` | 10-digit CIK string, or `NA` if the ticker is unknown |
| `get_company(cik, useragent, cache)` | Company name, or `NA` if the CIK is unknown |
| `get_submissions(cik, useragent, cache)` | Tibble with one row per filing, all pages included |
| `get_filing(cik, accession, useragent, cache)` | Path(s) to the downloaded SGML `.txt`; vectorized over `accession` |
| `extract_instance(sgml_path, out_dir)` | `out_dir`, after writing every document in the filing to o it (uuencoded attachments are decoded, zips unzipped) |
| `parse_4(filename)` | List of 7 tibbles (`filing`, `owners`, `nonderiv_transactions`, `nonderiv_holdings`, `deriv_transactions`, `deriv_holdings`, `footnotes`), or `NULL` with a warning for non-Form-4 or pre-2003 filings |
| `parse_13g(filename)` | One-row tibble for a Schedule 13G or 13G/A |

`get_submissions` returns typed columns: `filingDate` and `reportDate` are
Dates, `acceptanceDateTime` is POSIXct, `isXBRL` / `isInlineXBRL` /
`isXBRLNumeric` are logical, and identifiers (`filmNumber`, `fileNumber`,
`items`) are character.

## Caching

Every download is cached under `cache` (default `./.cache`) and reused on later calls:

```
.cache/
|--tickers.csv                      # get_cik, get_company
|--submissions/<cik>.csv            # get_submissions
|--filings/<cik>/<accession>.txt    # get_filing
```

Delete a file to force a fresh download. The ticker list and submissions change
over time; filings never do.

## Notes

- **User agent.** SEC requires every request to identify you as
  `"Name email@domain.com"`; generic or missing user agents get HTTP 403.
- **Rate limits.** SEC allows about 10 requests per second. Requests are
  retried with backoff on failure, but bulk downloads (e.g. every Form 4 for a
  large company) should be paced.
- **Ticker format.** SEC uses dashes for share classes (`BRK-B`), not dots.
- **Form 4 amendments** (`4/A`) are currently rejected by `parse_4`.

## Development

```r
devtools::test()
devtools::check()
```

Tests that hit SEC are skipped unless `SEC_USERAGENT` is set (add it to
`~/.Renviron`). Everything else runs offline against fixtures in
`tests/testthat/fixtures/`, with HTTP responses mocked from `fixtures/http/`.

## Dependencies

- [`httr`](https://cran.r-project.org/package=httr) - HTTP requests with retry
- [`readr`](https://cran.r-project.org/package=readr) - CSV cache
- [`dplyr`](https://cran.r-project.org/package=dplyr),
  [`tibble`](https://cran.r-project.org/package=tibble) - data frames
- [`xml2`](https://cran.r-project.org/package=xml2) - Form 4 / 13G parsing
- [`stringr`](https://cran.r-project.org/package=stringr) - Form 4 / 13G parsing

