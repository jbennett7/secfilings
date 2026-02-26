# inst/verify/verify_amazon.R
# Amazon FY2023 10-K (CIK 1018724, filed 2024-02-15)
# Known values source from SEC EDGAR interactive viewer:
#  https://www.sec.gov/ix?doc=/Archives/edgar/data/0001018724/000101872424000008/amzn-20231231.htm

devtools::load_all(".")

UA <- "Joseph Bennett jbennett@jbennettconsulting.com"

xbrl <- get_filing_xbrl(1018724, 2024, "10-K", UA)

# Helper: pull a single fact value by concept + period end date
get_fact <- function(xbrl, concept, period_end) {
  facts <- xbrl$fact
  ctx   <- xbrl$context
  dims  <- unique(xbrl$dimension$contextId)

  ctx_match <- ctx[
    !ctx$contextId %in% dims &
    ((!is.na(ctx$endDate)  & ctx$endDate  == period_end) |
     (!is.na(ctx$instant)  & ctx$instant  == period_end)),
  ]

  f <- facts[facts$elementId == concept & facts$contextId %in% ctx_match$contextId, ]
  if (nrow(f) == 0) return(NA_real_)
  as.numeric(f$fact[1])
}

known <- list(
  revenue      = list(concept = "us-gaap_RevenueFromContractWithCustomerExcludingAssessedTax",
                      period  = "2023-12-31",
                      value   = 574785e6),
  net_income   = list(concept = "us-gaap_NetIncomeLoss",
                      period  = "2023-12-31",
                      value   = 30425e6),
  total_assets = list(concept = "us-gaap_Assets",
                      period  = "2023-12-31",
                      value   = 527854e6)
)

# Compare
results <- lapply(names(known), function(nm) {
  k      <- known[[nm]]
  actual <- get_fact(xbrl, k$concept, k$period)
  data.frame(
    metric   = nm,
    expected = k$value,
    actual   = actual,
    match    = isTRUE(all.equal(actual, k$value))
  )
})

print(do.call(rbind, results))

