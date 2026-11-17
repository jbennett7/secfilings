sec_useragent <- function() {
    skip_on_cran()
    skip_if_offline("www.sec.gov")
    ua <- Sys.getenv("SEC_USERAGENT")
    skip_if(ua == "", "SEC_USERAGENT not set")
    ua
}
