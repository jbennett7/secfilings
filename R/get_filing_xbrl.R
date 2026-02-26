find_instance_doc <- function(index_df) {
  ret_df <- index_df[index_df$type == "EX-101.INS", ]
  if(nrow(ret_df) == 0) stop("No EX-101.INS exist")
  if(nrow(ret_df) > 1) stop("Multiple EX-101.INS exists")
  return(ret_df$filename)
}

accession_from_link <- function(edgar_link) {
  gsub("\\.txt$", "", unlist(strsplit(edgar_link, "/"))[4])
}

ixbrl_doc_from_sgml <- function(sgml_path, form_type) {
  lines <- readLines(sgml_path, warn = FALSE, encoding = "latin1")
  regex <- paste0("^<TYPE>", form_type)

  # Find all <DOCUMENT> block boundaries
  doc_starts <- which(lines == "<DOCUMENT>")
  doc_ends   <- which(lines == "</DOCUMENT>")

  for (i in seq_along(doc_starts)) {
    block <- lines[doc_starts[i]:doc_ends[i]]
    type_lines <- block[grepl("<TYPE>", block)]
    if (length(type_lines) == 0 || sub("^<TYPE>", "", type_lines[1]) != form_type) next
    filename <- sub("^<FILENAME>", "", block[grepl("^<FILENAME>", block)][1])
    return(filename)
  }
  stop("No ", form_type, " document found in SGML: ", sgml_path)
}

#' @export
get_filing_xbrl <- function(cik, year, form_type, useragent) {

  # Check if master index exists, if it doesn't download it.
  filepath <- paste0("edgar_MasterIndex/", year, "master.Rda")
  if(!file.exists(filepath)) {
    edgar::getMasterIndex(year, useragent)
  }

  # Load the master index
  load(filepath)

  # Filter by cik
  df <- year.master |>
    dplyr::filter(
      cik == as.character(.env$cik),
      form.type == form_type
    )
  if(nrow(df) == 0) stop("Nothing returned")
  if(nrow(df) > 1){
    df <- df[df$date.filed == max(as.Date(df$date.filed)), ]
  }

  # Build SGML URL
  sgml_url <- paste0("https://www.sec.gov/Archives/", df$edgar.link)

  # Download file
  sgml_file <- tempfile(fileext = ".txt")
  response <- httr::GET(sgml_url, httr::user_agent(useragent))
  httr::stop_for_status(response)
  writeBin(httr::content(response, as = "raw"), sgml_file)
  inst_file <- tryCatch(
      extract_instance(sgml_file),
      error = function(e) {
        htm_name         <- ixbrl_doc_from_sgml(sgml_file, form_type)
        xml_name         <- sub("\\.htm$", "_htm.xml", htm_name)
        accession        <- accession_from_link(as.character(df$edgar.link))
        accession_nodash <- gsub("-", "", accession)
        cik_clean        <- sub("^0+", "", as.character(cik))
        base_url         <- paste0("https://www.sec.gov/Archives/edgar/data/",
                              cik_clean, "/", accession_nodash, "/")
        tmp_dir <- tempdir()
        resp <- httr::GET(paste0(base_url, xml_name), httr::user_agent(useragent))
        httr::stop_for_status(resp)
        writeBin(httr::content(resp, as = "raw"), file.path(tmp_dir, xml_name))

        xsd_name <- sub("_htm\\.xml$", ".xsd", xml_name)
        xsd_resp <- httr::GET(paste0(base_url, xsd_name), httr::user_agent(useragent))
        httr::stop_for_status(xsd_resp)
        writeBin(httr::content(xsd_resp, as = "raw"), file.path(tmp_dir, xsd_name))

        lab_name <- sub("_htm\\.xml$", "_lab.xml", xml_name)
        lab_resp <- httr::GET(paste0(base_url, lab_name), httr::user_agent(useragent))
        httr::stop_for_status(lab_resp)
        writeBin(httr::content(lab_resp, as = "raw"), file.path(tmp_dir, lab_name))

        pre_name <- sub("_htm\\.xml$", "_pre.xml", xml_name)
        pre_resp <- httr::GET(paste0(base_url, pre_name), httr::user_agent(useragent))
        httr::stop_for_status(pre_resp)
        writeBin(httr::content(pre_resp, as = "raw"), file.path(tmp_dir, pre_name))

        cal_name <- sub("_htm\\.xml$", "_cal.xml", xml_name)
        cal_resp <- httr::GET(paste0(base_url, cal_name), httr::user_agent(useragent))
        httr::stop_for_status(cal_resp)
        writeBin(httr::content(cal_resp, as = "raw"), file.path(tmp_dir, cal_name))

        def_name <- sub("_htm\\.xml$", "_def.xml", xml_name)
        def_resp <- httr::GET(paste0(base_url, def_name), httr::user_agent(useragent))
        httr::stop_for_status(def_resp)
        writeBin(httr::content(def_resp, as = "raw"), file.path(tmp_dir, def_name))

        file.path(tmp_dir, xml_name)
      }
  )
  XBRL::xbrlDoAll(inst_file, cache.dir = "xbrl.Cache")
}
