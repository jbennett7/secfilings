#' Extract the XBRL instance document from an SEC SGML submission file
#'
#' @param sgml_path Path to the downloaded SGML submission .txt file
#' @param out_dir Directory to write the extracted .xml file into.
#'   Defaults to a temporary directory.
#' @return Path to the extracted XBRL instance .xml file
#' @export
extract_instance <- function(sgml_path, out_dir = tempdir()) {
  lines <- readLines(sgml_path, warn = FALSE, encoding = "latin1")

  # Find all <DOCUMENT> block boundaries
  doc_starts <- which(lines == "<DOCUMENT>")
  doc_ends   <- which(lines == "</DOCUMENT>")

  for (i in seq_along(doc_starts)) {
    block <- lines[doc_starts[i]:doc_ends[i]]

    type <- block[grepl("^<TYPE>", block)][1]
    if (!identical(type, "<TYPE>EX-101.INS")) next

    filename <- sub("^<FILENAME>", "", block[grepl("^<FILENAME>", block)][1])

    text_start <- which(block == "<TEXT>")[1]
    text_end   <- which(block == "</TEXT>")[1]
    content    <- block[(text_start + 1):(text_end - 1)]

    out_path <- file.path(out_dir, filename)
    writeLines(content, out_path)
    return(out_path)
  }

  stop("No XBRL instance (EX-101.INS) found in: ", sgml_path)
}
