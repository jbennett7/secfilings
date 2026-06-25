#' Extract the XBRL instance document from an SEC SGML submission file
#'
#' @param sgml_path, Character. Path to the downloaded SGML submission .txt file
#' @param out_dir, Character. Directory to write the extracted .xml file into.
#'   Defaults to a temporary directory.
#' @return Path to the extracted XBRL instance .xml file.
#' @export
extract_instance <- function(sgml_path, out_dir = tempdir()) {
    # Create out_dir if it does not exist.
    if (!dir.exists(out_dir)) dir.create(out_dir)

    # Read the entire sgml file.
    lines <- readLines(sgml_path, warn = FALSE, encoding = "latin1")
  
    # Find all <DOCUMENT> block boundaries
    doc_starts <- which(lines == "<DOCUMENT>")
    doc_ends   <- which(lines == "</DOCUMENT>")
  
    # Process each line one at a time withing each document
    for (i in seq_along(doc_starts)) {
      #  The document block.
      block <- lines[doc_starts[i]:doc_ends[i]]
      # File name, used when writing to disk.
      filename <- sub("^<FILENAME>(.*)$", "\\1", block[grepl("^<FILENAME>", block)])

      # Where the actual contents of the file begins.
      text_start <- which(block == "<TEXT>")+1
      text_end   <- which(block == "</TEXT>")-1
      # If the next line after <TEXT> is <XBRL> or <XML> skip this line as well.
      if (grepl("^<(?:XBRL|XML)>|^begin", block[text_start])) {
          text_start <- text_start+1
          text_end   <- text_end-1
      }
      # The content is between these two.
      content <- block[(text_start):(text_end)]
      # Write to disk
      out_path <- file.path(out_dir, filename)
      writeLines(content, out_path)
    }
    return(out_dir)
}
