#' Decode a uuencoded document body into raw bytes
#'
#' @param content Character vector of uuencoded lines (the "begin"/"end"
#'   wrapper lines already stripped).
#' @return A raw vector of the decoded bytes.
#' @noRd
uudecode <- function(content) {
    chunks <- vector("list", length(content))
    for (i in seq_along(content)) {
        line <- content[i]
        if (nchar(line) == 0) next
        # Leading character encodes the number of decoded bytes on this line.
        n <- bitwAnd(utf8ToInt(substr(line, 1, 1)) - 32L, 0x3FL)
        if (n == 0) next
        codes <- bitwAnd(utf8ToInt(substring(line, 2)) - 32L, 0x3FL)
        # Uuencoded data is grouped in 4-character (6-bit) chunks decoding
        # to 3 bytes each. Lines are sometimes short of the length implied
        # by n (e.g. trailing spaces stripped by an editor) - space decodes
        # to a zero code anyway, so pad/truncate out to the declared length.
        expected <- ((n + 2L) %/% 3L) * 4L
        if (length(codes) < expected) {
            codes <- c(codes, rep(0L, expected - length(codes)))
        } else if (length(codes) > expected) {
            codes <- codes[seq_len(expected)]
        }
        m <- matrix(codes, nrow = 4)
        b1 <- bitwOr(bitwShiftL(m[1, ], 2), bitwShiftR(m[2, ], 4))
        b2 <- bitwOr(bitwShiftL(bitwAnd(m[2, ], 0xFL), 4), bitwShiftR(m[3, ], 2))
        b3 <- bitwOr(bitwShiftL(bitwAnd(m[3, ], 0x3L), 6), m[4, ])
        bytes <- as.vector(rbind(b1, b2, b3))[seq_len(n)]
        chunks[[i]] <- as.raw(bytes)
    }
    do.call(c, chunks)
}

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

      # Binary documents (ZIP, GRAPHIC, ...) are uuencoded: "begin <mode>
      # <name>" wraps the body and "end" trails it. Everything else may
      # have an <XBRL>/<XML> wrapper line to skip instead.
      is_uuencoded <- grepl("^begin [0-7]+ ", block[text_start])
      if (is_uuencoded || grepl("^<(?:XBRL|XML)>", block[text_start])) {
          text_start <- text_start+1
          text_end   <- text_end-1
      }
      # The content is between these two.
      content <- block[(text_start):(text_end)]
      # Write to disk
      out_path <- file.path(out_dir, filename)

      if (is_uuencoded) {
          writeBin(uudecode(content), out_path)
          if (grepl("\\.zip$", filename, ignore.case = TRUE)) {
              utils::unzip(out_path, exdir = out_dir)
          }
      } else {
          writeLines(content, out_path)
      }
    }
    return(out_dir)
}
