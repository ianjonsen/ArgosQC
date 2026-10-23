## Deployment references (internal)
##
## References typed or pasted into metadata, dropIDs and trimIDs files can
## differ from the SMRU spelling: Unicode dashes (e.g. the minus sign U+2212 or
## an en dash) in place of "-", "-" in place of "_", stray spaces or a byte
## order mark. These helpers clean them and match them to the SMRU spelling.

## replace Unicode dashes with "-", drop a byte order mark, trim spaces
qc_clean_ref <- function(x) {
  x <- as.character(x)
  x <- gsub("[\u2010\u2011\u2012\u2013\u2014\u2015\u2212\ufe58\ufe63\uff0d]", "-", enc2utf8(x), perl = TRUE)
  x <- gsub("\ufeff", "", x, fixed = TRUE)
  trimws(x)
}

## matching key: "-" and "_" treated as the same character
qc_ref_key <- function(x) gsub("_", "-", qc_clean_ref(x), fixed = TRUE)

## SMRU references in the tag data tables
qc_tag_refs <- function(smru) {
  tabs <- intersect(c("diag", "gps", "ctd", "dive", "haulout", "summary"), names(smru))
  unique(unlist(lapply(tabs, function(t) {
    if (is.data.frame(smru[[t]]) && "ref" %in% names(smru[[t]])) as.character(smru[[t]]$ref)
  })))
}

## references x in the SMRU spelling of refs, matched with qc_ref_key();
##  references with no match are returned cleaned
qc_match_refs <- function(x, refs) {
  x <- qc_clean_ref(x)
  refs <- unique(as.character(refs))
  k <- match(qc_ref_key(x), qc_ref_key(refs))
  ifelse(!is.na(k) & !x %in% refs, refs[k], x)
}
