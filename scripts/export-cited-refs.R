#!/usr/bin/env Rscript

# Pre-render build step.
# Scans the manuscript sources for citation keys and writes a trimmed,
# project-local `references.bib` containing only the cited entries, drawn
# from the central master bib (the source of truth). Also copies the CSL
# locally so the Quarto manuscript PDF bundler can find both inside the
# project directory.

master_bib <- "/Users/matthewnowlin/Library/CloudStorage/OneDrive-UTArlington/01-RESEARCH/Manuscript-Files/refs.bib"
master_csl <- "/Users/matthewnowlin/Library/CloudStorage/OneDrive-UTArlington/01-RESEARCH/Manuscript-Files/csl/american-political-science-association.csl"

out_bib <- "references.bib"
out_csl <- "american-political-science-association.csl"

# --- 1. Collect citation keys from every manuscript source -----------------
src <- c(
  "cue-energy.qmd",
  list.files("manuscript", pattern = "\\.qmd$", full.names = TRUE),
  list.files("notebooks",  pattern = "\\.qmd$", full.names = TRUE)
)
src <- src[file.exists(src)]
text <- paste(unlist(lapply(src, readLines, warn = FALSE, encoding = "UTF-8")),
              collapse = "\n")

# Pandoc citation keys: @key, where key starts alphanumeric and may contain
# internal _ : . + - . Crossref keys (@fig-*, @tbl-*, ...) are harmlessly
# included here but dropped later because they are not in the bib.
cite_pat <- "@[A-Za-z0-9][A-Za-z0-9_:.+-]*"
keys <- regmatches(text, gregexpr(cite_pat, text, perl = TRUE))[[1]]
keys <- unique(sub("^@", "", keys))
keys <- sub("[.:+-]+$", "", keys)  # trim trailing punctuation not part of key

# --- 2. Split the master bib into entries ----------------------------------
bib <- readLines(master_bib, warn = FALSE, encoding = "UTF-8")
starts <- grep("^[[:space:]]*@", bib)
ends   <- c(starts[-1] - 1L, length(bib))
entry_keys <- trimws(sub("^[[:space:]]*@[^{(]+[{(]([^,]+),?.*$", "\\1", bib[starts]))

# --- 3. Select cited entries and write the local bib -----------------------
sel <- which(entry_keys %in% keys)
out_lines <- unlist(lapply(sel, function(i) c(bib[starts[i]:ends[i]], "")))
writeLines(out_lines, out_bib, useBytes = TRUE)

# --- 4. Copy the CSL locally and suppress URLs for journal articles ----------
invisible(file.copy(master_csl, out_csl, overwrite = TRUE))
csl_txt <- readLines(out_csl, warn = FALSE, encoding = "UTF-8")
csl_txt <- gsub(
  'type="legal_case" match="none"',
  'type="legal_case article-journal" match="none"',
  csl_txt, fixed = TRUE
)
writeLines(csl_txt, out_csl, useBytes = TRUE)

# --- 5. Report -------------------------------------------------------------
missing <- setdiff(keys[grepl("[0-9]", keys)], entry_keys)  # likely-real keys
message(sprintf("export-cited-refs: %d/%d cited keys matched -> %s",
                length(sel), length(entry_keys[entry_keys %in% keys]), out_bib))
if (length(missing)) {
  message("export-cited-refs: cited keys NOT found in master bib:\n  ",
          paste(sort(missing), collapse = "\n  "))
}
