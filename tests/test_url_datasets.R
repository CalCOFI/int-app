#!/usr/bin/env Rscript
# tests/test_url_datasets.R — the ?datasets= rule (app/functions.R's
# parse_datasets_param).
#
#     Rscript tests/test_url_datasets.R
#
# The app needs a current release DuckDB to start, so the rule that decides what
# a link selects is a pure function and is checked here rather than by clicking.
# Two things it must get right, both of which a link outliving a release will
# find: an unknown key is dropped so the app opens instead of erroring, and an
# all-unknown list is NULL rather than character(0), because in this app an empty
# dataset selection already means ALL of them — a stale link must not read as a
# deliberate empty filter.

# source the one function under test, not the whole app (functions.R expects a
# live DB connection at load)
src   <- readLines(file.path("app", "functions.R"))
start <- grep("^parse_datasets_param <- function", src)
stopifnot(length(start) == 1)
eval(parse(text = paste(src[start:length(src)], collapse = "\n")))

KNOWN <- c("calcofi_bottle", "swfsc_ichthyo", "cce-lter_zoodb", "farallon_bird-mammal")
fails <- 0L
ok <- function(label, got, want) {
  good <- identical(got, want)
  if (!good) {
    fails <<- fails + 1L
    cat("FAIL ", label, "\n  got:  ", paste(deparse(got), collapse = ""),
        "\n  want: ", paste(deparse(want), collapse = ""), "\n", sep = "")
  } else cat("ok   ", label, "\n", sep = "")
}

ok("one key",              parse_datasets_param("swfsc_ichthyo", KNOWN), "swfsc_ichthyo")
ok("several, URL order",   parse_datasets_param("swfsc_ichthyo,calcofi_bottle", KNOWN),
                           c("swfsc_ichthyo", "calcofi_bottle"))
ok("whitespace is trimmed", parse_datasets_param(" swfsc_ichthyo , calcofi_bottle ", KNOWN),
                           c("swfsc_ichthyo", "calcofi_bottle"))
ok("a hyphenated key survives", parse_datasets_param("cce-lter_zoodb", KNOWN), "cce-lter_zoodb")
ok("duplicates collapse",  parse_datasets_param("swfsc_ichthyo,swfsc_ichthyo", KNOWN), "swfsc_ichthyo")

# a link that outlives a release
ok("an unknown key is dropped, the known ones kept",
   parse_datasets_param("swfsc_ichthyo,retired_dataset", KNOWN), "swfsc_ichthyo")
ok("ALL unknown is NULL, never character(0)",
   parse_datasets_param("retired_dataset,gone_too", KNOWN), NULL)

# nothing asked for
ok("no parameter",         parse_datasets_param(NULL, KNOWN), NULL)
ok("empty parameter",      parse_datasets_param("", KNOWN), NULL)
ok("commas only",          parse_datasets_param(",,", KNOWN), NULL)

# the exact link CalCOFI.github.io builds from products.yml
ok("the dataset page's own link",
   parse_datasets_param("calcofi_bottle", KNOWN), "calcofi_bottle")

cat("\n", if (fails) sprintf("%d FAILURE(S)\n", fails) else "all pass\n", sep = "")
quit(status = if (fails) 1L else 0L)
