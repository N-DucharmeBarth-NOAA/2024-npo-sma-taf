source("renv/activate.R")
options(vsc.rstudioapi = TRUE)
# jsonlite needed for VScode and needs to be
# referenced in code to be captured by renv
  library("jsonlite")

if (interactive() && Sys.getenv("TERM_PROGRAM") == "vscode") {

  if ("httpgd" %in% .packages(all.available = TRUE)) {
    options(vsc.plot = FALSE)
    options(device = function(...) {
      httpgd::hgd(silent = TRUE)
      .vsc.browser(httpgd::hgd_url(), viewer = "Beside")
    })
  }
}

### padded line
