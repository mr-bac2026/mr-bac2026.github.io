PPM_URL <- paste0("https://packagemanager.posit.co/cran/",
                  "__linux__/noble/latest")

options(
  repos = c(PPM = PPM_URL),
  HTTPUserAgent = sprintf(
    "R/%s R (%s)",
    getRversion(),
    paste(getRversion(), R.version$platform, R.version$arch, R.version$os)
  ),
  Ncpus = max(1L, parallel::detectCores() - 1L)
)

need <- c(
  "ggplot2", "dplyr", "tidyr", "readr", "forcats", "stringr", "scales",
  "patchwork", "ggridges", "viridis", "ggtext", "jsonlite", "gt",
  "systemfonts", "textshaping", "ragg"
)
have <- rownames(installed.packages())
miss <- setdiff(need, have)
cat("To install:", paste(miss, collapse = ", "), "\n")
if (length(miss)) install.packages(miss, quiet = TRUE)

have2 <- rownames(installed.packages())
for (p in need) {
  cat(sprintf("%-12s %s\n", p, if (p %in% have2) "OK" else "FAILED"))
}
cat("DONE\n")
