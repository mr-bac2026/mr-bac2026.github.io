# Fast binary installs from Posit Public Package Manager.
options(
  repos = c(PPM = "https://packagemanager.posit.co/cran/__linux__/noble/latest"),
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
  "sysfonts", "showtext"
)
have <- rownames(installed.packages())
miss <- setdiff(need, have)
cat("To install:", paste(miss, collapse = ", "), "\n")
if (length(miss)) install.packages(miss, quiet = TRUE)

have2 <- rownames(installed.packages())
for (p in need) cat(sprintf("%-10s %s\n", p, if (p %in% have2) "OK" else "FAILED"))
cat("DONE\n")
