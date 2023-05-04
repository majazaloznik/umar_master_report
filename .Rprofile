source("renv/activate.R")
Sys.setenv(http_proxy="http://proxy.gov.si:80")
Sys.setenv(http_proxy_user="http://proxy.gov.si:80")
Sys.setenv(https_proxy="http://proxy.gov.si:80")
Sys.setenv(https_proxy_user="http://proxy.gov.si:80")
cat("User level rprofile here, how are you doing?\n")
cat("UMAR proxy is set, hooray!\n")
options(continue = " ")

#' prettify base plot
#' use with old_par <- nice_par() to keep old settings for reuse
nice_par = function(mar = c(3.3, 3, 2, 1), mgp = c(2, 0.4, 0), tck = -0.01,
                    cex.axis = 0.9, las = 1, mfrow = c(1, 1), ...) {
  par(mar = mar, mgp = mgp, tck = tck, cex.axis = cex.axis, las = las,
      mfrow = mfrow, ...)
}

options(
  usethis.full_name = "Maja Zaloznik",
  usethis.description = list(
    "Authors@R" = utils::person("Maja", "Zaloznik",
                                email = "maja.zaloznik@gmail.com",
                                role = c("aut", "cre")),
    Version = "0.0.0.9000"
  ),
  usethis.overwrite = TRUE
)


