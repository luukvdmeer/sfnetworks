# Define an auxiliary function which is used to test that the relevant spatstat
# packages are installed and that the spatstat version is 2.0.0 or greater
# Check https://github.com/rubak/spatstat.revdep/blob/main/README.md and
# https://github.com/luukvdmeer/sfnetworks/issues/137 for more details
#' @importFrom utils packageVersion
check_spatstat <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste("Package", pkg, "required; please install it (or the full spatstat package) first."))
  } else {
    spst_ver <- try(packageVersion("spatstat"), silent = TRUE)
    if (!inherits(spst_ver, "try-error") && spst_ver < 2.0-0) {
      stop(paste0(
        "You have an old version of spatstat installed which is incompatible with ",
        pkg,
        ". Please update spatstat (or uninstall it)."
      ))
    }
  }
}
