#Function to gather version info

# Get session info

session <- sessionInfo()

# Extract R version
r_version <- session$R.version$version.string

# Extract base packages (these don't have version numbers in sessionInfo)
base_packages <- session$basePkgs

# Extract other packages with versions
other_packages <- session$otherPkgs
if (!is.null(other_packages)) {
  other_pkg_info <- data.frame(
    Package = names(other_packages),
    Version = sapply(other_packages, function(x) x$Version),
    stringsAsFactors = FALSE
  )
} else {
  other_pkg_info <- data.frame(Package = character(0), Version = character(0))
}

This_session <- list(r_version, base_packages, other_packages)
