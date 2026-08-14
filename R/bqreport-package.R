#' @keywords internal
"_PACKAGE"

# The dplyr methods in this package are registered on dplyr's generics, which
# `R CMD check` does not count as using the namespace. Importing one generic
# makes the dependency explicit in NAMESPACE as well.
#' @importFrom dplyr dplyr_reconstruct
NULL
