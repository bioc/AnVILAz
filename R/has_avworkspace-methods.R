#' @name has_avworkspace-methods
#'
#' @title Helper to check if the current environment is within an Azure
#'   workspace
#'
#' @description `has_avworkspace()` checks that the AnVIL environment is set up
#'   to work with Azure. If `strict = TRUE`, it also checks that the workspace
#'   name is set.
#'
#' @inheritParams AnVILBase::has_avworkspace
#'
#' @return `logical(1)` `TRUE` if the AnVIL environment is set up properly to
#'   interact with Azure, otherwise `FALSE`.
#'
#' @examples
#' has_avworkspace(platform = azure())
#'
NULL

#' @describeIn has_avworkspace-methods Check if the AnVIL environment is set up
#'
#' @importFrom AnVILBase has_avworkspace
#'
#' @exportMethod has_avworkspace
setMethod("has_avworkspace", signature = c(platform = "azure"), definition =
    function(strict = FALSE, ..., platform = cloud_platform()) {
        az_exists() &&
            identical(AnVILBase::avplatform_namespace(), "AnVILAz") &&
            (!strict || nzchar(avworkspace_name()))
    }
)
