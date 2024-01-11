#' @docType class
#' @title Azure platform class
#' @description This class represents the Azure platform.
#' @name azure-class
#'
#' @importClassesFrom AnVILBase Platform
#' @importFrom methods new
#' @importFrom AnVILBase cloud_platform
#' @exportClass azure
.azure <- setClass("azure", contains = "Platform")

#' @rdname azure-class
#'
#' @export
azure <- function() {
    if (identical(cloud_platform(), "AnVILAz"))
        .azure()
    else
        stop("Azure platform not detected; contact maintainer.")
}
