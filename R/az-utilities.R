#' @name az-utilities
#'
#' @title az health check helpers
#'
#' @description These functions provide checks for essential workspace tools and
#'   variables. `az_exists` checks for the presence of the `az` command line
#'   utility. `az_healthcheck` checks for the presence of the `az` command line
#'   as well as the essential environment variables.
#'
#' @return az_exists returns a logical value indicating the presence of the `az`
#'   command line utility. `az_healthcheck` returns a logical value indicating
#'   the presence of the `az` command line utility and the essential environment
#'   variables.
#'
#' @examples
#' if (interactive()) {
#'     az_exists()
#'     az_healthcheck()
#' }
#' @export
az_exists <- function() {
    result <- tryCatch({
        .az_find("az")
    }, error = function(...) "")
    nchar(result) > 0L
}

#' @rdname az-utilities
#'
#' @importFrom BiocBaseUtils isScalarCharacter
#' @export
az_health_check <- function() {
    if (!az_exists())
        warning("The 'az' command line utility is not available", call. = FALSE)
    keys <- .avcache$keys()
    nzchars <-  vapply(
        keys, function(x) isScalarCharacter(.avcache$get(x)), logical(1L)
    )
    notfounds <- paste(keys[!nzchars], collapse = ", ")
    if (!nzchar(notfounds))
        warning(
            "The environment variable(s) ", notfounds, " are not set.",
            call. = FALSE
        )
    all(nzchars)
}
