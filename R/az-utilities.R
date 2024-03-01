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
#' @export
az_healthcheck <- function() {
    if (!az_exists())
        warning("The 'az' command line utility is not available", call. = FALSE)
    keys <- .avcache$keys()
    nchars <-  vapply(
        keys, function(x) nchar(.avcache$get(x)), numeric(1L)
    )
    notfounds <- keys[!nchars]
    if (length(notfounds))
        warning(
            "The environment variable(s) ",
            paste(notfounds, collapse = ", "),
            " are not set.",
            call. = FALSE
        )
    all(nchar(keys))
}
