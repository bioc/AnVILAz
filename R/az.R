#' @rdname az
#'
#' @title az command line utility
#'
#' @description
#' These functions invoke the `az` command line utility.
#'
#' @export
az_exists <- function() {
    result <- tryCatch({
        .az_find("az")
    }, error = function(...) "")
    nchar(result) > 0L
}
