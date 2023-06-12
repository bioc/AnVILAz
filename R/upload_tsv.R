#' Upload a TSV file to WDS URL
#'
#' A function to move a flat Tab-Separated Values (TSV) file into the
#' Azure workspace using the Workspace Data Services (WDS) API
#'
#' @param tsv_file `character(1)` A path to a tab-separated values file
#'
#' @param type `character(1)` A nickname for the uploaded dataset important for
#'   retreival. By default, the file name will be used.
#'
#' @param version `character(1)` The version of the API. Set to `v0.2` by
#'   default.
#'
#' @return The contents of the API POST request after uploading the TSV file
#'
#' @examples
#' if (interactive()) {
#'   type <- "model"
#'   mtcars_tbl <-
#'       mtcars |>
#'       as_tibble(rownames = "model_id") |>
#'       mutate(model_id = gsub(" ", "-", model_id))
#'
#'   tsv_file <- tempfile()
#'   readr::write_tsv(mtcars_tbl, tsv_file)
#'   upload_tsv(
#'     tsv_file = tsvfile,
#'     type = "testData",
#'   )
#' }
#' @export
upload_tsv <- function(
    tsv_file,
    type = tools:::file_path_sans_ext(basename(tsv_file)),
    version = "v0.2"
) {
    base_uri <- get_wds_url()
    path <- paste0("/", workspace_id(), "/tsv/", version, "/", type)
    uri <- paste0(base_uri, path)
    response <- POST(
        uri,
        body = list(records =
            upload_file(tsv_file, "text/tab-separated-values")
        ),
        add_headers(authorization = az_token()),
        accept_json()
    )
    .stop_for_status(response)
    content(response)
}

#' @export
get_tsv <- function(type, version = "v0.2") {
    base_uri <- get_wds_url()
    path <- paste0("/", workspace_id(), "/tsv/", version, "/", type)
    uri <- paste0(base_uri, path)
    response <- GET(
        uri,
        add_headers(authorization = az_token()),
        accept_json()
    )
    .stop_for_status(response)
    content(response, encoding = "UTF-8")
}

