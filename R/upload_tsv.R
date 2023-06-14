.WDS_API_VERSION <- "v0.2"

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
#' @param version `character(1)` The version of the API. Set to the value of
#'   the internal `.WDS_API_VERSION` variable by default. See the current
#'   version with `AnVILAz:::.WDS_API_VERSION`.
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
    version = .WDS_API_VERSION
) {
    # "/{instanceid}/tsv/{v}/{type}"
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
retrieve_tsv <- function(type, version = .WDS_API_VERSION) {
    # "/{instanceid}/tsv/{v}/{type}"
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

#' @importFrom httr DELETE
#' @export
delete_tsv_row <- function(type, id, verison = .RECORDS_API_VERSION) {
    # "/{instanceid}/records/{v}/{type}/{id}"
    tsv <- retrieve_tsv(type = type, version = version)
    allids <- tsv[[1L]]
    stopifnot(id %in% allids)
    base_uri <- get_wds_url()
    path <- paste0("/", workspace_id(), "/records/", version, "/", type, id)
    uri <- paste0(base_uri, path)
    response <- DELETE(
        uri,
        add_headers(authorization = az_token()),
        accept_json()
    )
    stop_for_status(response)
    content(response)
}
