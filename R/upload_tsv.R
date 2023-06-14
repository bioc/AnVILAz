.WDS_API_VERSION <- "v0.2"

#' Upload a TSV file to the Workspace Data Tab
#'
#' A function to move a flat Tab-Separated Values (TSV) file into the
#' Azure workspace using the Workspace Data Services (WDS) API. The dataset
#' will appear as a table under the 'Data' tab.
#'
#' @param tsv_file `character(1)` A path to a tab-separated values file
#'
#' @param type `character(1)` A nickname for the uploaded dataset important for
#'   retreival. By default, the file name will be used.
#'
#' @param api_version `character(1)` The version of the Workspace Data Service
#'   API. Set to the value of the internal `.WDS_API_VERSION` variable by
#'   default. See the current version with `AnVILAz:::.WDS_API_VERSION`.
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
    api_version = .WDS_API_VERSION
) {
    api_endpoint <- "/{{instanceid}}/tsv/{{v}}/{{type}}"
    instanceid <- get_wds_url()
    v <- api_version
    endpoint <- whisker.render(api_endpoint)
    base_uri <- get_wds_url()
    uri <- paste0(base_uri, endpoint)
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
retrieve_tsv <- function(type, api_version = .WDS_API_VERSION) {
    api_endpoint <- "/{{instanceid}}/tsv/{{v}}/{{type}}"
    instanceid <- get_wds_url()
    v <- api_version
    endpoint <- whisker.render(api_endpoint)
    uri <- paste0(base_uri, endpoint)
    response <- GET(
        uri,
        add_headers(authorization = az_token()),
        accept_json()
    )
    .stop_for_status(response)
    content(response, encoding = "UTF-8")
}

#' @importFrom whisker whisker.render
#' @importFrom httr DELETE
#' @export
delete_tsv_row <- function(type, id, api_verison = .RECORDS_API_VERSION) {
    instanceid <- workspace_id()
    v <- api_version
    api_endpoint <- "/{{instanceid}}/records/{{v}}/{{type}}/{{id}}"
    endpoint <- whisker.render(api_endpoint)

    tsv <- retrieve_tsv(type = type, api_version = api_version)
    allids <- tsv[[1L]]
    stopifnot(id %in% allids)
    base_uri <- get_wds_url()
    uri <- paste0(base_uri, path)
    response <- DELETE(
        uri,
        add_headers(authorization = az_token()),
        accept_json()
    )
    stop_for_status(response)
    content(response)
}
