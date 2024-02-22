.WDS_API_VERSION <- "v0.2"

#' @name workspace-data-ops
#'
#' @title Functions to work with workspace data
#'
#' @aliases upload_tsv download_tsv delete_tsv_row delete_tsv
#'
#' @description These group of functions will allow you to manipulate data in
#'   the "DATA" tab. Example operations include moving a flat Tab-Separated
#'   Values (TSV) file into the workspace, deleting records, deleting tables,
#'   and retrieving the data table.
#'
#' @details These functions use the Workspace Data Services (WDS) API. Current
#'   operations that affect the "DATA" tab include:
#'   * `upload_tsv` - a `POST` request using a TSV file that populates the data
#'   * `download_tsv` - a `GET` request with the data name (`type` argument) in
#'     `upload_tsv` to represent the data locally as a `tibble`
#'   * `delete_tsv_row` - a `DELETE` request to remove a record or row from
#'     `type`
#'   * `add_tsv_row` - a `PUT` request to add a single row to an existing table
#'     (`type`)
#'   * `get_tsv_row` - a `GET` request to retrieve a single row from an existing
#'     table (`type`)
#'   * `delete_tsv` - a `DELETE` request to remove then entire data set (`type`)
#'
#' @param tsv_file `character(1)` A path to a tab-separated values file
#'
#' @param row `tibble()` or `data.frame()` A single row to add to an existing
#'   table. The row must have the same column names as the table. The
#'   `primaryKey` column must be unique.
#'
#' @param type `character(1)` A nickname for the uploaded dataset important for
#'   retreival. By default, the file name will be used.
#'
#' @param id `character(1)` The value in the `primaryKey` column that
#'   indicates the row to be removed.
#'
#' @param primaryKey `character(1)` The optional column name to uniquely
#'   identify a record. By default, the first column is used as the primary
#'   key and all values in the column must be unique.
#'
#' @param api_version `character(1)` The version of the Workspace Data Service
#'   API. Set to the value of the internal `.WDS_API_VERSION` variable by
#'   default. See the current version with `AnVILAz:::.WDS_API_VERSION`.
#'
#' @return
#'   * `upload_tsv` - A response list indicating successful upload
#'   * `download_tsv` - A `tibble` corresponding to the data labeled with `type`
#'   * `delete_tsv_row`; `delete_tsv` - When successful, a `NULL` value
#'
#' @importFrom httr upload_file
#'
#' @examples
#' if (interactive()) {
#'   library(dplyr)
#'   type <- "model"
#'   mtcars_tbl <-
#'       mtcars |>
#'       as_tibble(rownames = "model_id") |>
#'       mutate(model_id = gsub(" ", "-", model_id))
#'
#'   tsv_file <- tempfile()
#'   readr::write_tsv(mtcars_tbl, tsv_file)
#'   upload_tsv(
#'     tsv_file = tsv_file,
#'     type = "testData",
#'     primaryKey = "model_id"
#'   )
#'
#'   download_tsv("testData")
#'
#'   datsun <- filter(mtcars_tbl, model_id == "Datsun-710")
#'   datsun[["model_id"]] <- "Datsun-512"
#'
#'   add_tsv_row(row = datsun, type = "testData")
#'
#'   get_tsv_row("testData", "Datsun-512")
#'
#'   delete_tsv_row("testData", "Datsun-512")
#'
#'   delete_tsv("testData")
#' }
#' @export
upload_tsv <- function(
    tsv_file,
    type = tools:::file_path_sans_ext(basename(tsv_file)),
    primaryKey = NULL,
    api_version = .WDS_API_VERSION
) {
    api_endpoint <- "/{{instanceid}}/tsv/{{v}}/{{type}}"
    instanceid <- workspace_id()
    v <- api_version
    endpoint <- whisker.render(api_endpoint)
    base_uri <- workspace_data_service_url()
    uri <- paste0(base_uri, endpoint)
    response <- POST(
        uri,
        query = list(primaryKey = primaryKey),
        body = list(
            records = upload_file(tsv_file, "text/tab-separated-values")
        ),
        add_headers(authorization = az_token()),
        accept_json()
    )
    avstop_for_status(response, "upload_tsv")
    content(response)
}

#' @rdname workspace-data-ops
#' @export
download_tsv <- function(type, api_version = .WDS_API_VERSION) {
    opt <- options(readr.show_col_types = FALSE)
    on.exit(options(opt))

    api_endpoint <- "/{{instanceid}}/tsv/{{v}}/{{type}}"
    instanceid <- workspace_id()
    v <- api_version
    base_uri <- workspace_data_service_url()
    endpoint <- whisker.render(api_endpoint)
    uri <- paste0(base_uri, endpoint)
    response <- GET(
        uri,
        add_headers(authorization = az_token()),
        accept_json()
    )
    avstop_for_status(response, "download_tsv")
    content(response, encoding = "UTF-8")
}

#' @rdname workspace-data-ops
#' @importFrom whisker whisker.render
#' @importFrom httr DELETE
#' @export
delete_tsv_row <- function(type, id, api_version = .WDS_API_VERSION) {
    opt <- options(readr.show_col_types = FALSE)
    on.exit(options(opt))

    instanceid <- workspace_id()
    v <- api_version
    api_endpoint <- "/{{instanceid}}/records/{{v}}/{{type}}/{{id}}"
    endpoint <- whisker.render(api_endpoint)

    tsv <- download_tsv(type = type, api_version = api_version)
    allids <- tsv[[1L]]
    stopifnot(id %in% allids)
    base_uri <- workspace_data_service_url()
    uri <- paste0(base_uri, endpoint)
    response <- DELETE(
        uri,
        add_headers(authorization = az_token()),
        accept_json()
    )
    avstop_for_status(response, "delete_tsv_row")
    is.null(
        content(response)
    )
}

#' @rdname workspace-data-ops
#' @importFrom httr PUT
#' @export
add_tsv_row <- function(
    row, type, id = row[[1L]], api_version = .WDS_API_VERSION
) {
    stopifnot(identical(nrow(row), 1L))

    opt <- options(readr.show_col_types = FALSE)
    on.exit(options(opt))

    instanceid <- workspace_id()
    v <- api_version
    api_endpoint <- "/{{instanceid}}/records/{{v}}/{{type}}/{{id}}"
    endpoint <- whisker.render(api_endpoint)

    tsv <- download_tsv(type = type, api_version = api_version)
    primaryKey <- names(tsv)[[1L]]
    allids <- tsv[[primaryKey]]

    if (!primaryKey %in% names(row) && missing(id))
        stop("Primary key not found in row or missing id argument.")
    else if (id %in% allids)
        warning("Replacing record with id: ", id)

    row <- row[, !names(row) %in% primaryKey]
    base_uri <- workspace_data_service_url()
    uri <- paste0(base_uri, endpoint)
    response <- PUT(
        uri,
        body = list(attributes = as.list(row)),
        query = c(primaryKey = primaryKey),
        encode = "json",
        add_headers(authorization = az_token()),
        accept_json()
    )
    avstop_for_status(response, "add_tsv_row")
    result <- content(response)
    result <- tibble::as_tibble(
        c(key = id, result[["attributes"]])
    )
    names(result)[[1L]] <- primaryKey
    result
}

#' @rdname workspace-data-ops
#' @importFrom httr GET
#' @export
get_tsv_row <- function(type, id, api_version = .WDS_API_VERSION) {
    opt <- options(readr.show_col_types = FALSE)
    on.exit(options(opt))

    instanceid <- workspace_id()
    v <- api_version
    api_endpoint <- "/{{instanceid}}/records/{{v}}/{{type}}/{{id}}"
    endpoint <- whisker.render(api_endpoint)

    base_uri <- workspace_data_service_url()
    uri <- paste0(base_uri, endpoint)
    response <- GET(
        uri,
        add_headers(authorization = az_token()),
        accept_json()
    )

    avstop_for_status(response, "get_tsv_row")
    content(response)[["attributes"]] |>
        tibble::as_tibble()
}

#' @rdname workspace-data-ops
#' @export
delete_tsv <- function(type, api_version = .WDS_API_VERSION) {
    opt <- options(readr.show_col_types = FALSE)
    on.exit(options(opt))

    instanceid <- workspace_id()
    v <- api_version
    api_endpoint <- "/{{instanceid}}/types/{{v}}/{{type}}"
    endpoint <- whisker.render(api_endpoint)
    base_uri <- workspace_data_service_url()
    uri <- paste0(base_uri, endpoint)
    response <- DELETE(
        uri,
        add_headers(authorization = az_token()),
        accept_json()
    )
    avstop_for_status(response, "delete_tsv")
    content(response)
}

