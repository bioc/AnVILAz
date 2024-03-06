#' @name workspace-dev-ops
#'
#' @title Functions to work with workspace data for developers
#'
#' @description These group of functions will allow you to manipulate tables in
#'   the "DATA" tab. Example operations include moving a flat Tab-Separated
#'   Values (TSV) file into the workspace, deleting records, deleting tables,
#'   adding a single row, retrieving a single row, and retrieving the data
#'   table. Note that the API used refers to tables as `types`.
#'
#' @details These functions use the Workspace Data Services (WDS) API. Current
#'   operations that affect the "DATA" tab include:
#'   * `upload_tsv` - a `POST` request using a TSV file that populates the data
#'   * `download_tsv` - a `GET` request with the data name (`type` argument) in
#'     `upload_tsv` to represent the data locally as a `tibble`
#'   * `delete_type_id` - a `DELETE` request to remove a record or row from
#'     `type`
#'   * `add_type_id` - a `PUT` request to add a single row to an existing table
#'     (`type`)
#'   * `get_type_id` - a `GET` request to retrieve a single row from an existing
#'     table (`type`)
#'   * `delete_type` - a `DELETE` request to remove then entire data set (`type`)
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
#' @return
#'   * `upload_tsv` - A response list indicating successful upload
#'   * `download_tsv` - A `tibble` corresponding to the data labeled with `type`
#'   * `delete_type_id`; `delete_type` - When successful, a `NULL` value
#'
#' @keywords internal
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
#'   ## create an example single row tibble for add_type_id
#'   datsun <- filter(mtcars_tbl, model_id == "Datsun-710")
#'   ## change the model_id to be unique
#'   datsun[["model_id"]] <- "Datsun-512"
#'
#'   add_type_id(row = datsun, type = "testData")
#'
#'   get_type_id("testData", "Datsun-512")
#'
#'   delete_type_id("testData", "Datsun-512")
#'
#'   delete_type("testData")
#' }
upload_tsv <- function(
    tsv_file,
    type = tools:::file_path_sans_ext(basename(tsv_file)),
    primaryKey = NULL
) {
    api_endpoint <- "/{{instanceid}}/tsv/{{v}}/{{type}}"
    instanceid <- .avcache$get("workspaceId")
    v <- .avcache$get("wdsApiVersion")
    endpoint <- whisker.render(api_endpoint)
    base_uri <- workspace_data_service_url()
    uri <- paste0(base_uri, endpoint)
    request(uri) |>
        req_auth_bearer_token(az_token()) |>
        req_url_query(primaryKey = primaryKey) |>
        req_body_multipart(
            records = curl::form_file(
                tsv_file, type = "text/tab-separated-values"
            )
        ) |>
        req_perform() |>
        resp_body_json()
}

#' @rdname workspace-dev-ops
download_tsv <- function(type) {
    opt <- options(readr.show_col_types = FALSE)
    on.exit(options(opt))

    api_endpoint <- "/{{instanceid}}/tsv/{{v}}/{{type}}"
    instanceid <- .avcache$get("workspaceId")
    v <- .avcache$get("wdsApiVersion")
    base_uri <- workspace_data_service_url()
    endpoint <- whisker.render(api_endpoint)
    uri <- paste0(base_uri, endpoint)

    if (!requireNamespace("readr", quietly = TRUE))
        stop("Install the 'readr' package to import TSV files")

    request(uri) |>
        req_auth_bearer_token(az_token()) |>
        req_perform() |>
        resp_body_string() |>
        readr::read_tsv(show_col_types = FALSE)
}

#' @rdname workspace-dev-ops
#' @importFrom whisker whisker.render
delete_type_id <- function(type, id) {
    opt <- options(readr.show_col_types = FALSE)
    on.exit(options(opt))

    instanceid <- .avcache$get("workspaceId")
    v <- .avcache$get("wdsApiVersion")
    api_endpoint <- "/{{instanceid}}/records/{{v}}/{{type}}/{{id}}"
    endpoint <- whisker.render(api_endpoint)

    base_uri <- workspace_data_service_url()
    uri <- paste0(base_uri, endpoint)
    response <- request(uri) |>
        req_auth_bearer_token(az_token()) |>
        req_method("DELETE") |>
        req_perform() |>
        resp_is_error()

    !response
}

#' @rdname workspace-dev-ops
add_type_id <- function(row, type, id = row[[1L]]) {
    stopifnot(identical(nrow(row), 1L))

    opt <- options(readr.show_col_types = FALSE)
    on.exit(options(opt))

    instanceid <- .avcache$get("workspaceId")
    v <- .avcache$get("wdsApiVersion")
    api_endpoint <- "/{{instanceid}}/records/{{v}}/{{type}}/{{id}}"
    endpoint <- whisker.render(api_endpoint)

    tsv <- download_tsv(type = type)
    primaryKey <- names(tsv)[[1L]]
    allids <- tsv[[primaryKey]]

    if (!primaryKey %in% names(row) && missing(id))
        stop("Primary key not found in row or missing id argument.")
    else if (id %in% allids)
        warning("Replacing record with id: ", id)

    row <- row[, !names(row) %in% primaryKey]
    base_uri <- workspace_data_service_url()
    uri <- paste0(base_uri, endpoint)
    result <- request(uri) |>
        req_auth_bearer_token(az_token()) |>
        req_body_json(
            list(attributes = as.list(row))
        ) |>
        req_url_query(primaryKey = primaryKey) |>
        req_method("PUT") |>
        req_perform() |>
        resp_body_json()

    result <- tibble::as_tibble(
        c(key = id, result[["attributes"]])
    )
    names(result)[[1L]] <- primaryKey
    result
}

#' @rdname workspace-dev-ops
get_type_id <- function(type, id) {
    opt <- options(readr.show_col_types = FALSE)
    on.exit(options(opt))

    instanceid <- .avcache$get("workspaceId")
    v <- .avcache$get("wdsApiVersion")
    api_endpoint <- "/{{instanceid}}/records/{{v}}/{{type}}/{{id}}"
    endpoint <- whisker.render(api_endpoint)

    base_uri <- workspace_data_service_url()
    uri <- paste0(base_uri, endpoint)
    response <- request(uri) |>
        req_auth_bearer_token(az_token()) |>
        req_perform() |>
        resp_body_json()

    response[["attributes"]] |>
        tibble::as_tibble()
}

#' @rdname workspace-dev-ops
delete_type <- function(type) {
    opt <- options(readr.show_col_types = FALSE)
    on.exit(options(opt))

    instanceid <- .avcache$get("workspaceId")
    v <- .avcache$get("wdsApiVersion")
    api_endpoint <- "/{{instanceid}}/types/{{v}}/{{type}}"
    endpoint <- whisker.render(api_endpoint)
    base_uri <- workspace_data_service_url()
    uri <- paste0(base_uri, endpoint)

    result <- request(uri) |>
        req_auth_bearer_token(az_token()) |>
        req_method("DELETE") |>
        req_perform() |>
        resp_is_error()

    !result
}
