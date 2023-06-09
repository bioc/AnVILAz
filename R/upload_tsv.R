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

