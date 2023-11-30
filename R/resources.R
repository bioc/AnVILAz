.DSDE_PROD_URL <- "https://workspace.dsde-prod.broadinstitute.org"

#' @importFrom httr GET content add_headers
#' @export
query_resources <- function(as = NULL) {
    workspaceId <- workspace_id()
    api_endpoint <- "/api/workspaces/v1/{{workspaceId}}/resources"
    endpoint <- whisker.render(api_endpoint)
    url <- paste0(.DSDE_PROD_URL, endpoint)
    qrs <- GET(
        url = url,
        query = list(stewardship = "CONTROLLED", limit = 1000),
        add_headers(
            authorization = az_token()
        )
    )
    .stop_for_status(qrs, "resources")
    content(qrs, as = as)
}

query_records <- function(type, version = .WDS_API_VERSION, as = NULL) {
    workspaceId <- workspace_id()
    v <- version
    api_endpoint <- "/{{workspaceId}}/search/{{v}}/{{type}}"
    endpoint <- whisker.render(api_endpoint)

    base_uri <- workspace_data_service_url()
    uri <- paste0(base_uri, endpoint)
    response <- POST(
        url = uri,
        body = list(
            offset = 0, limit = 10, sort = "asc", sortAttribute = "string"
        ),
        add_headers(authorization = az_token()),
        encode = "multipart",
        accept_json()
    )
    .stop_for_status(response, "query_records")
    content(response, as = as)
}
