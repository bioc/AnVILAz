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
