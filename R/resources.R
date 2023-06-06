.DSDE_PROD_URL <- "https://workspace.dsde-prod.broadinstitute.org/"

#' @importFrom httr GET content add_headers
#' @export
query_resources <- function(as = NULL) {
    qrs <- GET(
        url = paste0(
            .DSDE_PROD_URL,
            "api/workspaces/v1/",
            workspace_id(),
            "/resources"
        ),
        query = list(stewardship = "CONTROLLED", limit = 1000),
        add_headers(
            authorization = az_token()
        )
    )
    .stop_for_status(qrs, "resources")
    content(qrs, as = as)
}
