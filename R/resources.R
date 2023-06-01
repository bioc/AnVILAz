.DSDE_PROD_URL <- "https://workspace.dsde-prod.broadinstitute.org/"

#' @importFrom httr GET content add_headers
.query_resources <- function() {
    qrs <- GET(
        url = paste0(.DSDE_PROD_URL, "api/workspaces/v1/", WSID, "/resources"),
        query = list(stewardship = "CONTROLLED", limit = 1000),
        add_headers(
            authorization = .az_token()
        )
    )
    content(qrs)
}
