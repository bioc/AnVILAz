#' @importFrom httr POST
#' @export
get_sas_token <- function(as = NULL, expiration = 28800) {
    workspaceId <- workspace_id()
    resourceId <- workspace_storage_cont_id()
    api_endpoint <- paste0(
        "/api/workspaces/v1/{{workspaceId}}/resources/",
        "controlled/azure/storageContainer/{{resourceId}}/getSasToken"
    )
    endpoint <- whisker.render(api_endpoint)
    url <- paste0(.DSDE_PROD_URL, endpoint)
    sas_tkn <- POST(
        url = url,
        query = list(sasExpirationDuration = expiration),
        add_headers(
            authorization = az_token()
        )
    )
    .stop_for_status(sas_tkn, "getSasToken")
    content(sas_tkn, as = as)
}
