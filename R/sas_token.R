#' Obtain the Shared Access Signature (SAS) for the Azure Storage
#'
#' The function provides a user delegation SAS token for management of
#' resources. Mainly used in other functions to move files to and from the
#' Azure Storage Container
#'
#' @inheritParams httr::content
#'
#' @param sasExpirationDuration `numeric(1)` The number of seconds until the SAS
#'   token expires (default: 28,800 seconds)
#'
#' @return A list of two elements named `token` and `url`
#'
#' @examples
#' if (interactive()) {
#'   sas <- get_sas_token()
#'   sas[["token"]]
#'   sas[["url"]]
#' }
#' @importFrom httr POST
#' @export
get_sas_token <- function(as = "parsed", sasExpirationDuration = 28800) {
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
        query = list(sasExpirationDuration = sasExpirationDuration),
        add_headers(
            authorization = az_token()
        )
    )
    .stop_for_status(sas_tkn, "getSasToken")
    content(sas_tkn, as = as)
}
