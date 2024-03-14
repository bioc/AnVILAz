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
#' @export
get_sas_token <- function(as = "parsed", sasExpirationDuration = 28800) {
    api_endpoint <- paste0(
        "/api/workspaces/v1/{{workspaceId}}/resources/",
        "controlled/azure/storageContainer/{{resourceId}}/getSasToken"
    )
    request(.DSDE_PROD_URL) |>
        req_template(
            api_endpoint,
            workspaceId = .avcache$get("workspaceId"),
            resourceId = .avcache$get("resourceId")
        ) |>
        req_auth_bearer_token(az_token()) |>
        req_url_query(sasExpirationDuration = sasExpirationDuration) |>
        req_method("POST") |>
        req_perform() |>
        resp_body_json()
}
