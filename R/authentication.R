#' @title Generate an Azure authentication token with the `az` command line
#'   utility
#'
#' @description This function generates an Azure authentication token with the
#'   `az` command line utility. The token is used to authenticate with the Azure
#'   services. This function is called internally by the `az_*` functions. It
#'   is not meant to be called directly.
#'
#' @return A character string containing the authentication token with a
#'   "Bearer" prefix.
#'
#' @keywords internal
az_token <- function() {
    system2("az", c("login",  "--identity", "--allow-no-subscriptions"))
    cli_token <- system2("az", c("account", "get-access-token"), stdout = TRUE)
    restkn <- jsonlite::fromJSON(cli_token)
    restkn[["accessToken"]]
}
