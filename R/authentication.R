#' @export
az_token <- function() {
    system2("az", c("login",  "--identity", "--allow-no-subscriptions"))
    cli_token <- system2("az", c("account", "get-access-token"), stdout = TRUE)
    restkn <- jsonlite::fromJSON(cli_token)
    token <- restkn[["accessToken"]]
    paste('Bearer', token)
}
