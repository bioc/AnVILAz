.az_token <- function() {
    system2("az", c("login",  "--identity", "--allow-no-subscriptions"))
    cli_token <- system2("az", c("account", "get-access-token"), stdout = TRUE)
    restkn <- jsonlite::fromJSON(cli_token)
    token <- restkn[["accessToken"]]
    paste('Bearer', token)
}

.workspace_id <- function() {
    opt <- Sys.getenv("WORKSPACE_ID", "")
    getOption("AnVILAz.workspace_id", opt)
}

.workspace_storage_cont_id <- function() {
    opt <- Sys.getenv("WORKSPACE_STORAGE_CONTAINER_ID", "")
    getOption("AnVILAz.workspace_storage_cont_id", opt)
}

.workspace_storage_cont_url <- function() {
    opt <- Sys.getenv("WORKSPACE_STORAGE_CONTAINER_URL", "")
    getOption("AnVILAz.workspace_storage_cont_url", opt)
}
