.az_token <- function() {
    system2("az", c("login",  "--identity", "--allow-no-subscriptions"))
    cli_token <- system2("az", c("account", "get-access-token"), stdout = TRUE)
    restkn <- jsonlite::fromJSON(cli_token)
    token <- restkn[["accessToken"]]
    paste('Bearer', token)
}

.workspace_id <- function() {
    opt <- Sys.getenv("WORKSPACE_ID", "")
    opt <- getOption("AnVILAz.workspace_id", opt)
    if (!nzchar(opt))
        character()
    else
        opt
}

.workspace_storage_cont_id <- function() {
    opt <- Sys.getenv("WORKSPACE_STORAGE_CONTAINER_ID", "")
    opt <- getOption("AnVILAz.workspace_storage_cont_id", opt)
    if (!nzchar(opt))
        character()
    else
        opt
}
