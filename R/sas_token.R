#' @importFrom httr POST
.get_sas_token <- function() {
    sas_tkn <- POST(
        url = paste0(
            .DSDE_PROD_URL,
            "api/workspaces/v1/",
            .workspace_id(),
            "/resources/controlled/azure/storageContainer/",
            .workspace_storage_cont_id(),
            "/getSasToken"
        ),
        query = list(sasExpirationDuration=28800),
        add_headers(
            authorization = .az_token()
        )
    )
    content(sas_tkn)
}
