.get_sas_token <- function() {
    sas_tkn <- POST(
        url = paste0(
            .DSDE_PROD_URL, "api/workspaces/v1/", WSID,
            "/resources/controlled/azure/storageContainer/",
            WSCID, "/getSasToken"
        ),
        query = list(sasExpirationDuration=28800),
        add_headers(
            authorization = .az_token()
        )
    )
    content(sas_tkn)
}

