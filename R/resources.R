.DSDE_PROD_URL <- "https://workspace.dsde-prod.broadinstitute.org"

query_resources <- function(FUN = resp_body_json) {
    request(.DSDE_PROD_URL) |>
        req_template(
            "/api/workspaces/v1/{workspaceId}/resources",
            workspaceId = .avcache$get("workspaceId")
        ) |>
        req_auth_bearer_token(az_token()) |>
        req_url_query(stewardship = "CONTROLLED", limit = 1000) |>
        req_perform() |>
        FUN()
}

query_userName <- function() {
    userName <- query_resources(httr2::resp_body_string) |>
        rjsoncons::jmespath(
            paste0(
                "resources[?contains(metadata.resourceType,'AZURE_VM')].",
                "metadata.controlledResourceMetadata.privateResourceUser.",
                "userName"
            )
        ) |> jsonlite::fromJSON()
}
