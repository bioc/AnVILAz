# AnVIL/R/av.R
#' @importFrom httr status_code http_condition headers
.stop_for_status <-
    function(response, op)
{
    status <- status_code(response)
    if (status < 400L)
        return(invisible(response))

    cond <- http_condition(status, "error")
    type <- headers(response)[["content-type"]]
    msg <- NULL
    if (nzchar(type) && grepl("application/json", type)) {
        content <- as.list(response)
        msg <- content[["message"]]
        if (is.null(msg))
            ## e.g., from bond DRS server
            msg <- content$response$text
    } else if (nzchar(type) && grepl("text/html", type)) {
        ## these pages can be too long for a standard 'stop()' message
        cat(as.character(response), file = stderr())
    }

    message <- paste0(
        "'", op, "' failed:\n  ",
        conditionMessage(cond),
        if (!is.null(msg)) "\n  ", msg
    )
    stop(message, call.=FALSE)
}

## from terra-workspace-data-service/docs/WDS Python Client.md
#' @importFrom httr accept_json
#' @export
get_wds_url <- function(env = "prod") {
    uri <- paste0(
        "https://leonardo.dsde-",
        env,
        ".broadinstitute.org/api/apps/v2/",
        .workspace_id()
    )
    url_resp <- GET(
        url = uri,
        query = list(includeDeleted = "false"),
        accept_json(),
        add_headers(
            authorization = .az_token()
        )
    )
    .stop_for_status(url_resp, "wds_url")
    res_json <- content(url_resp, type = "text")
    res_url <- rjsoncons::jmespath(res_json, "[*].proxyUrls.wds")
    jsonlite::fromJSON(res_url)
}
