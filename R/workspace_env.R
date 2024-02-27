#' @name workspace-env
#'
#' @aliases workspace_id workspace_storage_container_id
#'   workspace_storage_container_url workspace_data_service_url
#'
#' @title Access Terra on Azure workspace session variables
#'
#' @description A group of functions that return environment variables in the
#'   Terra Azure workspace. The Workspace Data Service URL sends out an API
#'   `GET` request to obtain the data services URL for uploading data to the
#'   workspace "DATA" tab.
#'
#' @param env `character(1)` One of "prod" or "dev" for the 'leonardo.dsde' URL
#'   endpoint
#'
#' @return
#' * `workspace_id` - A UUID string referring to "workspaceId" or "workspaceid"
#'   in API calls
#' * `workspace_storage_cont_id` - A UUID string identifiying the resource
#'   storage container owned by the user account, a.k.a. "resourceId"
#' * `workspace_storage_cont_url` - The base URI string used to move data
#'   to and from the Azure Storage Container
#' * `wds_api_version` - The version of the Workspace Data Service API, defaults
#'   to "v0.2"
#' * `workspace_data_service_url` - The base URI string used to move data to
#'   to and from the workspace "DATA" tab
#' * `cbas_url` - The base URI string used to query the workflow submission
#'   history
#'
#' @examples
#' workspace_id()
#' workspace_storage_cont_id()
#' workspace_storage_cont_url()
#' if (interactive()) {
#'   workspace_data_service_url()
#'   cbas_url()
#' }
#' @export
workspace_id <- function() {
    opt <- Sys.getenv("WORKSPACE_ID", "")
    getOption("AnVILAz.workspace_id", opt)
}

.workspace_name <- function() {
    opt <- Sys.getenv("WORKSPACE_NAME", "")
    getOption("AnVILAz.workspace_name", opt)
}

#' @rdname workspace-env
#' @export
workspace_storage_cont_id <- function() {
    opt <- Sys.getenv("WORKSPACE_STORAGE_CONTAINER_ID", "")
    getOption("AnVILAz.workspace_storage_cont_id", opt)
}

#' @rdname workspace-env
#' @export
workspace_storage_cont_url <- function() {
    opt <- Sys.getenv("WORKSPACE_STORAGE_CONTAINER_URL", "")
    getOption("AnVILAz.workspace_storage_cont_url", opt)
}

#' @rdname workspace-env
#' @export
wds_api_version <- function() {
    opt <- Sys.getenv("WDS_API_VERSION", "v0.2")
    getOption("AnVILAz.wds_api_version", opt)
}

## from terra-workspace-data-service/docs/WDS Python Client.md

#' @rdname workspace-env
#' @importFrom httr accept_json
#' @export
workspace_data_service_url <- function(env = "prod") {
    workspaceId <- .avcache$get("workspaceId")
    api_url <- paste0(
        "https://leonardo.dsde-{{env}}.broadinstitute.org",
        "/api/apps/v2/{{workspaceId}}"
    )
    uri <- whisker.render(api_url)
    url_resp <- GET(
        url = uri,
        query = list(includeDeleted = "false"),
        accept_json(),
        add_headers(
            authorization = az_token()
        )
    )
    avstop_for_status(url_resp, "wds_url")
    res_json <- content(url_resp, type = "text", encoding = "UTF-8")
    res_url <- rjsoncons::jmespath(res_json, "[*].proxyUrls.wds")
    jsonlite::fromJSON(res_url)
}

#' @rdname workspace-env
#' @export
cbas_url <- function(env = "prod") {
    workspaceId <- .avcache$get("workspaceId")
    api_url <- paste0(
        "https://leonardo.dsde-{{env}}.broadinstitute.org",
        "/api/apps/v2/{{workspaceId}}"
    )
    uri <- whisker.render(api_url)
    url_resp <- GET(
        url = uri,
        query = list(includeDeleted = "false"),
        accept_json(),
        add_headers(
            authorization = az_token()
        )
    )
    avstop_for_status(url_resp, "cbas_url")
    res_json <- content(url_resp, type = "text", encoding = "UTF-8")
    res_url <- rjsoncons::jmespath(res_json, "[*].proxyUrls.cbas")
    jsonlite::fromJSON(res_url)
}

# .avcache ----------------------------------------------------------------

# Cache for workspace environment variables
.avcache <- local({
    hash <- new.env(parent = emptyenv())
    list(
        get = function(key) {
            hash[[key]]
        },
        set = function(key, value) {
            if (!is.null(value))
                hash[[key]] <- value
            hash[[key]]
        },
        keys = function() {
            names(hash)
        },
        reset = function() {
            rm(list = ls(hash), envir = hash)
        }
    )
})
