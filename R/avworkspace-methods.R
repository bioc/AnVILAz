#' @name avworkspace-methods
#'
#' @title AnVIL Azure Workspace methods
#'
#' @inheritParams azure-methods
#'
#' @include azure-class.R
#'
NULL

# avworkspaces ------------------------------------------------------------

.RAWLS_URL <- "https://rawls.dsde-prod.broadinstitute.org"

#' @describeIn avworkspace-methods List workspaces
#'
#' @importFrom AnVILBase avworkspaces
#' @exportMethod avworkspaces
setMethod("avworkspaces", signature = c(platform = "azure"), definition =
    function(..., platform = cloud_platform()) {
        api_endpoint <- "/api/workspaces"
        url <- paste0(.RAWLS_URL, api_endpoint)
        qrs <- request(url) |>
            req_auth_bearer_token(az_token()) |>
            req_perform()

        AnVILBase::flatten(qrs) |>
            AnVILBase::avworkspaces_clean()
    }
)

# avworkspace_namespace ---------------------------------------------------

.LEONARDO_URL <- "https://leonardo.dsde-prod.broadinstitute.org"

#' @describeIn avworkspace-methods List the workspace namespace
#'
#' @importFrom AnVILBase avworkspace_namespace
#' @importFrom whisker whisker.render
#' @exportMethod avworkspace_namespace
setMethod("avworkspace_namespace", signature = c(platform = "azure"),
    definition = function(..., platform = cloud_platform()) {
        api_endpoint <- "/api/v2/runtimes/{{workspaceid}}"
        workspaceid <- .avcache$get("workspaceId")
        url <- paste0(.LEONARDO_URL, api_endpoint)
        url <- whisker.render(url)
        qrs <- request(url) |>
            req_auth_bearer_token(az_token()) |>
            req_perform() |>
            resp_body_json()

        qrs[[1L]][["labels"]][["saturnWorkspaceNamespace"]]
    }
)

# avworkspace_name --------------------------------------------------------

#' @describeIn avworkspace-methods Obtain the workspace name
#'
#' @importFrom AnVILBase avworkspace_name
#' @exportMethod avworkspace_name
setMethod("avworkspace_name", signature = c(platform = "azure"), definition =
    function(..., platform = cloud_platform()) {
        .avcache$get("workspaceName")
    }
)

# avworkspace -------------------------------------------------------------

#' @describeIn avworkspace-methods Obtain the current workspace namespace and
#'   name combination
#'
#' @importFrom AnVILBase avworkspace
#' @exportMethod avworkspace
setMethod("avworkspace", signature = c(platform = "azure"), definition =
    function(..., platform = cloud_platform()) {
        paste0(avworkspace_namespace(), "/", avworkspace_name())
    }
)
