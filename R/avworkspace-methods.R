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
#' @importFrom AnVILBase avworkspaces avstop_for_status
#' @exportMethod avworkspaces
setMethod(f = "avworkspaces", signature = "azure", definition =
    function(..., platform = cloud_platform()) {
        api_endpoint <- "/api/workspaces"
        url <- paste0(.RAWLS_URL, api_endpoint)
        qrs <- GET(
            url = url,
            add_headers(
                authorization = az_token()
            )
        )
        avstop_for_status(qrs, "avworkspaces")
        AnVILBase::flatten(qrs) |>
            AnVILBase::avworkspaces_clean()
    }
)

# avworkspace_namespace ---------------------------------------------------

.LEONARDO_URL <- "https://leonardo.dsde-prod.broadinstitute.org"

#' @describeIn avworkspace-methods List the workspace namespace
#'
#' @importFrom AnVILBase avworkspace_namespace avstop_for_status
#' @importFrom whisker whisker.render
#' @exportMethod avworkspace_namespace
setMethod(f = "avworkspace_namespace", signature = "azure", definition =
    function(..., platform = cloud_platform()) {
        api_endpoint <- "/api/v2/runtimes/{{workspaceid}}"
        workspaceid <- workspace_id()
        url <- paste0(.LEONARDO_URL, api_endpoint)
        url <- whisker.render(url)
        qrs <- GET(
            url = url,
            add_headers(
                authorization = az_token()
            )
        )
        avstop_for_status(qrs, "avworkspace_namespace")
        content(qrs)[[1L]][["labels"]][["saturnWorkspaceNamespace"]]
    }
)

# avworkspace_name --------------------------------------------------------

#' @describeIn avworkspace-methods Obtain the workspace name
#'
#' @importFrom AnVILBase avworkspace_name
#' @exportMethod avworkspace_name
setMethod(f = "avworkspace_name", signature = "azure", definition =
    function(..., platform = cloud_platform()) {
        .workspace_name()
    }
)

# avworkspace -------------------------------------------------------------

#' @describeIn avworkspace-methods Obtain the current workspace namespace and
#'   name combination
#'
#' @importFrom AnVILBase avworkspace
#' @exportMethod avworkspace
setMethod(f = "avworkspace", signature = "azure", definition =
    function(..., platform = cloud_platform()) {
        paste0(avworkspace_namespace(), "/", avworkspace_name())
    }
)
