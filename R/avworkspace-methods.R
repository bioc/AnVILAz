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
#' @return `avworkspaces()`: a `tibble` table of available workspaces
#'
#' @importFrom AnVILBase avworkspaces
#' @exportMethod avworkspaces
setMethod("avworkspaces", signature = c(platform = "azure"), definition =
    function(..., platform = cloud_platform()) {
        qrs <- request(.RAWLS_URL) |>
            req_template("/api/workspaces") |>
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
#' @return `avworkspace_namespace()`: a character string of the workspace
#'   namespace
#'
#' @importFrom AnVILBase avworkspace_namespace
#' @exportMethod avworkspace_namespace
setMethod("avworkspace_namespace", signature = c(platform = "azure"),
    definition = function(..., platform = cloud_platform()) {
        qrs <- request(.LEONARDO_URL) |>
            req_template(
                "/api/v2/runtimes/{workspaceid}",
                workspaceid = .avcache$get("workspaceId")
            ) |>
            req_auth_bearer_token(az_token()) |>
            req_perform() |>
            resp_body_json()

        qrs[[1L]][[c("labels", "saturnWorkspaceNamespace")]]
    }
)

# avworkspace_name --------------------------------------------------------

#' @describeIn avworkspace-methods Obtain the workspace name
#'
#' @return `avworkspace_name()`: a character string of the workspace name
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
#' @return `avworkspace()`: a character string of the workspace namespace and
#'  name combination
#'
#' @importFrom AnVILBase avworkspace
#' @exportMethod avworkspace
setMethod("avworkspace", signature = c(platform = "azure"), definition =
    function(..., platform = cloud_platform()) {
        paste0(avworkspace_namespace(), "/", avworkspace_name())
    }
)

# avworkspace_clone -------------------------------------------------------

#' @describeIn avworkspace-methods Clone a workspace
#'
#' @inheritParams avnotebooks-methods
#' @inheritParams azure-methods
#'
#' @param namespace `character(1)` AnVIL workspace namespace as returned by,
#'   e.g., `avworkspace_namespace()`
#'
#' @param name `character(1)` AnVIL workspace name as returned by, eg.,
#'   `avworkspace_name()`.
#'
#' @param to_namespace `character(1)` workspace (billing account) in
#'     which to make the clone.
#'
#' @param to_name `character(1)` name of the cloned workspace.
#'
#' @param bucket_location `character(1)` region in which bucket
#'   attached to the workspace should be created. The default is set to a
#'   single region ("US"); multi-region is available but more costly.
#'
#' @return `avworkspace_clone()`: called for the side-effect of cloning a
#'  workspace to a new namespace and name.
#'
#' @importFrom AnVILBase avworkspace_clone
#' @exportMethod avworkspace_clone
setMethod("avworkspace_clone", signature = c(platform = "azure"),
    definition = function(
        namespace = avworkspace_namespace(),
        name = avworkspace_name(),
        to_namespace = namespace,
        to_name,
        bucket_location = "US",
        ..., platform = cloud_platform()
    ) {
        stopifnot(
            isScalarCharacter(to_name),
            isScalarCharacter(to_namespace),
            isScalarCharacter(bucket_location),
            `source and destination 'namespace/name' must be different` =
                !identical(namespace, to_namespace) || !identical(name, to_name)
        )

        api_endpoint <-
            "/api/workspaces/{workspaceNamespace}/{workspaceName}/clone"

        request(.RAWLS_URL) |>
            req_template(
                api_endpoint,
                workspaceNamespace = namespace,
                workspaceName = name
            ) |>
            req_auth_bearer_token(az_token()) |>
            req_body_json(
                list(
                    namespace = to_namespace,
                    name = to_name,
                    authorizationDomain = list(), # []
                    attributes = structure(list(), .Names = character()), # {}
                    copyFilesWithPrefix = "analyses/",
                    # createdBy = userName,
                    bucketLocation = bucket_location,
                    enhancedBucketLogging = FALSE
                )
            ) |>
            req_perform()

        paste(to_namespace, to_name, sep = "/")
    }
)
