#' @name avworkflow-methods
#'
#' @aliases avworkflow_jobs
#'
#' @title Azure Workflow methods
#'
#' @description `avworkflow_jobs()` reports the status of workflow executions
#'    in the current workspace.
#'
#' @return `avworkflow_jobs()` returns a tibble with the status of the
#'   jobs in the current workspace.
#'
#' @inheritParams azure-methods
#'
#' @include azure-class.R
#'
#' @examples
#' library(AnVILBase)
#' if (
#'     az_exists() && identical(avplatform_namespace(), "AnVILAz") &&
#'     nzchar(avworkspace_name())
#' )
#'     ## from within AnVIL
#'     avworkflow_jobs()
#'
NULL

.jmespath_template <-
    function(x, value, base = "run_sets[*]")
{
    rjsoncons::jmespath(x, paste(base, value, sep = "."), as = "R")
}

.CBAS_VALUES <- c(
    "run_set_id", "run_set_name", "run_set_description", "state",
    "submission_timestamp", "last_modified_timestamp", "run_count",
    "error_count", "user_id"
)

#' @importFrom rjsoncons jmespath
.avworkflow_job <-
    function(x)
{
    vals <- structure(.CBAS_VALUES, .Names = .CBAS_VALUES)
    listvals <- lapply(vals, .jmespath_template, x = x)
    do.call(data.frame, listvals) |>
        tibble::as_tibble()
}

#  avworkflow_jobs ----------------------------------------------------------

#' @describeIn avworkflow-methods List the status of workflow jobs
#'
#' @importFrom AnVILBase avworkflow_jobs
#' @exportMethod avworkflow_jobs
setMethod("avworkflow_jobs", signature = c(platform = "azure"),
    definition = function(
        namespace = avworkspace_namespace(),
        name = avworkspace_name(),
        ...,
        platform = cloud_platform()
    ) {
        stopifnot(
            isScalarCharacter(namespace), isScalarCharacter(name)
        )

        endpoint <-
            # "/api/workspaces/{workspaceNamespace}/{workspaceName}/submissions"
            "/api/batch/v1/run_sets"
        qrs <- request(cbas_url()) |>
            req_template(
                endpoint,
                workspaceNamespace = namespace,
                workspaceName = URLencode(name)
            ) |>
            req_auth_bearer_token(az_token()) |>
            req_perform() |>
            resp_body_string()

        .avworkflow_job(qrs)
    }
)
