#' @name avworkflow-methods
#'
#' @aliases avworkflow_jobs
#'
#' @title Azure Workflow methods
#'
#' @description `avworkflow_jobs()` reports the status of workflow executions
#'    in the current workspace.
#'
#' @details The `avworkflow_jobs_inputs()` function returns the input parameters
#'   for the workflow jobs as a `tibble`.
#'
#' @return `avworkflow_jobs()` returns a tibble with the status of the
#'   jobs in the current workspace.
#'
#' @inheritParams azure-methods
#' @inheritParams avnotebooks-methods
#'
#' @include azure-class.R
#'
#' @examples
#' if (has_avworkspace(strict = TRUE, platform = azure()))
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
        ...,
        platform = cloud_platform()
    ) {
        qrs <- .avrun_sets()
        .avworkflow_job(qrs)
    }
)

.avrun_sets <- function() {
    request(cbas_url()) |>
        req_template(
            "/api/batch/v1/run_sets"
        ) |>
        req_auth_bearer_token(az_token()) |>
        req_perform() |>
        resp_body_string()
}

#' @rdname avworkflow-methods
#'
#' @export
avworkflow_jobs_inputs <- function() {
    qrs <- .avrun_sets()
    input_tbls <- lapply(
        .jmespath_template(qrs, "input_definition"),
        jsonlite::fromJSON
    )
    names(input_tbls) <- .jmespath_template(qrs, "run_set_id")
    input_tbls
}
