#' @name avnotebooks-methods
#'
#' @aliases avnotebooks avnotebooks_localize avnotebooks_delocalize
#'
#' @title Azure Notebook Management
#'
#' @description `avnotebooks()` lists the notebooks in the current workspace.
#'
#' @inheritParams avdata-methods
#'
#' @param local `logical(1)` notebooks located on the workspace
#'   (`local = FALSE`, default) or runtime / local instance (`local = TRUE`).
#'   When `local = TRUE`, the notebook path is
#'   `<workspace_data_service_url()>/analyses`.
#'
#' @return `avnotebooks()` returns a character vector of files located
#'   in the workspace 'analyses/' folder path, or on the local file
#'   system.
#'
#' @examples
#' library(AnVILBase)
#' if (
#'     az_exists() && identical(avplatform_namespace(), "AnVILAz") &&
#'     nzchar(avworkspace_name())
#' )
#'     avnotebooks()
#'
NULL

#' @describeIn avnotebooks-methods List the notebooks in the current workspace
#'
#' @importFrom BiocBaseUtils isScalarLogical
#' @importFrom AnVILBase avnotebooks
#' @exportMethod avnotebooks
setMethod("avnotebooks", signature = c(platform = "azure"),
    definition = function(
        namespace = avworkspace_namespace(),
        name = avworkspace_name(),
        local = FALSE,
        ...,
        platform = cloud_platform()
    ) {
        stopifnot(
            isScalarCharacter(namespace), isScalarCharacter(name),
            isScalarLogical(local)
        )
        if (!local)
            files <- Filter(
                function(x) startsWith(x, "analyses/"), avlist()[["INFO"]]
            )
        else
            files <- list.files()
        isNotebook <- tolower(tools::file_ext(files)) %in% c("ipynb", "rmd")
        files[isNotebook]
    }
)
