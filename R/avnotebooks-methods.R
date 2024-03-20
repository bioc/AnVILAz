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
        local = FALSE,
        ...,
        platform = cloud_platform()
    ) {
        stopifnot(
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

#' @describeIn avnotebooks-methods Sync notebooks between the Azure Blob Storage
#'   Container and the local runtime
#'
#' @param destination `character(1)` or `missing` file path to the local file
#'   system directory for synchronization. The default location is
#'   `~/<workspace_data_service_url()>/analyses`. Out-of-date local files are
#'   replaced with the workspace version.
#'
#' @importFrom AnVILBase avnotebooks_localize
#' @exportMethod avnotebooks_localize
setMethod("avnotebooks_localize", signature = c(platform = "azure"),
    definition = function(
        destination = "./analyses",
        dry = TRUE,
        ...,
        platform = cloud_platform()
    ) {
        stopifnot(
            missing(destination) || isScalarCharacter(destination),
            isScalarLogical(dry)
        )

        source <- file.path(workspace_data_service_url(), "analyses")
        if (missing(destination)) {
            if (!dry && !dir.exists(destination))
                dir.create(destination, recursive = TRUE)
        }
        az_copy_from_storage(
            from = source, to = destination, recursive = TRUE, dry = dry
        )
    }
)


#' @describeIn avnotebooks-methods Sync notebooks between the local runtime and
#'   the Azure Blob Storage Container
#'
#' @param source `character(1)` or `missing` file path to the local file system
#'   directory for synchronization. The default location is the home folder.
#'   Out-of-date local files are replaced with the workspace version.
#'
#' @importFrom AnVILBase avnotebooks_delocalize
#' @exportMethod avnotebooks_delocalize
setMethod("avnotebooks_delocalize", signature = c(platform = "azure"),
    definition = function(
        source = "./",
        dry = TRUE,
        ...,
        platform = cloud_platform()
    ) {
        stopifnot(
            missing(source) || isScalarCharacter(source),
            isScalarLogical(dry)
        )

        files <- list.files(path = source)
        isNotebook <- tolower(tools::file_ext(files)) %in% c("ipynb", "rmd")
        notebooks <- file.path(source, files[isNotebook])
        if (!length(notebooks))
            stop("No notebooks found in ", source)

        lapply(
            notebooks,
            function(notebook)
                az_copy_to_storage(
                    from = notebook, to = "analyses/",
                    recursive = FALSE, dry = dry
                )
        )
    }
)
