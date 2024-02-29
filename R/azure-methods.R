#' @name azure-methods
#'
#' @title A number of methods compatible with the Azure platform class.
#'
#' @param source `character(1)` A relative file path corresponding to
#'   either the remote (`az_copy_from_storage`) or local (`az_copy_to_storage`)
#'   file location. Remote locations should be relative to the base directory in
#'   the Azure Storage Container e.g., `analyses/jupyter.log`.
#'
#' @param destination `character(1)` A relative file path corresponding to
#'   either the remote (`az_copy_to_storage`) or local (`az_copy_from_storage`)
#'   file location. Remote locations should be relative to the base directory in
#'   the Azure Storage Container. When not specified, it will default to the
#'   base directory of the remote location. The `to` path can be a folder path
#'   but must end in a forward slash (`/`). If the `to` path points to a
#'   non-existent directory, it will be created.
#'
#' @param recursive `logical(1)` Whether to recursively move or remove files in
#'   a directory. Only applies to `avremove`, `avbackup`, and `avrestore`.
#'   Default is `TRUE` for `backup` and `restore` operations and `FALSE` for
#'   `avremove`.
#'
#' @param platform `character(1)` The platform class to dispatch on. The default
#'   is `"azure"` for the `AnVILAz` package.
#'
#' @param ... Additional arguments passed to the underlying methods (not used).
#'
#' @details  The `recursive` argument for `avbackup` and `avrestore` is set to
#'   `TRUE` by default and `FALSE` for `avremove`. Note that wildcards are not
#'   supported for local or remote paths.
#'
#' @return
#' * `avlist` - a `tibble` of files and metadata
#' * `avcopy` - called for the side effect of copying a file __to__ or __from__
#'   the Azure Storage Container depending on the `source` and `destination`
#'   inputs
#' * `avremove` - called for the side effect of removing a file or folder
#' * `avbackup` - called for the side effect of copying a directory __to__
#'   the Azure Storage Container
#' * `avrestore` - called for the side effect of copying a directory
#'   __from__ the Azure Storage Container
#' * `avstorage` - a URL string of the Azure Storage Container location
#' * `avworkspaces` - a tibble of workspaces on AnVIL
#' * `avtable_import` - a response list indicating successful upload
#' * `avtable_delete_values` - when successful, a `NULL` value
#'
#' @examples
#' if (interactive()) {
#'
#'   avlist()
#'
#'   ## local -> remote
#'   ## using general interface avcopy
#'   avcopy("jupyter.log", "analyses/jupyter.log")
#'
#'   ## upload a directory
#'   avbackup("./test/", "analyses/test/")
#'
#'   ## using general interface az_copy
#'   avcopy("analyses/jupyter.log", "./jupyter.log")
#'
#'   ## download a directory
#'   avrestore("analyses/test/", "./test/")
#'
#'   avremove("analyses/jupyter.log")
#'
#' }
NULL

# avcopy ------------------------------------------------------------------

#' @describeIn azure-methods a generalized interface for either
#'   `az_copy_from_storage` or `az_copy_to_storage`; deduced from the `source`
#'   and `destination` inputs
#'
#' @importFrom AnVILBase avcopy
#' @exportMethod avcopy
setMethod(f = "avcopy", signature = c(platform = "azure"), definition =
    function(source, destination, ..., platform = cloud_platform()) {
        stopifnot(
            isScalarCharacter(source)
        )
        if (.is_remote_path(source))
            az_copy_from_storage(from = source, to = destination)
        else if (
            file.exists(source) && (
                .is_remote_path(destination) || missing(destination)
            )
        )
            az_copy_to_storage(from = source, to = destination)
        else
            stop("Invalid source or destination path")
    }
)

# avlist ------------------------------------------------------------------

#' @describeIn azure-methods list all the files in the Azure Storage Container
#'
#' @importFrom AnVILBase avlist
#' @exportMethod avlist
setMethod(f = "avlist", signature = c(platform = "azure"), definition =
    function(..., platform = cloud_platform()) {
        path <- get_sas_token()[["url"]]
        args <- c("list", shQuote(path))
        output <- .az_do("azcopy", args = args)
        files <- strsplit(output, "; ")
        files <- lapply(
            files,
            function(x) gsub("INFO:\\s+|Content\\sLength:\\s+", "", x)
        )
        res <- do.call(rbind.data.frame, files)
        names(res) <- c("INFO", "Content.Length")
        tibble::as_tibble(res)
    }
)

# avremove ----------------------------------------------------------------

#' @describeIn azure-methods remove a file or directory from the Azure Storage
#'
#' @importFrom AnVILBase avremove
#' @exportMethod avremove
setMethod(f = "avremove", signature = c(platform = "azure"), definition =
    function(source, recursive = FALSE, ..., platform = cloud_platform()) {
        stopifnot(
            isScalarCharacter(source)
        )
        .validate_blob(source)
        wscu <- .avcache$get("wscu")
        sas_cred <- get_sas_token()
        token_slug <- sas_cred[["token"]]
        path <- paste0(wscu, "/", source, "?")
        path <- shQuote(paste0(path, token_slug))
        recurse <- paste0("--recursive=", tolower(recursive))
        args <- c("rm", path, recurse)
        .az_do("azcopy", args = args)
    }
)

# avbackup ----------------------------------------------------------------

#' @describeIn azure-methods copy a directory from the workspace environment to
#'   the Azure Storage Container
#'
#' @importFrom AnVILBase avbackup
#' @exportMethod avbackup
setMethod(f = "avbackup", signature = c(platform = "azure"), definition =
    function(
        source, destination, recursive = TRUE, ...,
        platform = cloud_platform()
    ) {
        if (endsWith(source, "*"))
            stop("Source path cannot end with a wildcard")

        stopifnot(
            isScalarCharacter(source), dir.exists(source),
            is.logical(recursive)
        )

        source <- gsub("\\/$", "", source)
        source <- normalizePath(source)

        wscu <- .avcache$get("wscu")
        sas_cred <- get_sas_token()
        token <- sas_cred[["token"]]
        path <- sas_cred[["url"]]
        if (!missing(destination)) {
            destination <- gsub("\\/$", "", destination)
            path <- paste0(wscu, "/", destination, "?")
            path <- paste0(path, token)
        }

        .az_copy(
            from = shQuote(source), to = shQuote(path),
            paste0("--recursive=", tolower(recursive))
        )
    }
)

# avrestore ---------------------------------------------------------------

#' @describeIn azure-methods copy a file or directory from the Azure Storage
#'   Container to the workspace environment
#'
#' @importFrom AnVILBase avrestore
#' @exportMethod avrestore
setMethod(f = "avrestore", signature = c(platform = "azure"), definition =
    function(
        source, destination, recursive = TRUE, ...,
        platform = cloud_platform()
    ) {
        stopifnot(
            isScalarCharacter(source),
            isScalarCharacter(destination)
        )
        if (!dir.exists(destination))
            dir.create(destination, recursive = TRUE)

        source <- gsub("\\/$", "", source)

        sas_cred <- get_sas_token()
        token <- sas_cred[["token"]]
        wscu <- .avcache$get("wscu")
        path <- paste0(wscu, "/", source, "?")
        path <- paste0(path, token)

        .az_copy(
            from = shQuote(path), to = .az_shQuote(destination),
            paste0("--recursive=", tolower(recursive))
        )
    }
)

# avstorage ---------------------------------------------------------------

#' @describeIn azure-methods The base URI string used to move data to and from
#'   the Azure Storage Container
#'
#' @importFrom AnVILBase avstorage
#' @exportMethod avstorage
setMethod(f = "avstorage", signature = c(platform = "azure"), definition =
    function(..., platform = cloud_platform()) {
        .avcache$get("wscu")
    }
)

