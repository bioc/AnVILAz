#' @name az_copy-helpers
#'
#' @title Azure Copy command line utility helpers
#'
#' @description These functions invoke the `azcopy` command line utility. The
#'   utilities make use of a managed SAS token to mainly transfer files from the
#'   Azure workspace to the Azure Storage container. See `av_sas_token` for
#'   credential details.
#'
#' @details
#' * `az_copy_from_storage` - copy a file from the Azure Storage Container to
#'   the workspace environment
#' * `az_copy_to_storage` - copy a file from the workspace environment to the
#'   Azure Storage Container
#'
#' @param from `character(1)` A relative file path corresponding to
#'   either the remote (`az_copy_from_storage`) or local (`az_copy_to_storage`)
#'   file location. Remote locations should be relative to the base directory in
#'   the Azure Storage Container e.g., `analyses/jupyter.log`.
#'
#' @param to `character(1)` A relative file path corresponding to
#'   either the remote (`az_copy_to_storage`) or local (`az_copy_from_storage`)
#'   file location. Remote locations should be relative to the base directory in
#'   the Azure Storage Container. When not specified, it will default to the
#'   base directory of the remote location. The `to` path can be a folder path
#'   but must end in a forward slash (`/`). If the `to` path points to a
#'   non-existent directory, it will be created.
#'
#' @return
#' * `az_copy_from_storage` - called for the side effect of copying a file
#'   __from__ the Azure Storage Container
#' * `az_copy_to_storage` - called for the side effect of copying a file __to__
#'   the Azure Storage Container
#'
#' @examples
#' if (interactive()) {
#'
#'   ## local -> remote
#'   az_copy_to_storage("jupyter.log", "analyses/jupyter.log")
#'   az_copy_to_storage("jupyter.log", "analyses/test/")
#'
#'   ## placed in the base storage UUID directory
#'   az_copy_to_storage("jupyter.log")
#'
#'   ## remote -> local
#'   az_copy_from_storage("analyses/jupyter.log", "jupyter.log")
#'   ## download to the current directory
#'   az_copy_from_storage("analyses/jupyter.log")
#'
#' }
#' @export
az_copy_from_storage <-
    function(from, to = "./", recursive = FALSE, dry = TRUE)
{
    stopifnot(
        isScalarCharacter(from), isScalarCharacter(to),
        isScalarLogical(recursive), isScalarLogical(dry)
    )
    if (endsWith(from, "/"))
        stop("Provide a remote file location in the 'from' input")
    .validate_blob(from)

    if (!endsWith(to, "/"))
       stop("Provide a local directory with a forward slash (e.g., './to/')")

    isdir <- file.info(to)[["isdir"]]
    if (is.na(isdir) || !dir.exists(to))
        dir.create(to, recursive = TRUE)
    if (isTRUE(isdir) || endsWith(to, "/"))
        to <- file.path(normalizePath(to), basename(from))
    else
        to <- file.path(normalizePath(dirname(to)), basename(to))

    recurse <- tolower(as.character(recursive))

    sas_cred <- av_sas_token()
    wscu <- .avcache$get("wscu")
    token <- sas_cred[["token"]]
    path <- paste0(wscu, "/", from, "?")
    path <- paste0(path, token)

    .az_copy(
        shQuote(path), shQuote(to), paste0("--recursive=", recurse),
        if (dry) "--dry-run"
    )
}

#' @rdname az_copy-helpers
#' @export
az_copy_to_storage <-
    function(from, to, recursive = FALSE, dry = TRUE)
{
    if (!missing(to))
        stopifnot(
            isScalarCharacter(to)
        )

    stopifnot(
        isScalarCharacter(from),
        isScalarLogical(recursive), isScalarLogical(dry)
    )

    recurse <- tolower(as.character(recursive))

    sas_cred <- av_sas_token()
    wscu <- .avcache$get("wscu")
    token <- sas_cred[["token"]]
    path <- sas_cred[["url"]]

    if (!missing(to)) {
        path <- paste0(wscu, "/", to, "?")
        path <- paste0(path, token)
    }

    .az_copy(
        .az_shQuote(from), shQuote(path), paste0("--recursive=", recurse),
        if (dry) "--dry-run"
    )
}

