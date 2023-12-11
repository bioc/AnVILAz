#' @name az_copy
#'
#' @title Azure Copy command line utility interface
#'
#' @description These functions invoke the `azcopy` command line utility. The
#'   utilities make use of a managed SAS token to mainly transfer files from the
#'   Azure workspace to the Azure Storage container. See `get_sas_token` for
#'   credential details.
#'
#' @details
#' * `az_copy_list` - list all the files in the Azure Storage Container
#' * `az_copy_from_storage` - copy a file from the Azure Storage Container to
#'   the workspace environment
#' * `az_copy_to_storage` - copy a file from the workspace environment to the
#'   Azure Storage Container
#' * `az_copy` - a generalized interface for either `az_copy_from_storage` or
#'   `az_copy_to_storage`; deduced from the `source` and `destination` inputs
#' * `az_copy_rm` - remove a file or folder from the Azure Storage Container
#' * `az_copy_backup` - copy a directory from the workspace environment to the
#'   Azure Storage Container
#' * `az_copy_restore` - copy a directory from the Azure Storage Container to
#'   the workspace environment
#'
#' @param source,from `character(1)` A relative file path corresponding to
#'   either the remote (`az_copy_from_storage`) or local (`az_copy_to_storage`)
#'   file location. Remote locations should be relative to the base directory in
#'   the Azure Storage Container e.g., `analyses/jupyter.log`.
#'
#' @param from_dir `character(1)` A relative folder path that corresponds to
#'   either the remote (`az_copy_backup`) or local (`az_copy_restore`) directory
#'   location. Local locations are relative to the working directory, usually
#'   the home directory. Remote locations should be relative to the base
#'   directory in the Azure Storage Container e.g., `analyses/`.
#'
#' @param destination,to `character(1)` A relative file path corresponding to
#'   either the remote (`az_copy_to_storage`) or local (`az_copy_from_storage`)
#'   file location. Remote locations should be relative to the base directory in
#'   the Azure Storage Container. When not specified, it will default to the
#'   base directory of the remote location. The `to` path can be a folder path
#'   but must end in a forward slash (`/`). If the `to` path points to a
#'   non-existent directory, it will be created.
#'
#' @param to_dir `character(1)` A relative folder path pointing to either the
#' remote (`az_copy_backup`) or local (`az_copy_restore`) directory. When
#' performing a 'restore' operation with `az_copy_restore`, the default `to_dir`
#' location is the current working directory, i.e., `"."`.
#'
#' @param contentsOnly `logical(1)` Whether to only upload / download the
#'   contents of the `from_dir`
#'
#' @param blob_file `character(1)` A relative path to a file or folder in the
#'   Azure Storage Container to be removed. If a folder is specified, all files
#'   in the folder will be removed. Folder inputs **must** end with a forward
#'   slash (`/`).
#'
#' @param recursive `logical(1)` Whether to recursively remove files in a
#'   directory. Only applies to `az_copy_rm`. Default is `FALSE`.
#'
#' @return
#' * `az_copy_list` - a `tibble` of files and metadata
#' * `az_copy_from_storage` - called for the side effect of copying a file
#'   __from__ the Azure Storage Container
#' * `az_copy_to_storage` - called for the side effect of copying a file __to__
#'   the Azure Storage Container
#' * `az_copy` - called for the side effect of copying a file __to__ or __from__
#'   the Azure Storage Container depending on the `source` and `destination`
#'   inputs
#' * `az_copy_rm` - called for the side effect of removing a file or folder
#' * `az_copy_backup` - called for the side effect of copying a directory __to__
#'   the Azure Storage Container
#' * `az_copy_restore` - called for the side effect of copying a directory
#'   __from__ the Azure Storage Container
#'
#' @examples
#' if (interactive()) {
#'
#'   az_copy_list()
#'
#'   ## local -> remote
#'   az_copy_to_storage("jupyter.log", "analyses/jupyter.log")
#'   az_copy_to_storage("jupyter.log", "analyses/test/")
#'   ## using general interface az_copy
#'   az_copy("jupyter.log", "analyses/jupyter.log")
#'
#'   ## placed in the base storage UUID directory
#'   az_copy_to_storage("jupyter.log")
#'   ## upload a directory
#'   az_copy_backup("./test/", "analyses/test/", contentsOnly = TRUE)
#'
#'   ## remote -> local
#'   az_copy_from_storage("analyses/jupyter.log", "jupyter.log")
#'   ## download to the current directory
#'   az_copy_from_storage("analyses/jupyter.log")
#'   ## using general interface az_copy
#'   az_copy("analyses/jupyter.log", "./jupyter.log")
#'   ## download a directory
#'   az_copy_restore("analyses/test/", "./test/", contentsOnly = TRUE)
#'
#'   az_copy_rm("analyses/jupyter.log")
#'
#' }
#' @export
az_copy_from_storage <- function(from, to = "./") {
    stopifnot(
        isScalarCharacter(from), isScalarCharacter(to)
    )
    if (endsWith(from, "/"))
        stop("Provide a remote file location in the 'from' input")
    .validate_blob(from)

    isdir <- file.info(to)[["isdir"]]
    if (isTRUE(isdir) || endsWith(to, "/"))
        to <- file.path(normalizePath(to), basename(from))
    else
        to <- file.path(normalizePath(dirname(to)), basename(to))

    sas_cred <- get_sas_token()
    wscu <- workspace_storage_cont_url()
    token <- sas_cred[["token"]]
    path <- paste0(wscu, "/", from, "?")
    path <- paste0(path, token)

    .az_copy(from = shQuote(path), to = shQuote(to))
}

#' @rdname az_copy
#' @export
az_copy_to_storage <- function(from, to) {
    if (!missing(to))
        stopifnot(
            isScalarCharacter(to)
        )

    stopifnot(
        isScalarCharacter(from)
    )

    sas_cred <- get_sas_token()
    wscu <- workspace_storage_cont_url()
    token <- sas_cred[["token"]]
    path <- sas_cred[["url"]]

    if (!missing(to)) {
        path <- paste0(wscu, "/", to, "?")
        path <- paste0(path, token)
    }

    .az_copy(.az_shQuote(from), shQuote(path))
}

.is_remote_path <- function(path) {
    startsWith(path, "http") || !file.exists(path)
}

#' @rdname az_copy
#' @export
az_copy <- function(source, destination, ...) {
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

#' @rdname az_copy
#' @export
az_copy_list <- function() {
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

#' @rdname az_copy
#' @export
az_copy_rm <- function(blob_file, recursive = FALSE) {
    stopifnot(
        isScalarCharacter(blob_file)
    )
    .validate_blob(blob_file)

    wscu <- workspace_storage_cont_url()
    sas_cred <- get_sas_token()
    token_slug <- sas_cred[["token"]]
    path <- paste0(wscu, "/", blob_file, "?")
    path <- shQuote(paste0(path, token_slug))
    recurse <- paste0("--recursive=", tolower(recursive))
    args <- c("rm", path, recurse)
    .az_do("azcopy", args = args)
}

.validate_blob <- function(blob) {
    file_tbl <- az_copy_list()
    allfiles <- file_tbl[["INFO"]]
    is_dir <- endsWith(blob, "/")
    if (is_dir && !any(startsWith(allfiles, blob)))
        stop("Virtual directory not found; check path with `az_copy_list`")

    if (!blob %in% allfiles && !is_dir)
        stop("File not found; check path to blob file with `az_copy_list`")

    TRUE
}

#' @rdname az_copy
#' @export
az_copy_backup <- function(from_dir, to_dir, contentsOnly = FALSE) {
    stopifnot(
        isScalarCharacter(from_dir),
        dir.exists(from_dir)
    )

    from_dir <- gsub("\\/$", "", from_dir)
    from_dir <- normalizePath(from_dir)
    if (contentsOnly)
        from_dir <- paste0(from_dir, "/*")

    sas_cred <- get_sas_token()
    wscu <- workspace_storage_cont_url()
    token <- sas_cred[["token"]]
    path <- sas_cred[["url"]]
    if (!missing(to_dir)) {
        to_dir <- gsub("\\/$", "", to_dir)
        path <- paste0(wscu, "/", to_dir, "?")
        path <- paste0(path, token)
    }

    .az_copy(
        from = shQuote(from_dir), to = shQuote(path), "--recursive=true"
    )
}

#' @rdname az_copy
#' @export
az_copy_restore <- function(from_dir, to_dir = ".", contentsOnly = FALSE) {
    stopifnot(
        isScalarCharacter(to_dir),
        isScalarCharacter(from_dir)
    )
    if (!dir.exists(to_dir))
        dir.create(to_dir, recursive = TRUE)

    from_dir <- gsub("\\/$", "", from_dir)
    if (contentsOnly)
        from_dir <- paste0(from_dir, "/*")

    sas_cred <- get_sas_token()
    wscu <- workspace_storage_cont_url()
    token <- sas_cred[["token"]]
    path <- paste0(wscu, "/", from_dir, "?")
    path <- paste0(path, token)

    .az_copy(
        from = shQuote(path), to = .az_shQuote(to_dir), "--recursive=true"
    )
}

#' @importFrom BiocBaseUtils isScalarCharacter isCharacter
.az_do <- function(command, args) {
    stopifnot(
        isScalarCharacter(command),
        isCharacter(args, na.ok = FALSE)
    )
    bin <- .az_find(command)
    res <- withCallingHandlers({
        tryCatch({
            system2(bin, args, stdout = TRUE, stderr = TRUE, wait=TRUE)
        }, error = function(err) {
            msg <- paste0(
                "'", command, " ", paste(args, collapse = " "), "' failed:\n",
                "  ", conditionMessage(err)
            )
            stop(msg, call. = FALSE)
        })
    }, warning = function(warn) {
        invokeRestart("muffleWarning")
    })
    if (!is.null(attr(res, "status"))) {
        msg <- paste0(
            "'", command, " ", paste(args, collapse = " "), "' failed:",
            "\n  ", paste(as.vector(res), collapse = "\n    "),
            "\n  exit status: ", attr(res, "status")
        )
        stop(msg, call. = FALSE)
    }
    res
}

.az_find <- function(command) {
    bin <- Sys.which(command)
    if (nzchar(bin))
        normalizePath(bin)
    else
        stop("failed to find '", command, "' binary", call. = FALSE)
}

.az_copy <- function(from, to, ...) {
    args <- c("copy", from, to, ...)
    .az_do("azcopy", args = args)
}
