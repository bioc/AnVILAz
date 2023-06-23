#' @name az_copy
#'
#' @aliases az_copy_list az_copy_copy az_copy_rm
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
#' * `az_copy_copy` - copy a file from the Azure workspace to the container
#' * `az_copy_rm` - remove a file from the Azure Storage Container
#'
#' @param from `character(1)` A relative file path corresponding to either the
#'   remote or local (working directory) file location. Remote locations should
#'   be relative to the base directory in the Azure Storage Container.
#'
#' @param to `character(1)` A relative file path corresponding to either the
#'   remote or local (working directory) file location. Remote locations should
#'   be relative to the base directory in the Azure Storage Container.
#'
#' @param blob_file `character(1)` A relative path to a file in the Azure
#'   Storage Container to be removed
#'
#' @return
#' * `az_copy_list` - a `tibble` of files and metadata
#' * `az_copy_from_storage` - called for the side effect of copying a file
#'   __from__ the Azure Storage Container
#' * `az_copy_to_storage` - called for the side effect of copying a file __to__
#'   the Azure Storage Container
#' * `az_copy_rm` - called for the side effect of removing a file
#'
#' @examples
#' if (interactive()) {
#'
#'   az_copy_list()
#'   az_copy_to_storage("jupyter.log", "analyses/jupyter.log")
#'   az_copy_from_storage("analyses/jupyter.log", "jupyter.log")
#'   az_copy_rm("analyses/jupyter.log")
#'
#' }
#' @export
az_copy_from_storage <- function(from, to) {
    stopifnot(
        isScalarCharacter(from), isScalarCharacter(to)
    )
    sas_cred <- get_sas_token()
    wscu <- workspace_storage_cont_url()
    token <- sas_cred[["token"]]
    path <- sas_cred[["url"]]

    if (!missing(from)) {
        path <- file.path(wscu, from, "?")
        path <- paste0(path, token)
    }

    .az_copy(path, to)
}

#' @rdname az_copy
#' @export
az_copy_to_storage <- function(from, to) {
    stopifnot(
        isScalarCharacter(from), isScalarCharacter(to)
    )
    sas_cred <- get_sas_token()
    wscu <- workspace_storage_cont_url()
    token <- sas_cred[["token"]]
    path <- sas_cred[["url"]]

    if (!missing(to)) {
        path <- file.path(wscu, to, "?")
        path <- paste0(path, token)
    }

    .az_copy(from, path)
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
az_copy_rm <- function(blob_file) {
    stopifnot(
        isScalarCharacter(blob_file)
    )
    file_tbl <- az_copy_list()
    allfiles <- file_tbl[["INFO"]]
    if (!blob_file %in% allfiles)
        stop("File not found; check path to blob file with `az_copy_list`")

    wscu <- workspace_storage_cont_url()
    sas_cred <- get_sas_token()
    token_slug <- sas_cred[["token"]]
    path <- file.path(wscu, blob_file, "?")
    path <- shQuote(paste0(path, token_slug))
    args <- c("rm", path)
    .az_do("azcopy", args = args)
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

.az_copy <- function(from, to) {
    args <- c("copy", from, to)
    .az_do("azcopy", args = args)
}
