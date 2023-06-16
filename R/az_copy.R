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
#' @param from `character(1)` A file path relevant to the Azure workspace,
#'   usually where the Jupyter Notebooks are found.
#'
#' @param to `character(1)` A relative path in the Azure Storage Container
#'
#' @param blob_file `character(1)` A relative path to a file in the Azure
#'   Storage Container to be removed
#'
#' @return
#' * `az_copy_list` - a `tibble` of files and metadata
#' * `az_copy_copy` - called for the side effect of copying a file
#' * `az_copy_rm` - called for the side effect of removing a file
#'
#' @examples
#' if (interactive()) {
#'
#'   az_copy_list()
#'   az_copy_copy("jupyter.log", "analyses/jupyter.log")
#'   az_copy_rm("analyses/jupyter.log")
#'
#' }
#' @export
az_copy_copy <- function(from, to) {
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
    args <- c("copy", from, shQuote(path))
    .az_do("azcopy", args = args)
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
    system2(command = command, args = args, stdout = TRUE)
}

.az_find <- function(command) {
    bin <- Sys.which(command)
    if (nzchar(bin))
        normalizePath(bin)
    else
        stop("failed to find '", command, "' binary", call. = FALSE)
}
