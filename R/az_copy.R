#' @export
az_copy <- function(from, to) {
    sas_cred <- .get_sas_token()
    tkn <- sas_cred[["token"]]
    path <- shQuote(sas_cred[["url"]])
    if (!missing(to)) {
        path <- file.path(workspace_storage_cont_url(), to, "?")
        path <- shQuote(paste0(path, tkn))
    }
    args <- c("copy", from, path)
    .az_do("azcopy", args = args)
}

#' @export
az_copy_list <- function() {
    path <- get_sas_token()[["url"]]
    args <- c("list", shQuote(path))
    output <- .az_do("azcopy", args = args)
    files <- strsplit(output, "; ")
    files <- lapply(files,
        function(x) gsub("INFO:\\s+|Content\\sLength:\\s+", "", x)
    )
    res <- do.call(rbind.data.frame, files)
    names(res) <- c("INFO", "Content.Length")
    tibble::as_tibble(res)
}

#' @export
az_copy_rm <- function(blob_file) {
    file_tbl <- az_copy_list()
    allfiles <- file_tbl[["INFO"]]
    if (!blob_file %in% allfiles)
        stop("File not found; check path to blob file with `az_copy_list`")

    sas_cred <- .get_sas_token()
    token_slug <- sas_cred[["token"]]
    path <- file.path(workspace_storage_cont_url(), blob_file, "?")
    path <- shQuote(paste0(path, token_slug))
    args <- c("rm", path)
    .az_do("azcopy", args = args)
}

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
