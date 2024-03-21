#' @name az_copy-helpers
#'
#' @title Azure Copy command line utility helpers
#'
#' @description These functions invoke the `azcopy` command line utility. The
#'   utilities make use of a managed SAS token to mainly transfer files from the
#'   Azure workspace to the Azure Storage container. See `av_sas_token` for
#'   credential details. The results of `azcopy copy` commands are returned as
#'   an `azcopyStatus` object which has S3 methods to print and convert to
#'   logical.
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

    results <- .az_copy(
        shQuote(path), shQuote(to), paste0("--recursive=", recurse),
        if (dry) "--dry-run"
    )
    .azcopyStatus(results)
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

    results <- .az_copy(
        .az_shQuote(from), shQuote(path), paste0("--recursive=", recurse),
        if (dry) "--dry-run"
    )
    .azcopyStatus(results)
}

.azcopyStatus <- function(txtlist) {
    res <- lapply(txtlist, .parse_job_status_text)
    class(res) <- c("azcopyStatus", class(res))
    res
}

.clean_summary <- function(txt) {
    jobsumInd <- grep("Job.*summary$", txt) + 1
    summary <- txt[jobsumInd:length(txt)]
    if (!length(summary))
        return(NA_character_)
    else
        strwrap(summary, indent = 2L, exdent = 4L, width = 80)
}

.parse_job_status_text <- function(txt) {
    txt <- sub("\\r", "", txt)
    txt <- Filter(nzchar, txt)
    statusLine <- grep(".*Done.*Failed", txt, value = TRUE)
    status <- grepl("100.0 %", statusLine)
    info <- Filter(function(x) startsWith(x, "INFO"), txt)
    summary <- .clean_summary(txt)
    logLine <- grep("Log file is located at:", txt, value = TRUE)
    logfile <- gsub("Log file is located at: (.*)", "\\1", logLine)
    jobLine <- grep("Job (.*) has started", txt, value = TRUE)
    jobId <- gsub("Job (.*) has started", "\\1", jobLine)
    result <- list(
        INFO = if (length(info)) info else NA_character_,
        statusLine = statusLine,
        Status = status,
        summary = if (length(summary)) summary else NA_character_,
        logFile = if (length(logfile)) logfile else NA_character_,
        jobId = jobId
    )
}

#' @describeIn az_copy-helpers Convert results of azcopy operations to logical
#'   values
#'
#' @param x `azcopyStatus` object to be checked; usually the output of `avcopy`
#'   operations
#'
#' @export
as.logical.azcopyStatus <- function(x) {
    vapply(x, function(x) x$Status, logical(1))
}

#' @describeIn az_copy-helpers Print the results of `azcopy` operations
#'
#' @param ... Additional arguments (not used).
#'
#' @param verbose `logical(1)` Print the `INFO` lines from the `azcopy` output
#'
#' @export
print.azcopyStatus <- function(x, ..., verbose = FALSE) {
    for (i in seq_along(x)) {
        if (verbose)
            cat(
                paste(x[[i]]$INFO, collapse = "\n"), "\n"
            )
       cat(
           "LogFile: ", x[[i]]$logFile, "\n  ",
            x[[i]]$statusLine, "\n",
           sep = ""
        )
    }
}

#' @describeIn az_copy-helpers Get a summary of the results of `azcopy`
#'   operations
#'
#' @param na.rm `logical(1)` Not used.
#'
#' @export
summary.azcopyStatus <- function(x, ..., na.rm = FALSE) {
    for (i in seq_along(x)) {
        cat(
            "Job ID: ", x[[i]]$jobId, "\n",
            paste(x[[i]]$summary, collapse = "\n"), "\n",
            sep = ""
        )
    }
}
