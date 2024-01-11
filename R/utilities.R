# AnVIL/R/av.R
#' @importFrom httr status_code http_condition headers
.stop_for_status <-
    function(response, op)
{
    status <- status_code(response)
    if (status < 400L)
        return(invisible(response))

    cond <- http_condition(status, "error")
    type <- headers(response)[["content-type"]]
    msg <- NULL
    if (nzchar(type) && grepl("application/json", type)) {
        content <- as.list(response)
        msg <- content[["message"]]
        if (is.null(msg))
            ## e.g., from bond DRS server
            msg <- content$response$text
    } else if (nzchar(type) && grepl("text/html", type)) {
        ## these pages can be too long for a standard 'stop()' message
        cat(as.character(response), file = stderr())
    }

    message <- paste0(
        "'", op, "' failed:\n  ",
        conditionMessage(cond),
        if (!is.null(msg)) "\n  ", msg
    )
    stop(message, call.=FALSE)
}

## from AnVILGCP
.az_shQuote <- function(source) {
    ## Expand local paths with ~ or . or .. to full path names.
    ## Needed because we also use shQuote() (to allow for spaces in
    ## file names), and shQuote() would otherwise use paths with ~ or
    ## . in the current working directory.
    source <- normalizePath(source)
    shQuote(source)
}

.is_remote_path <- function(path) {
    startsWith(path, "http") || !file.exists(path)
}

.validate_blob <- function(blob) {
    file_tbl <- avlist()
    allfiles <- file_tbl[["INFO"]]
    is_dir <- endsWith(blob, "/")
    if (is_dir && !any(startsWith(allfiles, blob)))
        stop("Virtual directory not found; check path with `avlist`")

    if (!blob %in% allfiles && !is_dir)
        stop("File not found; check path to blob file with `avlist`")

    TRUE
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

## tinytest helpers

.is_anvil_az <- function() {
    nzchar(Sys.getenv("WORKSPACE_ID"))
}

.is_anvil_gcp <- function() {
    nzchar(Sys.getenv("WORKSPACE_BUCKET"))
}

.exit_if_not_anvilaz <- function() {
    if (!.is_anvil_az())
        tinytest::exit_file("Not running on AnVIL Azure workspace")
}
