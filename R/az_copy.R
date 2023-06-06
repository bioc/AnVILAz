#' @export
az_copy <- function(from, to) {
    sas_cred <- .get_sas_token()
    tkn <- sas_cred[["token"]]
    path <- shQuote(sas_cred[["url"]])
    if (!missing(to)) {
        path <- file.path(.workspace_storage_cont_url(), to, "?")
        path <- shQuote(paste0(path, tkn))
    }
    cmd <- "azcopy"
    args <- c("copy", from, path)
    system2(command = cmd, args = args, stdout = TRUE)
}
