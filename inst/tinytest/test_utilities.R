sas_token <- paste0(
    "?sv=2020-08-04&ss=b&srt=sco&sp=rwdlacx&se=2026-12-31T23:59:59Z&",
    "sig=SAS_SIGNATURE"
)

fake_url <-
    paste0("https://mystorage.blob.core.windows.net/container/file", sas_token)

mock_system2 <- function(command, args, stdout, stderr, wait)
    stop("Simulated system2 error: invalid flag used")

orig_fun <- AnVILAz:::.az_system2
unlockBinding(".az_system2", asNamespace("AnVILAz"))
assign(".az_system2", mock_system2, envir = asNamespace("AnVILAz"))

err <- tryCatch(
    {
        AnVILAz:::.az_do("ls", c("--non-existent-flag", fake_url))
    }, error = function(e) conditionMessage(e)
)

expect_true(
    grepl("https://mystorage.blob.core.windows.net/container/file", err),
    info = "Error message should still contain the target URL path for context"
)

expect_false(
    grepl("SAS_SIGNATURE", err),
    info = "Signature was found in the error message!"
)

expect_true(
    grepl("\\?\\[\\.\\.\\.\\]", err),
    info = "The query parameters should be replaced by '[...]'"
)

assign(".az_system2", orig_fun, envir = asNamespace("AnVILAz"))
lockBinding(".az_system2", asNamespace("AnVILAz"))
