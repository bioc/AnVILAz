# ensure .azcopyStatus works ----------------------------------------------

oneres <- c(
    "INFO: Scanning...", paste0("DRYRUN: copy /home/jupyter/jupyter.log to",
    "https://lza90fd4fb79c636349cdaad.blob.core.windows.net/",
    "sc-995e7d70-320f-42d9-b731-dfb4bfb84708/analyses/jupyter.log")
)

x <- AnVILAz:::.azcopyStatus(oneres)
expect_true(is.list(x))
expect_true(inherits(x, "azcopyStatus"))
expect_identical(length(x), 1L)
expect_identical(
    capture.output(print(x)),
    c("LogFile: NA", "  100.0 %, 1 DRYRUN")
)
expect_identical(
    unlist(x, recursive = FALSE),
    list(
        INFO = oneres[1],
        DRYRUN = oneres[2],
        statusLine = "100.0 %, 1 DRYRUN",
        Status = FALSE,
        summary = NA_character_,
        logFile = NA_character_,
        jobId = NA_character_
    )
)

fullres <- c(
    "INFO: Scanning...",
    paste0(
        "INFO: Any empty folders will not be processed, because source ",
        "and/or destination doesn't have full folder support"
    ),
    "",
    "Job c48205ab-73cf-de44-4669-0eeca2ddf9f9 has started",
    paste0(
        "Log file is located at: /home/jupyter/.azcopy/",
        "c48205ab-73cf-de44-4669-0eeca2ddf9f9.log"
    ),
    "",
    "INFO: azcopy 10.18.1: A newer version 10.25.1 is available to download",
    "",
    paste0(
        "\r100.0 %, 1 Done, 0 Failed, 0 Pending, 0 Skipped, ",
        "1 Total, 2-sec Throughput (Mb/s): 0.0452"
    ),
    "",
    "",
    "Job c48205ab-73cf-de44-4669-0eeca2ddf9f9 summary",
    "Elapsed Time (Minutes): 0.0347",
    "Number of File Transfers: 1",
    "Number of Folder Property Transfers: 0",
    "Number of Symlink Transfers: 0",
    "Total Number of Transfers: 1",
    "Number of File Transfers Completed: 1",
    "Number of Folder Transfers Completed: 0",
    "Number of File Transfers Failed: 0",
    "Number of Folder Transfers Failed: 0",
    "Number of File Transfers Skipped: 0",
    "Number of Folder Transfers Skipped: 0",
    "TotalBytesTransferred: 11762",
    "Final Job Status: Completed",
    ""
)

x <- AnVILAz:::.azcopyStatus(fullres)

expect_true(is.list(x))
expect_true(inherits(x, "azcopyStatus"))
expect_identical(length(x), 1L)
out <- capture.output(print(x))
expect_identical(
   out,
   c(
       "LogFile: /home/jupyter/.azcopy/c48205ab-73cf-de44-4669-0eeca2ddf9f9.log",
       paste0(
            "  100.0 %, 1 Done, 0 Failed, 0 Pending, 0 Skipped, 1 Total, ",
            "2-sec Throughput (Mb/s): 0.0452"
       )
   )
)
expect_identical(
    unlist(x, recursive = FALSE),
    list(
        INFO = grep("INFO", fullres, value = TRUE),
        DRYRUN = NA_character_,
        statusLine = paste0(
            "100.0 %, 1 Done, 0 Failed, 0 Pending, 0 Skipped, 1 Total, ",
            "2-sec Throughput (Mb/s): 0.0452"
        ),
        Status = TRUE,
        summary = c(
            "  Elapsed Time (Minutes): 0.0347",
            "  Number of File Transfers: 1",
            "  Number of Folder Property Transfers: 0",
            "  Number of Symlink Transfers: 0",
            "  Total Number of Transfers: 1",
            "  Number of File Transfers Completed: 1",
            "  Number of Folder Transfers Completed: 0",
            "  Number of File Transfers Failed: 0",
            "  Number of Folder Transfers Failed: 0",
            "  Number of File Transfers Skipped: 0",
            "  Number of Folder Transfers Skipped: 0",
            "  TotalBytesTransferred: 11762",
            "  Final Job Status: Completed"
        ),
        logFile =
            "/home/jupyter/.azcopy/c48205ab-73cf-de44-4669-0eeca2ddf9f9.log",
        jobId = "c48205ab-73cf-de44-4669-0eeca2ddf9f9"
    )
)
