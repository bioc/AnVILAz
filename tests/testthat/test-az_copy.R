test_that("file listing works", {
    .skip_if_not_anvilaz()
    files <- az_copy_list()
    expect_true(
        tibble::is_tibble(files) && is.data.frame(files)
    )
    expect_identical(
        names(files), c("INFO", "Content.Length")
    )
})

test_that("copying files works", {
    .skip_if_not_anvilaz()
    file.create("test.log")
    az_copy_to_storage("test.log", "analyses/test/test.log")
    expect_true(
        any(
            grepl("test\\.log$", az_copy_list()[["INFO"]])
        )
    )
    file.remove("test.log")
    az_copy_from_storage("analyses/test/test.log", "./test.log")
    expect_true(
        file.exists("test.log")
    )

    az_copy_rm("analyses/test/test.log")
    file.remove("test.log")
})
