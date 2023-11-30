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
    expect_false(
        any(
            grepl("test\\.log$", az_copy_list()[["INFO"]])
        )
    )
    az_copy_rm("analyses/test")
    expect_false(
        any(
            grepl("test", az_copy_list()[["INFO"]])
        )
    )

    file.remove("test.log")
})

test_that("backing up and restoring works", {
    .skip_if_not_anvilaz()
    dir.create("test")
    file.create("test/test.log")

    ## create remote folder and copy file
    az_copy_backup("./test", "analyses/test_backup", contentsOnly = TRUE)
    expect_true(
        any(
            grepl("test_backup\\/test\\.log$", az_copy_list()[["INFO"]])
        )
    )

    az_copy_rm("analyses/test_backup", recursive = TRUE)
    expect_false(
        any(
            grepl("test\\.log$", az_copy_list()[["INFO"]])
        )
    )

    az_copy_backup("./test", "analyses/test_backup")
    expect_true(
        any(
            grepl("test_backup\\/test\\/test\\.log$", az_copy_list()[["INFO"]])
        )
    )
    az_copy_rm("analyses/test_backup", recursive = TRUE)

    az_copy_backup("./test", "analyses/test_restore", contentsOnly = TRUE)
    az_copy_restore(
        "analyses/test_restore", "./test_restore", contentsOnly = TRUE
    )
    expect_true(
        dir.exists("test_restore/")
    )
    expect_true(
        file.exists("test_restore/test.log")
    )

    unlink("./test_restore", recursive = TRUE)

    az_copy_restore("analyses/test_restore", "./test_restore")
    expect_true(
        dir.exists("test_restore/test_restore")
    )
    expect_true(
        file.exists("test_restore/test_restore/test.log")
    )

    unlink("./test_restore", recursive = TRUE)
    az_copy_rm("analyses/test_restore", recursive = TRUE)
})
