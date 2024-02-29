AnVILAz:::.exit_if_not_anvilaz()

if (!AnVILAz:::.is_anvil_az())
    tinytest::exit_file("Not running on AnVIL Azure workspace")

tinytest::exit_if_not(
    "Not running on AnVIL Azure workspace" = AnVILAz:::.is_anvil_az()
)

## https://github.com/markvanderloo/tinytest/issues/126
## fallback to logical condition for now

if (AnVILAz:::.is_anvil_az()) {

# test avlist -------------------------------------------------------

files <- avlist()
expect_true(
    tibble::is_tibble(files) && is.data.frame(files)
)
expect_identical(
    names(files), c("INFO", "Content.Length")
)

# test az_copy_to and from_storage ----------------------------------------

file.create("test.log")
az_copy_to_storage("test.log", "analyses/test/test.log")
expect_true(
    "analyses/test/test.log" %in% avlist()[["INFO"]]
)
file.remove("test.log")
expect_error(
    az_copy_from_storage("analyses/test/test.log", "./test.log")
)
az_copy_from_storage("analyses/test/test.log", "./")
expect_true(
    file.exists("test.log")
)

# test avremove ---------------------------------------------------------

avremove("analyses/test/test.log")
expect_false(
    "analyses/test/test.log" %in% avlist()[["INFO"]]
)
expect_false(
    "analyses/test" %in% avlist()[["INFO"]]
)

file.remove("test.log")

# test avbackup and restore -----------------------------------------

dir.create("test")
file.create("test/test.log")
file.create("test/test2.log")

## create remote folder and copy file
avbackup("./test", "analyses/test_backup/", contentsOnly = TRUE)
expect_true(
    "analyses/test_backup/test.log" %in% avlist()[["INFO"]]
)
expect_true(
    "analyses/test_backup/test2.log" %in% avlist()[["INFO"]]
)

avremove("analyses/test_backup/", recursive = TRUE)
expect_false(
    "analyses/test_backup/test.log" %in% avlist()[["INFO"]]
)
expect_false(
    "analyses/test_backup/test2.log" %in% avlist()[["INFO"]]
)

avbackup("./test", "analyses/test_backup")
expect_true(
    "analyses/test_backup/test/test.log" %in% avlist()[["INFO"]]
)
avremove("analyses/test_backup/", recursive = TRUE)

avbackup("./test", "analyses/test_restore")
avrestore(
    "analyses/test_restore", "./test_restore"
)
expect_true(
    dir.exists("test_restore/")
)
expect_true(
    file.exists("test_restore/test.log")
)

unlink("./test", recursive = TRUE)
unlink("./test_restore", recursive = TRUE)

avrestore("analyses/test_restore", "./test_restore")
expect_true(
    dir.exists("test_restore/test_restore")
)
expect_true(
    file.exists("test_restore/test_restore/test.log")
)

unlink("./test_restore", recursive = TRUE)
avremove("analyses/test_restore/", recursive = TRUE)

}
