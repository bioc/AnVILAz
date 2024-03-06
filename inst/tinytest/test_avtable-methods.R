AnVILAz:::.exit_if_not_anvilaz()

if (!AnVILAz:::.is_anvil_az())
    tinytest::exit_file("Not running on AnVIL Azure workspace")

tinytest::exit_if_not(
    "Not running on AnVIL Azure workspace" = AnVILAz:::.is_anvil_az()
)

## https://github.com/markvanderloo/tinytest/issues/126
## fallback to logical condition for now

if (AnVILAz:::.is_anvil_az()) {

library(dplyr)
library(tibble)

type <- "model"
primaryKey <- "model_id"
mtcars_tbl <-
    mtcars |>
    as_tibble(rownames = primaryKey) |>
    mutate(model_id = gsub(" ", "-", model_id))

tsv_file <- tempfile()
readr::write_tsv(mtcars_tbl, tsv_file)

# avtable_import and upload_tsv -------------------------------------------

avtable_import(tsv_file, "testData", primaryKey)

# avtables ----------------------------------------------------------------

avtab <- avtables()
expect_true(
    is_tibble(avtab)
)

expect_identical(
    avtab[["table"]], "testData"
)

expect_identical(
    avtab[["count"]], nrow(mtcars_tbl)
)

expect_identical(
    setequal(avtab[["columns"]], colnames(mtcars_tbl))
)

expect_true(
    setequal(
        strsplit(avtab[["colnames"]], ", ")[[1L]],
        colnames(mtcars_tbl)
    )
)


# avtable and download_tsv ------------------------------------------------

avtab <- avtable("testData")
expect_true(
    is_tibble(avtab)
)
expect_identical(
    colnames(avtab)[1L], primaryKey
)
expect_true(
    setequal(
        colnames(avtab), colnames(mtcars_tbl)
    )
)
expect_identical(
    sort(avtab[[primaryKey]]),
    sort(mtcars_tbl[[primaryKey]])
)
expect_identical(
    dim(avtab),
    dim(mtcars_tbl)
)

# avtable_import_set ------------------------------------------------------

avtable("testData") |>
    avtable_import_set("testData", "cyl", "model_id")

tset <- avtable("testData_set")

expect_identical(
    colnames(tset),
    c("testData_set_id", "testData")
)

expect_identical(
    tset[["testData_set_id"]],
    unique(sort(mtcars_tbl[["cyl"]]))
)

Map(
    function(orig, set) {
        expect_true(
            setequal(orig, set)
        )
    },
    orig = lapply(split(mtcars_tbl, mtcars_tbl[["cyl"]]), `[[`, "model_id"),
    set = unlist(lapply(tset[["testData"]], strsplit, ", "), recursive = FALSE)
)

delete_type("testData")

# avtable_delete_values ---------------------------------------------------

avtable_delete_values("testData_set", "6")

tset <- avtable("testData_set")

expect_identical(
    dim(tset), c(2L, 2L)
)

expect_identical(
    tset[["testData_set_id"]], c(4, 8)
)

delete_type("testData_set")

}
