AnVILAz:::.exit_if_not_anvilaz()

if (!AnVILAz:::.is_anvil_az())
    tinytest::exit_file("Not running on AnVIL Azure workspace")

tinytest::exit_if_not(
    "Not running on AnVIL Azure workspace" = AnVILAz:::.is_anvil_az()
)

## https://github.com/markvanderloo/tinytest/issues/126
## fallback to logical condition for now

if (AnVILAz:::.is_anvil_az()) {

library(BiocBaseUtils)
# avworkspaces ------------------------------------------------------------

aws <- avworkspaces()

expect_true(
    is_tibble(aws)
)

expect_true(
    all(
        c("name", "namespace", "createdBy") %in% colnames(aws)
    )
)

awsns <- avworkspace_namespace()

expect_true(
    isScalarCharacter(awsns)
)

awsn <- avworkspace_name()

expect_true(
    isScalarCharacter(awsn)
)

expect_identical(
    avworkspace(), paste0(awsns, "/", awsn)
)

}
