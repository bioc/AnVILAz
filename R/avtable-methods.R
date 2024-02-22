#' @name avtable-methods
#'
#' @title AnVIL Azure table ("type") methods
#'
#' @description Methods for working with AnVIL Azure tables. These are referred
#'   to as "types" in the AnVIL Workspace Data Service (WDS) API.
#'
#' @details `avtable_import_set()` creates new rows in a table `<origin>_set`.
#'   One row will be created for each distinct value in the column identified by
#'   `set`. Each row entry has a corresponding column `<origin>` linking to one
#'   or more rows in the `<origin>` table, as given in the `member` column. The
#'   operation is somewhat like `split(member, set)`.
#'
#' @include azure-class.R
#'
#' @return `avtable_import_set()` returns a `character(1)` name of the imported
#'   tibble.
#'
#' @examples
#' \dontrun{
#' avtable("testData") |>                          # new 'testData_set' table
#'     avtable_import_set("testData", "cyl", "model_id")
#' }
NULL


# avtable -----------------------------------------------------------------

#' @describeIn avtable-methods List the contents of a particular table / type
#'
#' @param table `character(1)` The name of the table / type
#'
#' @importFrom AnVILBase avtable
#' @importFrom BiocBaseUtils isScalarCharacter
#' @exportMethod avtable
setMethod("avtable", signature = c(platform = "azure"), definition =
    function(table, ..., platform = cloud_platform()) {
        stopifnot(isScalarCharacter(table))
        download_tsv(
            type = table,
            api_version = .WDS_API_VERSION
        )
    }
)

# avtables ----------------------------------------------------------------

#' @describeIn avtable-methods List the available tables / types
#'
#' @importFrom AnVILBase avtables
#' @importFrom rjsoncons jmespath
#' @importFrom jsonlite fromJSON
#' @exportMethod avtables
setMethod("avtables", signature = c(platform = "azure"), definition =
    function(
        api_version = .WDS_API_VERSION, ..., platform = cloud_platform()
    ) {
        instanceid <- workspace_id()
        v <- version
        api_endpoint <- "/{{instanceid}}/types/{{v}}"
        endpoint <- whisker.render(api_endpoint)

        base_uri <- workspace_data_service_url()
        uri <- paste0(base_uri, endpoint)
        response <- GET(
            url = uri,
            add_headers(authorization = az_token()),
            accept_json()
        )
        avstop_for_status(response, "avtables")
        resp <- content(response, as = "text", encoding = "UTF-8")
        tibble::tibble(
            table = jmespath(resp, "[*].name", as = "R"),
            count = jmespath(resp, "[*].count", as = "R"),
            colnames = vapply(
                jmespath(resp, "[*].attributes[*].name", as = "R"),
                function(cnames) paste(cnames, collapse = ", "),
                character(1L)
            )
        )
    }
)

# avtable_import_set ------------------------------------------------------

#' @describeIn avtable-methods Create a grouping table from an origin dataset
#'
#' @param .data `tibble()` The dataset chiefly from the `avtable()` operation
#'
#' @param origin `character(1)` name of the type (table) used to create the set
#'   e.g "sample", "participant", etc.
#'
#' @param set `character(1)` column name of `.data` identifying the set(s) to be
#'   created.
#'
#' @param member `character()` vector of entity from the `avtable` identified by
#'   `origin`. The values may repeat if an ID is in more than one set
#'
#' @importFrom AnVILBase avtable_import_set
#' @importFrom BiocBaseUtils isScalarCharacter
#' @importFrom dplyr select
#' @exportMethod avtable_import_set
setMethod("avtable_import_set", signature = c(platform = "azure"),
    definition = function(
        .data, origin, set = names(.data)[[1]], member = names(.data)[[2]],
        ..., platform = cloud_platform()
    ) {
        stopifnot(
            is.data.frame(.data),
            isScalarCharacter(origin),
            isScalarCharacter(set),
            isScalarCharacter(member),
            set %in% names(.data),
            !identical(set, member), member %in% names(.data)
        )
        origin <- URLencode(origin)
        .data <- .data |> select(set, member)
        .data <- rev(stack(
            lapply(
                split(.data, .data[[1L]]),
                function(x) paste(x[[2L]], collapse = ", ")
            )
        ))
        names(.data)[[1L]] <- paste0(origin, "_set_id")
        names(.data)[[2L]] <- origin
        fl <- tempfile()
        readr::write_tsv(.data, fl)
        table_type <- paste0(origin, "_set")
        upload_tsv(
            tsv_file = fl,
            type = table_type,
            primaryKey = names(.data)[[1L]]
        )
        table_type
    }
)

# avtable_delete_values ---------------------------------------------------

#' @describeIn avtable-methods Delete rows from a table / type
#'
#' @param values `character()` vector of `primaryKey` values corresponding to
#'   rows to be deleted
#'
#' @importFrom AnVILBase avtable_delete_values
#' @exportMethod avtable_delete_values
setMethod("avtable_delete_values", signature = c(platform = "azure"),
    definition = function(table, values, ..., platform = cloud_platform()) {
        stopifnot(
            isCharacter(values), isScalarCharacter(table)
        )

        tsv <- download_tsv(type = table)
        allids <- tsv[[1L]]
        stopifnot(all(values %in% allids))

        names(values) <- values
        vapply(values, function(val) {
            delete_tsv_row(
                type = table,
                id = val
            )
        }, logical(1L))
    }
)
