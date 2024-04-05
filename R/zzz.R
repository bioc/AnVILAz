.onLoad <- function(libname, pkgname) {
    evars <- list(
        workspaceId = workspace_id(),
        wdsApiVersion = wds_api_version(),
        workspaceName = .workspace_name(),
        resourceId = workspace_storage_cont_id(),
        wscu = workspace_storage_cont_url()
    )
    for (k in names(evars)) {
        .avcache$set(k, evars[[k]])
    }
}
