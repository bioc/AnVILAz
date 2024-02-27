.onLoad <- function(libname, pkgname) {
    ## use memoise for caching calls
    # if (getOption("AnVILAz.memoise", TRUE)) { ## or env var
    #     avworkspace_namespace <<- memoise::memoise(avworkspace_namespace)
    #     avworkspace_name <<- memoise::memoise(avworkspace_name)
    # }
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
