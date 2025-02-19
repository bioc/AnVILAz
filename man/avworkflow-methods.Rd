% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/avworkflow-methods.R
\name{avworkflow-methods}
\alias{avworkflow-methods}
\alias{avworkflow_jobs}
\alias{avworkflow_jobs,azure-method}
\alias{avworkflow_jobs_inputs}
\title{Azure Workflow methods}
\usage{
\S4method{avworkflow_jobs}{azure}(..., platform = cloud_platform())

avworkflow_jobs_inputs()
}
\arguments{
\item{...}{Additional arguments passed to lower level functions (not used)}

\item{platform}{\code{azure()} The cloud platform class to dispatch on as given by
\link[AnVILBase:cloud_platform]{AnVILBase::cloud_platform}. Typically not set manually as
\code{cloud_platform()} returns the \code{"azure"} class for Azure workspaces on
AnVIL.}
}
\value{
\code{avworkflow_jobs()} returns a tibble with the status of the
jobs in the current workspace.
}
\description{
\code{avworkflow_jobs()} reports the status of workflow executions
in the current workspace.
}
\details{
The \code{avworkflow_jobs_inputs()} function returns the input parameters
for the workflow jobs as a \code{tibble}.
}
\section{Functions}{
\itemize{
\item \code{avworkflow_jobs(azure)}: List the status of workflow jobs

}}
\examples{
if (has_avworkspace(strict = TRUE, platform = azure()))
    ## from within AnVIL
    avworkflow_jobs()

}
