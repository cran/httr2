% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test.R
\name{example_url}
\alias{example_url}
\alias{example_github_client}
\title{Code for examples}
\usage{
example_url(path = "/")

example_github_client()
}
\description{
\code{example_url()} runs a simple websever using the webfakes package with the
following endpoints:
\itemize{
\item all the ones from the \code{\link[webfakes:httpbin_app]{webfakes::httpbin_app()}}
\item \verb{/iris}: paginate through the iris dataset. It has the query parameters
\code{page} and \code{limit} to control the pagination.
}

\code{example_github_client()} is an OAuth client for GitHub.
}
\keyword{internal}
