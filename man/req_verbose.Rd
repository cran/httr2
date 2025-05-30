% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/req-verbose.R
\name{req_verbose}
\alias{req_verbose}
\title{Show extra output when request is performed}
\usage{
req_verbose(
  req,
  header_req = TRUE,
  header_resp = TRUE,
  body_req = FALSE,
  body_resp = FALSE,
  info = FALSE,
  redact_headers = TRUE
)
}
\arguments{
\item{req}{A httr2 \link{request} object.}

\item{header_req, header_resp}{Show request/response headers?}

\item{body_req, body_resp}{Should request/response bodies? When the response
body is compressed, this will show the number of bytes received in
each "chunk".}

\item{info}{Show informational text from curl? This is mainly useful
for debugging https and auth problems, so is disabled by default.}

\item{redact_headers}{Redact confidential data in the headers? Currently
redacts the contents of the Authorization header to prevent you from
accidentally leaking credentials when debugging/reprexing.}
}
\value{
A modified HTTP \link{request}.
}
\description{
\code{req_verbose()} uses the following prefixes to distinguish between
different components of the HTTP requests and responses:
\itemize{
\item \verb{* } informative curl messages
\item \verb{->} request headers
\item \verb{>>} request body
\item \verb{<-} response headers
\item \verb{<<} response body
}
}
\examples{
# Use `req_verbose()` to see the headers that are sent back and forth when
# making a request
resp <- request("https://httr2.r-lib.org") |>
  req_verbose() |>
  req_perform()

# Or use one of the convenient shortcuts:
resp <- request("https://httr2.r-lib.org") |>
  req_perform(verbosity = 1)
}
\seealso{
\code{\link[=req_perform]{req_perform()}} which exposes a limited subset of these options
through the \code{verbosity} argument and \code{\link[=with_verbosity]{with_verbosity()}} which allows you
to control the verbosity of requests deeper within the call stack.
}
