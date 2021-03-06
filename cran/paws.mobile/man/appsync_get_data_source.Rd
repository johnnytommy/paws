% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/appsync_operations.R
\name{appsync_get_data_source}
\alias{appsync_get_data_source}
\title{Retrieves a DataSource object}
\usage{
appsync_get_data_source(apiId, name)
}
\arguments{
\item{apiId}{[required] The API ID.}

\item{name}{[required] The name of the data source.}
}
\description{
Retrieves a \code{DataSource} object.
}
\section{Request syntax}{
\preformatted{svc$get_data_source(
  apiId = "string",
  name = "string"
)
}
}

\keyword{internal}
