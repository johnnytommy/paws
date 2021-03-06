% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lightsail_operations.R
\name{lightsail_get_instance_snapshots}
\alias{lightsail_get_instance_snapshots}
\title{Returns all instance snapshots for the user's account}
\usage{
lightsail_get_instance_snapshots(pageToken)
}
\arguments{
\item{pageToken}{A token used for advancing to the next page of results from your get
instance snapshots request.}
}
\description{
Returns all instance snapshots for the user\'s account.
}
\section{Request syntax}{
\preformatted{svc$get_instance_snapshots(
  pageToken = "string"
)
}
}

\keyword{internal}
