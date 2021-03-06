% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/route53_operations.R
\name{route53_delete_traffic_policy}
\alias{route53_delete_traffic_policy}
\title{Deletes a traffic policy}
\usage{
route53_delete_traffic_policy(Id, Version)
}
\arguments{
\item{Id}{[required] The ID of the traffic policy that you want to delete.}

\item{Version}{[required] The version number of the traffic policy that you want to delete.}
}
\description{
Deletes a traffic policy.
}
\section{Request syntax}{
\preformatted{svc$delete_traffic_policy(
  Id = "string",
  Version = 123
)
}
}

\keyword{internal}
