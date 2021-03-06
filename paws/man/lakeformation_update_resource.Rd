% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lakeformation_operations.R
\name{lakeformation_update_resource}
\alias{lakeformation_update_resource}
\title{Updates the data access role used for vending access to the given
(registered) resource in AWS Lake Formation}
\usage{
lakeformation_update_resource(RoleArn, ResourceArn)
}
\arguments{
\item{RoleArn}{[required] The new role to use for the given resource registered in AWS Lake
Formation.}

\item{ResourceArn}{[required] The resource ARN.}
}
\description{
Updates the data access role used for vending access to the given
(registered) resource in AWS Lake Formation.
}
\section{Request syntax}{
\preformatted{svc$update_resource(
  RoleArn = "string",
  ResourceArn = "string"
)
}
}

\keyword{internal}
