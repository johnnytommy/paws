% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/glue_operations.R
\name{glue_get_data_catalog_encryption_settings}
\alias{glue_get_data_catalog_encryption_settings}
\title{Retrieves the security configuration for a specified catalog}
\usage{
glue_get_data_catalog_encryption_settings(CatalogId)
}
\arguments{
\item{CatalogId}{The ID of the Data Catalog to retrieve the security configuration for.
If none is provided, the AWS account ID is used by default.}
}
\description{
Retrieves the security configuration for a specified catalog.
}
\section{Request syntax}{
\preformatted{svc$get_data_catalog_encryption_settings(
  CatalogId = "string"
)
}
}

\keyword{internal}
