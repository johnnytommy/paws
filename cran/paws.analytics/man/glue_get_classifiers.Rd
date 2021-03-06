% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/glue_operations.R
\name{glue_get_classifiers}
\alias{glue_get_classifiers}
\title{Lists all classifier objects in the Data Catalog}
\usage{
glue_get_classifiers(MaxResults, NextToken)
}
\arguments{
\item{MaxResults}{The size of the list to return (optional).}

\item{NextToken}{An optional continuation token.}
}
\description{
Lists all classifier objects in the Data Catalog.
}
\section{Request syntax}{
\preformatted{svc$get_classifiers(
  MaxResults = 123,
  NextToken = "string"
)
}
}

\keyword{internal}
