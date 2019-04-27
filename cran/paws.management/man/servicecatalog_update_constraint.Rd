% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/servicecatalog_operations.R
\name{servicecatalog_update_constraint}
\alias{servicecatalog_update_constraint}
\title{Updates the specified constraint}
\usage{
servicecatalog_update_constraint(AcceptLanguage, Id, Description)
}
\arguments{
\item{AcceptLanguage}{The language code.
\itemize{
\item \code{en} - English (default)
\item \code{jp} - Japanese
\item \code{zh} - Chinese
}}

\item{Id}{[required] The identifier of the constraint.}

\item{Description}{The updated description of the constraint.}
}
\description{
Updates the specified constraint.
}
\section{Request syntax}{
\preformatted{svc$update_constraint(
  AcceptLanguage = "string",
  Id = "string",
  Description = "string"
)
}
}

\keyword{internal}