% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/serverlessapplicationrepository_operations.R
\name{serverlessapplicationrepository_put_application_policy}
\alias{serverlessapplicationrepository_put_application_policy}
\title{Sets the permission policy for an application}
\usage{
serverlessapplicationrepository_put_application_policy(ApplicationId,
  Statements)
}
\arguments{
\item{ApplicationId}{[required] The Amazon Resource Name (ARN) of the application.}

\item{Statements}{[required] An array of policy statements applied to the application.}
}
\description{
Sets the permission policy for an application. For the list of actions
supported for this operation, see \href{https://docs.aws.amazon.com/serverlessrepo/latest/devguide/access-control-resource-based.html#application-permissions}{Application Permissions}
.
}
\section{Request syntax}{
\preformatted{svc$put_application_policy(
  ApplicationId = "string",
  Statements = list(
    list(
      Actions = list(
        "string"
      ),
      Principals = list(
        "string"
      ),
      StatementId = "string"
    )
  )
)
}
}

\keyword{internal}
