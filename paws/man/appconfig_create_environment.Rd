% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/appconfig_operations.R
\name{appconfig_create_environment}
\alias{appconfig_create_environment}
\title{For each application, you define one or more environments}
\usage{
appconfig_create_environment(ApplicationId, Name, Description, Monitors,
  Tags)
}
\arguments{
\item{ApplicationId}{[required] The application ID.}

\item{Name}{[required] A name for the environment.}

\item{Description}{A description of the environment.}

\item{Monitors}{Amazon CloudWatch alarms to monitor during the deployment process.}

\item{Tags}{Metadata to assign to the environment. Tags help organize and categorize
your AppConfig resources. Each tag consists of a key and an optional
value, both of which you define.}
}
\description{
For each application, you define one or more environments. An
environment is a logical deployment group of AppConfig targets, such as
applications in a \code{Beta} or \code{Production} environment. You can also
define environments for application subcomponents such as the \code{Web},
\code{Mobile} and \code{Back-end} components for your application. You can
configure Amazon CloudWatch alarms for each environment. The system
monitors alarms during a configuration deployment. If an alarm is
triggered, the system rolls back the configuration.
}
\section{Request syntax}{
\preformatted{svc$create_environment(
  ApplicationId = "string",
  Name = "string",
  Description = "string",
  Monitors = list(
    list(
      AlarmArn = "string",
      AlarmRoleArn = "string"
    )
  ),
  Tags = list(
    "string"
  )
)
}
}

\keyword{internal}
