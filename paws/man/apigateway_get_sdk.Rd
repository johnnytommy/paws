% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apigateway_operations.R
\name{apigateway_get_sdk}
\alias{apigateway_get_sdk}
\title{Generates a client SDK for a RestApi and Stage}
\usage{
apigateway_get_sdk(restApiId, stageName, sdkType, parameters)
}
\arguments{
\item{restApiId}{[required] [Required] The string identifier of the associated RestApi.}

\item{stageName}{[required] [Required] The name of the Stage that the SDK will use.}

\item{sdkType}{[required] [Required] The language for the generated SDK. Currently \code{java},
\code{javascript}, \code{android}, \code{objectivec} (for iOS), \code{swift} (for iOS), and
\code{ruby} are supported.}

\item{parameters}{A string-to-string key-value map of query parameters \code{sdkType}-dependent
properties of the SDK. For \code{sdkType} of \code{objectivec} or \code{swift}, a
parameter named \code{classPrefix} is required. For \code{sdkType} of \code{android},
parameters named \code{groupId}, \code{artifactId}, \code{artifactVersion}, and
\code{invokerPackage} are required. For \code{sdkType} of \code{java}, parameters named
\code{serviceName} and \code{javaPackageName} are required.}
}
\description{
Generates a client SDK for a RestApi and Stage.
}
\section{Request syntax}{
\preformatted{svc$get_sdk(
  restApiId = "string",
  stageName = "string",
  sdkType = "string",
  parameters = list(
    "string"
  )
)
}
}

\keyword{internal}
