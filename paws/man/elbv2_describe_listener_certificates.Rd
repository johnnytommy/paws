% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/elbv2_operations.R
\name{elbv2_describe_listener_certificates}
\alias{elbv2_describe_listener_certificates}
\title{Describes the default certificate and the certificate list for the
specified HTTPS or TLS listener}
\usage{
elbv2_describe_listener_certificates(ListenerArn, Marker, PageSize)
}
\arguments{
\item{ListenerArn}{[required] The Amazon Resource Names (ARN) of the listener.}

\item{Marker}{The marker for the next set of results. (You received this marker from a
previous call.)}

\item{PageSize}{The maximum number of results to return with this call.}
}
\description{
Describes the default certificate and the certificate list for the
specified HTTPS or TLS listener.
}
\details{
If the default certificate is also in the certificate list, it appears
twice in the results (once with \code{IsDefault} set to true and once with
\code{IsDefault} set to false).

For more information, see \href{https://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html#https-listener-certificates}{SSL Certificates}
in the \emph{Application Load Balancers Guide}.
}
\section{Request syntax}{
\preformatted{svc$describe_listener_certificates(
  ListenerArn = "string",
  Marker = "string",
  PageSize = 123
)
}
}

\keyword{internal}
