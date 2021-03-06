% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mediaconnect_operations.R
\name{mediaconnect_describe_flow}
\alias{mediaconnect_describe_flow}
\title{Displays the details of a flow}
\usage{
mediaconnect_describe_flow(FlowArn)
}
\arguments{
\item{FlowArn}{[required] The ARN of the flow that you want to describe.}
}
\description{
Displays the details of a flow. The response includes the flow ARN, name, and Availability Zone, as well as details about the source, outputs, and entitlements.
}
\section{Request syntax}{
\preformatted{svc$describe_flow(
  FlowArn = "string"
)
}
}

\keyword{internal}
