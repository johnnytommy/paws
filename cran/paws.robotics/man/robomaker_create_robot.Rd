% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/robomaker_operations.R
\name{robomaker_create_robot}
\alias{robomaker_create_robot}
\title{Creates a robot}
\usage{
robomaker_create_robot(name, architecture, greengrassGroupId, tags)
}
\arguments{
\item{name}{[required] The name for the robot.}

\item{architecture}{[required] The target architecture of the robot.}

\item{greengrassGroupId}{[required] The Greengrass group id.}

\item{tags}{A map that contains tag keys and tag values that are attached to the
robot.}
}
\description{
Creates a robot.
}
\section{Request syntax}{
\preformatted{svc$create_robot(
  name = "string",
  architecture = "X86_64"|"ARM64"|"ARMHF",
  greengrassGroupId = "string",
  tags = list(
    "string"
  )
)
}
}

\keyword{internal}
