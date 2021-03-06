% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/amplify_operations.R
\name{amplify_get_job}
\alias{amplify_get_job}
\title{Get a job for a branch, part of an Amplify App}
\usage{
amplify_get_job(appId, branchName, jobId)
}
\arguments{
\item{appId}{[required] Unique Id for an Amplify App.}

\item{branchName}{[required] Name for the branch, for the Job.}

\item{jobId}{[required] Unique Id for the Job.}
}
\description{
Get a job for a branch, part of an Amplify App.
}
\section{Request syntax}{
\preformatted{svc$get_job(
  appId = "string",
  branchName = "string",
  jobId = "string"
)
}
}

\keyword{internal}
