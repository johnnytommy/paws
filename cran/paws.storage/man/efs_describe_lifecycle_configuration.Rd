% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/efs_operations.R
\name{efs_describe_lifecycle_configuration}
\alias{efs_describe_lifecycle_configuration}
\title{Returns the current LifecycleConfiguration object for the specified
Amazon EFS file system}
\usage{
efs_describe_lifecycle_configuration(FileSystemId)
}
\arguments{
\item{FileSystemId}{[required] The ID of the file system whose \code{LifecycleConfiguration} object you want
to retrieve (String).}
}
\description{
Returns the current \code{LifecycleConfiguration} object for the specified
Amazon EFS file system. EFS lifecycle management uses the
\code{LifecycleConfiguration} object to identify which files to move to the
EFS Infrequent Access (IA) storage class. For a file system without a
\code{LifecycleConfiguration} object, the call returns an empty array in the
response.
}
\details{
This operation requires permissions for the
\code{elasticfilesystem:DescribeLifecycleConfiguration} operation.
}
\section{Request syntax}{
\preformatted{svc$describe_lifecycle_configuration(
  FileSystemId = "string"
)
}
}

\examples{
\dontrun{
# This operation describes a file system's LifecycleConfiguration. EFS
# lifecycle management uses the LifecycleConfiguration object to identify
# which files to move to the EFS Infrequent Access (IA) storage class.
svc$describe_lifecycle_configuration(
  FileSystemId = "fs-01234567"
)
}

}
\keyword{internal}
