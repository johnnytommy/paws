% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/efs_operations.R
\name{efs_describe_file_systems}
\alias{efs_describe_file_systems}
\title{Returns the description of a specific Amazon EFS file system if either
the file system CreationToken or the FileSystemId is provided}
\usage{
efs_describe_file_systems(MaxItems, Marker, CreationToken, FileSystemId)
}
\arguments{
\item{MaxItems}{(Optional) Specifies the maximum number of file systems to return in the
response (integer). Currently, this number is automatically set to 10,
and other values are ignored. The response is paginated at 10 per page
if you have more than 10 file systems.}

\item{Marker}{(Optional) Opaque pagination token returned from a previous
\code{DescribeFileSystems} operation (String). If present, specifies to
continue the list from where the returning call had left off.}

\item{CreationToken}{(Optional) Restricts the list to the file system with this creation
token (String). You specify a creation token when you create an Amazon
EFS file system.}

\item{FileSystemId}{(Optional) ID of the file system whose description you want to retrieve
(String).}
}
\description{
Returns the description of a specific Amazon EFS file system if either
the file system \code{CreationToken} or the \code{FileSystemId} is provided.
Otherwise, it returns descriptions of all file systems owned by the
caller\'s AWS account in the AWS Region of the endpoint that you\'re
calling.
}
\details{
When retrieving all file system descriptions, you can optionally specify
the \code{MaxItems} parameter to limit the number of descriptions in a
response. Currently, this number is automatically set to 10. If more
file system descriptions remain, Amazon EFS returns a \code{NextMarker}, an
opaque token, in the response. In this case, you should send a
subsequent request with the \code{Marker} request parameter set to the value
of \code{NextMarker}.

To retrieve a list of your file system descriptions, this operation is
used in an iterative process, where \code{DescribeFileSystems} is called
first without the \code{Marker} and then the operation continues to call it
with the \code{Marker} parameter set to the value of the \code{NextMarker} from
the previous response until the response has no \code{NextMarker}.

The order of file systems returned in the response of one
\code{DescribeFileSystems} call and the order of file systems returned across
the responses of a multi-call iteration is unspecified.

This operation requires permissions for the
\code{elasticfilesystem:DescribeFileSystems} action.
}
\section{Request syntax}{
\preformatted{svc$describe_file_systems(
  MaxItems = 123,
  Marker = "string",
  CreationToken = "string",
  FileSystemId = "string"
)
}
}

\examples{
\dontrun{
# This operation describes all of the EFS file systems in an account.
svc$describe_file_systems()
}

}
\keyword{internal}
