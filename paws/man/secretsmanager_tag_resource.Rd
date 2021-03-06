% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/secretsmanager_operations.R
\name{secretsmanager_tag_resource}
\alias{secretsmanager_tag_resource}
\title{Attaches one or more tags, each consisting of a key name and a value, to
the specified secret}
\usage{
secretsmanager_tag_resource(SecretId, Tags)
}
\arguments{
\item{SecretId}{[required] The identifier for the secret that you want to attach tags to. You can
specify either the Amazon Resource Name (ARN) or the friendly name of
the secret.

If you specify an ARN, we generally recommend that you specify a
complete ARN. You can specify a partial ARN too---for example, if you
don't include the final hyphen and six random characters that Secrets
Manager adds at the end of the ARN when you created the secret. A
partial ARN match can work as long as it uniquely matches only one
secret. However, if your secret has a name that ends in a hyphen
followed by six characters (before Secrets Manager adds the hyphen and
six characters to the ARN) and you try to use that as a partial ARN,
then those characters cause Secrets Manager to assume that you're
specifying a complete ARN. This confusion can cause unexpected results.
To avoid this situation, we recommend that you don't create secret names
that end with a hyphen followed by six characters.}

\item{Tags}{[required] The tags to attach to the secret. Each element in the list consists of a
\code{Key} and a \code{Value}.

This parameter to the API requires a JSON text string argument. For
information on how to format a JSON parameter for the various command
line tool environments, see \href{https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json}{Using JSON for Parameters}
in the \emph{AWS CLI User Guide}. For the AWS CLI, you can also use the
syntax:
\verb{--Tags Key="Key1",Value="Value1",Key="Key2",Value="Value2"\\\[,â€¦\\\]}}
}
\description{
Attaches one or more tags, each consisting of a key name and a value, to
the specified secret. Tags are part of the secret\'s overall metadata,
and are not associated with any specific version of the secret. This
operation only appends tags to the existing list of tags. To remove
tags, you must use UntagResource.
}
\details{
The following basic restrictions apply to tags:
\itemize{
\item Maximum number of tags per secret---50
\item Maximum key length---127 Unicode characters in UTF-8
\item Maximum value length---255 Unicode characters in UTF-8
\item Tag keys and values are case sensitive.
\item Do not use the \verb{aws:} prefix in your tag names or values because it
is reserved for AWS use. You can\'t edit or delete tag names or
values with this prefix. Tags with this prefix do not count against
your tags per secret limit.
\item If your tagging schema will be used across multiple services and
resources, remember that other services might have restrictions on
allowed characters. Generally allowed characters are: letters,
spaces, and numbers representable in UTF-8, plus the following
special characters: + - = . \\_ : / @.
}

If you use tags as part of your security strategy, then adding or
removing a tag can change permissions. If successfully completing this
operation would result in you losing your permissions for this secret,
then the operation is blocked and returns an Access Denied error.

\strong{Minimum permissions}

To run this command, you must have the following permissions:
\itemize{
\item secretsmanager:TagResource
}

\strong{Related operations}
\itemize{
\item To remove one or more tags from the collection attached to a secret,
use UntagResource.
\item To view the list of tags attached to a secret, use DescribeSecret.
}
}
\section{Request syntax}{
\preformatted{svc$tag_resource(
  SecretId = "string",
  Tags = list(
    list(
      Key = "string",
      Value = "string"
    )
  )
)
}
}

\examples{
\dontrun{
# The following example shows how to attach two tags each with a Key and
# Value to a secret. There is no output from this API. To see the result,
# use the DescribeSecret operation.
svc$tag_resource(
  SecretId = "MyExampleSecret",
  Tags = list(
    list(
      Key = "FirstTag",
      Value = "SomeValue"
    ),
    list(
      Key = "SecondTag",
      Value = "AnotherValue"
    )
  )
)
}

}
\keyword{internal}
