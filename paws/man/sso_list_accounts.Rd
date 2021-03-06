% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sso_operations.R
\name{sso_list_accounts}
\alias{sso_list_accounts}
\title{Lists all AWS accounts assigned to the user}
\usage{
sso_list_accounts(nextToken, maxResults, accessToken)
}
\arguments{
\item{nextToken}{(Optional) When requesting subsequent pages, this is the page token from
the previous response output.}

\item{maxResults}{This is the number of items clients can request per page.}

\item{accessToken}{[required] The token issued by the \code{CreateToken} API call. For more information,
see
\href{https://docs.aws.amazon.com/singlesignon/latest/OIDCAPIReference/API_CreateToken.html}{CreateToken}
in the \emph{AWS SSO OIDC API Reference Guide}.}
}
\description{
Lists all AWS accounts assigned to the user. These AWS accounts are
assigned by the administrator of the account. For more information, see
\href{https://docs.aws.amazon.com/singlesignon/latest/userguide/useraccess.html#assignusers}{Assign User Access}
in the \emph{AWS SSO User Guide}. This operation returns a paginated
response.
}
\section{Request syntax}{
\preformatted{svc$list_accounts(
  nextToken = "string",
  maxResults = 123,
  accessToken = "string"
)
}
}

\keyword{internal}
