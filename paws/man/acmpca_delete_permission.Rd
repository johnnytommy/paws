% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/acmpca_operations.R
\name{acmpca_delete_permission}
\alias{acmpca_delete_permission}
\title{Revokes permissions that a private CA assigned to a designated AWS
service}
\usage{
acmpca_delete_permission(CertificateAuthorityArn, Principal,
  SourceAccount)
}
\arguments{
\item{CertificateAuthorityArn}{[required] The Amazon Resource Number (ARN) of the private CA that issued the
permissions. You can find the CA\'s ARN by calling the
ListCertificateAuthorities action. This must have the following form:

\verb{arn:aws:acm-pca:<i>region</i>:<i>account</i>:certificate-authority/<i>12345678-1234-1234-1234-123456789012</i> }.}

\item{Principal}{[required] The AWS service or identity that will have its CA permissions revoked.
At this time, the only valid service principal is \code{acm.amazonaws.com}}

\item{SourceAccount}{The AWS account that calls this action.}
}
\description{
Revokes permissions that a private CA assigned to a designated AWS
service. Permissions can be created with the CreatePermission action and
listed with the ListPermissions action.
}
\section{Request syntax}{
\preformatted{svc$delete_permission(
  CertificateAuthorityArn = "string",
  Principal = "string",
  SourceAccount = "string"
)
}
}

\keyword{internal}
