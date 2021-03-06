% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iam_operations.R
\name{iam_put_user_permissions_boundary}
\alias{iam_put_user_permissions_boundary}
\title{Adds or updates the policy that is specified as the IAM user's
permissions boundary}
\usage{
iam_put_user_permissions_boundary(UserName, PermissionsBoundary)
}
\arguments{
\item{UserName}{[required] The name (friendly name, not ARN) of the IAM user for which you want to
set the permissions boundary.}

\item{PermissionsBoundary}{[required] The ARN of the policy that is used to set the permissions boundary for
the user.}
}
\description{
Adds or updates the policy that is specified as the IAM user\'s
permissions boundary. You can use an AWS managed policy or a customer
managed policy to set the boundary for a user. Use the boundary to
control the maximum permissions that the user can have. Setting a
permissions boundary is an advanced feature that can affect the
permissions for the user.
}
\details{
Policies that are used as permissions boundaries do not provide
permissions. You must also attach a permissions policy to the user. To
learn how the effective permissions for a user are evaluated, see \href{https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_evaluation-logic.html}{IAM JSON Policy Evaluation Logic}
in the IAM User Guide.
}
\section{Request syntax}{
\preformatted{svc$put_user_permissions_boundary(
  UserName = "string",
  PermissionsBoundary = "string"
)
}
}

\keyword{internal}
