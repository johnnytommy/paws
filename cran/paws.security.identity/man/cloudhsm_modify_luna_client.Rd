% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cloudhsm_operations.R
\name{cloudhsm_modify_luna_client}
\alias{cloudhsm_modify_luna_client}
\title{This is documentation for AWS CLOUDHSM CLASSIC}
\usage{
cloudhsm_modify_luna_client(ClientArn, Certificate)
}
\arguments{
\item{ClientArn}{[required] The ARN of the client.}

\item{Certificate}{[required] The new certificate for the client.}
}
\description{
This is documentation for \strong{AWS CloudHSM Classic}. For more
information, see \href{http://aws.amazon.com/cloudhsm/faqs-classic/}{AWS CloudHSM Classic FAQs}, the \href{http://docs.aws.amazon.com/cloudhsm/classic/userguide/}{AWS CloudHSM Classic User Guide}, and the
\href{http://docs.aws.amazon.com/cloudhsm/classic/APIReference/}{AWS CloudHSM Classic API Reference}.
}
\details{
\strong{For information about the current version of AWS CloudHSM}, see \href{http://aws.amazon.com/cloudhsm/}{AWS CloudHSM}, the \href{http://docs.aws.amazon.com/cloudhsm/latest/userguide/}{AWS CloudHSM User Guide}, and the
\href{http://docs.aws.amazon.com/cloudhsm/latest/APIReference/}{AWS CloudHSM API Reference}.

Modifies the certificate used by the client.

This action can potentially start a workflow to install the new
certificate on the client\'s HSMs.
}
\section{Request syntax}{
\preformatted{svc$modify_luna_client(
  ClientArn = "string",
  Certificate = "string"
)
}
}

\keyword{internal}
