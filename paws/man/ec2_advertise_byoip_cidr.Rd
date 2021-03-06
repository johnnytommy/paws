% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ec2_operations.R
\name{ec2_advertise_byoip_cidr}
\alias{ec2_advertise_byoip_cidr}
\title{Advertises an IPv4 address range that is provisioned for use with your
AWS resources through bring your own IP addresses (BYOIP)}
\usage{
ec2_advertise_byoip_cidr(Cidr, DryRun)
}
\arguments{
\item{Cidr}{[required] The IPv4 address range, in CIDR notation. This must be the exact range
that you provisioned. You can\'t advertise only a portion of the
provisioned range.}

\item{DryRun}{Checks whether you have the required permissions for the action, without
actually making the request, and provides an error response. If you have
the required permissions, the error response is \code{DryRunOperation}.
Otherwise, it is \code{UnauthorizedOperation}.}
}
\description{
Advertises an IPv4 address range that is provisioned for use with your
AWS resources through bring your own IP addresses (BYOIP).
}
\details{
You can perform this operation at most once every 10 seconds, even if
you specify different address ranges each time.

We recommend that you stop advertising the BYOIP CIDR from other
locations when you advertise it from AWS. To minimize down time, you can
configure your AWS resources to use an address from a BYOIP CIDR before
it is advertised, and then simultaneously stop advertising it from the
current location and start advertising it through AWS.

It can take a few minutes before traffic to the specified addresses
starts routing to AWS because of BGP propagation delays.

To stop advertising the BYOIP CIDR, use WithdrawByoipCidr.
}
\section{Request syntax}{
\preformatted{svc$advertise_byoip_cidr(
  Cidr = "string",
  DryRun = TRUE|FALSE
)
}
}

\keyword{internal}
