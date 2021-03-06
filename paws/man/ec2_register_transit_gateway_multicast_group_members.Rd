% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ec2_operations.R
\name{ec2_register_transit_gateway_multicast_group_members}
\alias{ec2_register_transit_gateway_multicast_group_members}
\title{Registers members (network interfaces) with the transit gateway
multicast group}
\usage{
ec2_register_transit_gateway_multicast_group_members(
  TransitGatewayMulticastDomainId, GroupIpAddress, NetworkInterfaceIds,
  DryRun)
}
\arguments{
\item{TransitGatewayMulticastDomainId}{The ID of the transit gateway multicast domain.}

\item{GroupIpAddress}{The IP address assigned to the transit gateway multicast group.}

\item{NetworkInterfaceIds}{The group members\' network interface IDs to register with the transit
gateway multicast group.}

\item{DryRun}{Checks whether you have the required permissions for the action, without
actually making the request, and provides an error response. If you have
the required permissions, the error response is \code{DryRunOperation}.
Otherwise, it is \code{UnauthorizedOperation}.}
}
\description{
Registers members (network interfaces) with the transit gateway
multicast group. A member is a network interface associated with a
supported EC2 instance that receives multicast traffic. For information
about supported instances, see \href{https://docs.aws.amazon.com/vpc/latest/tgw/transit-gateway-limits.html#multicast-limits}{Multicast Consideration}
in \emph{Amazon VPC Transit Gateways}.
}
\details{
After you add the members, use
\href{https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SearchTransitGatewayMulticastGroups.html}{SearchTransitGatewayMulticastGroups}
to verify that the members were added to the transit gateway multicast
group.
}
\section{Request syntax}{
\preformatted{svc$register_transit_gateway_multicast_group_members(
  TransitGatewayMulticastDomainId = "string",
  GroupIpAddress = "string",
  NetworkInterfaceIds = list(
    "string"
  ),
  DryRun = TRUE|FALSE
)
}
}

\keyword{internal}
