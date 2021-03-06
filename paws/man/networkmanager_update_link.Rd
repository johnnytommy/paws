% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/networkmanager_operations.R
\name{networkmanager_update_link}
\alias{networkmanager_update_link}
\title{Updates the details for an existing link}
\usage{
networkmanager_update_link(GlobalNetworkId, LinkId, Description, Type,
  Bandwidth, Provider)
}
\arguments{
\item{GlobalNetworkId}{[required] The ID of the global network.}

\item{LinkId}{[required] The ID of the link.}

\item{Description}{A description of the link.

Length Constraints: Maximum length of 256 characters.}

\item{Type}{The type of the link.

Length Constraints: Maximum length of 128 characters.}

\item{Bandwidth}{The upload and download speed in Mbps.}

\item{Provider}{The provider of the link.

Length Constraints: Maximum length of 128 characters.}
}
\description{
Updates the details for an existing link. To remove information for any
of the parameters, specify an empty string.
}
\section{Request syntax}{
\preformatted{svc$update_link(
  GlobalNetworkId = "string",
  LinkId = "string",
  Description = "string",
  Type = "string",
  Bandwidth = list(
    UploadSpeed = 123,
    DownloadSpeed = 123
  ),
  Provider = "string"
)
}
}

\keyword{internal}
