% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kafka_operations.R
\name{kafka_update_broker_storage}
\alias{kafka_update_broker_storage}
\title{Updates the EBS storage associated with MSK brokers}
\usage{
kafka_update_broker_storage(ClusterArn, CurrentVersion,
  TargetBrokerEBSVolumeInfo)
}
\arguments{
\item{ClusterArn}{[required]
<p>The Amazon Resource Name (ARN) that uniquely identifies the cluster.</p>}

\item{CurrentVersion}{[required]
<p>The version of cluster to update from. A successful operation will then generate a new version.</p>}

\item{TargetBrokerEBSVolumeInfo}{[required]
<p>Describes the target volume size and the ID of the broker to apply the update to.</p>}
}
\description{
\preformatted{        <p>Updates the EBS storage associated with MSK brokers.</p>
}
}
\section{Request syntax}{
\preformatted{svc$update_broker_storage(
  ClusterArn = "string",
  CurrentVersion = "string",
  TargetBrokerEBSVolumeInfo = list(
    list(
      KafkaBrokerNodeId = "string",
      VolumeSizeGB = 123
    )
  )
)
}
}

\keyword{internal}
