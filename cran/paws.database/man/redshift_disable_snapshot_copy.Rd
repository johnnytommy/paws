% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redshift_operations.R
\name{redshift_disable_snapshot_copy}
\alias{redshift_disable_snapshot_copy}
\title{Disables the automatic copying of snapshots from one region to another
region for a specified cluster}
\usage{
redshift_disable_snapshot_copy(ClusterIdentifier)
}
\arguments{
\item{ClusterIdentifier}{[required] The unique identifier of the source cluster that you want to disable
copying of snapshots to a destination region.

Constraints: Must be the valid name of an existing cluster that has
cross-region snapshot copy enabled.}
}
\description{
Disables the automatic copying of snapshots from one region to another
region for a specified cluster.
}
\details{
If your cluster and its snapshots are encrypted using a customer master
key (CMK) from AWS KMS, use DeleteSnapshotCopyGrant to delete the grant
that grants Amazon Redshift permission to the CMK in the destination
region.
}
\section{Request syntax}{
\preformatted{svc$disable_snapshot_copy(
  ClusterIdentifier = "string"
)
}
}

\keyword{internal}
