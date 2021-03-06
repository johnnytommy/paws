% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chime_operations.R
\name{chime_create_meeting}
\alias{chime_create_meeting}
\title{Creates a new Amazon Chime SDK meeting in the specified media Region
with no initial attendees}
\usage{
chime_create_meeting(ClientRequestToken, MeetingHostId, MediaRegion,
  NotificationsConfiguration)
}
\arguments{
\item{ClientRequestToken}{[required] The unique identifier for the client request. Use a different token for
different meetings.}

\item{MeetingHostId}{Reserved.}

\item{MediaRegion}{The Region in which to create the meeting. Available values:
\code{ap-northeast-1}, \code{ap-southeast-1}, \code{ap-southeast-2}, \code{ca-central-1},
\code{eu-central-1}, \code{eu-north-1}, \code{eu-west-1}, \code{eu-west-2}, \code{eu-west-3},
\code{sa-east-1}, \code{us-east-1}, \code{us-east-2}, \code{us-west-1}, \code{us-west-2}.}

\item{NotificationsConfiguration}{The configuration for resource targets to receive notifications when
meeting and attendee events occur.}
}
\description{
Creates a new Amazon Chime SDK meeting in the specified media Region
with no initial attendees. For more information about the Amazon Chime
SDK, see \href{https://docs.aws.amazon.com/chime/latest/dg/meetings-sdk.html}{Using the Amazon Chime SDK} in
the \emph{Amazon Chime Developer Guide}.
}
\section{Request syntax}{
\preformatted{svc$create_meeting(
  ClientRequestToken = "string",
  MeetingHostId = "string",
  MediaRegion = "string",
  NotificationsConfiguration = list(
    SnsTopicArn = "string",
    SqsQueueArn = "string"
  )
)
}
}

\keyword{internal}
