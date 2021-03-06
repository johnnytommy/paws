% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/groundstation_operations.R
\name{groundstation_create_config}
\alias{groundstation_create_config}
\title{Creates a Config with the specified configData parameters}
\usage{
groundstation_create_config(configData, name, tags)
}
\arguments{
\item{configData}{[required] Parameters of a \code{Config}.}

\item{name}{[required] Name of a \code{Config}.}

\item{tags}{Tags assigned to a \code{Config}.}
}
\description{
Creates a \code{Config} with the specified \code{configData} parameters.
}
\details{
Only one type of \code{configData} can be specified.
}
\section{Request syntax}{
\preformatted{svc$create_config(
  configData = list(
    antennaDownlinkConfig = list(
      spectrumConfig = list(
        bandwidth = list(
          units = "GHz"|"MHz"|"kHz",
          value = 123.0
        ),
        centerFrequency = list(
          units = "GHz"|"MHz"|"kHz",
          value = 123.0
        ),
        polarization = "LEFT_HAND"|"NONE"|"RIGHT_HAND"
      )
    ),
    antennaDownlinkDemodDecodeConfig = list(
      decodeConfig = list(
        unvalidatedJSON = "string"
      ),
      demodulationConfig = list(
        unvalidatedJSON = "string"
      ),
      spectrumConfig = list(
        bandwidth = list(
          units = "GHz"|"MHz"|"kHz",
          value = 123.0
        ),
        centerFrequency = list(
          units = "GHz"|"MHz"|"kHz",
          value = 123.0
        ),
        polarization = "LEFT_HAND"|"NONE"|"RIGHT_HAND"
      )
    ),
    antennaUplinkConfig = list(
      spectrumConfig = list(
        centerFrequency = list(
          units = "GHz"|"MHz"|"kHz",
          value = 123.0
        ),
        polarization = "LEFT_HAND"|"NONE"|"RIGHT_HAND"
      ),
      targetEirp = list(
        units = "dBW",
        value = 123.0
      )
    ),
    dataflowEndpointConfig = list(
      dataflowEndpointName = "string"
    ),
    trackingConfig = list(
      autotrack = "PREFERRED"|"REMOVED"|"REQUIRED"
    ),
    uplinkEchoConfig = list(
      antennaUplinkConfigArn = "string",
      enabled = TRUE|FALSE
    )
  ),
  name = "string",
  tags = list(
    "string"
  )
)
}
}

\keyword{internal}
