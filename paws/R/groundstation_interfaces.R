# This file is generated by make.paws. Please do not edit here.
#' @importFrom paws.common populate
#' @include groundstation_service.R
NULL

.groundstation$cancel_contact_input <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(contactId = structure(logical(0), tags = list(location = "uri", locationName = "contactId", type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$cancel_contact_output <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(contactId = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$create_config_input <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(configData = structure(list(antennaDownlinkConfig = structure(list(spectrumConfig = structure(list(bandwidth = structure(list(units = structure(logical(0), tags = list(type = "string")), value = structure(logical(0), tags = list(type = "double", box = TRUE))), tags = list(type = "structure")), centerFrequency = structure(list(units = structure(logical(0), tags = list(type = "string")), value = structure(logical(0), tags = list(type = "double", box = TRUE))), tags = list(type = "structure")), polarization = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure"))), tags = list(type = "structure")), antennaDownlinkDemodDecodeConfig = structure(list(decodeConfig = structure(list(unvalidatedJSON = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure")), demodulationConfig = structure(list(unvalidatedJSON = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure")), spectrumConfig = structure(list(bandwidth = structure(list(units = structure(logical(0), tags = list(type = "string")), value = structure(logical(0), tags = list(type = "double", box = TRUE))), tags = list(type = "structure")), centerFrequency = structure(list(units = structure(logical(0), tags = list(type = "string")), value = structure(logical(0), tags = list(type = "double", box = TRUE))), tags = list(type = "structure")), polarization = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure"))), tags = list(type = "structure")), antennaUplinkConfig = structure(list(spectrumConfig = structure(list(centerFrequency = structure(list(units = structure(logical(0), tags = list(type = "string")), value = structure(logical(0), tags = list(type = "double", box = TRUE))), tags = list(type = "structure")), polarization = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure")), targetEirp = structure(list(units = structure(logical(0), tags = list(type = "string")), value = structure(logical(0), tags = list(type = "double", box = TRUE))), tags = list(type = "structure"))), tags = list(type = "structure")), dataflowEndpointConfig = structure(list(dataflowEndpointName = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure")), trackingConfig = structure(list(autotrack = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure")), uplinkEchoConfig = structure(list(antennaUplinkConfigArn = structure(logical(0), tags = list(type = "string")), enabled = structure(logical(0), tags = list(type = "boolean", box = TRUE))), tags = list(type = "structure"))), tags = list(type = "structure")), name = structure(logical(0), tags = list(type = "string")), tags = structure(list(structure(logical(0), tags = list(type = "string"))), tags = list(type = "map"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$create_config_output <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(configArn = structure(logical(0), tags = list(type = "string")), configId = structure(logical(0), tags = list(type = "string")), configType = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$create_dataflow_endpoint_group_input <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(endpointDetails = structure(list(structure(list(endpoint = structure(list(address = structure(list(name = structure(logical(0), tags = list(type = "string")), port = structure(logical(0), tags = list(type = "integer", box = TRUE))), tags = list(type = "structure")), name = structure(logical(0), tags = list(type = "string")), status = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure")), securityDetails = structure(list(roleArn = structure(logical(0), tags = list(type = "string")), securityGroupIds = structure(list(structure(logical(0), tags = list(type = "string"))), tags = list(type = "list")), subnetIds = structure(list(structure(logical(0), tags = list(type = "string"))), tags = list(type = "list"))), tags = list(type = "structure"))), tags = list(type = "structure"))), tags = list(type = "list")), tags = structure(list(structure(logical(0), tags = list(type = "string"))), tags = list(type = "map"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$create_dataflow_endpoint_group_output <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(dataflowEndpointGroupId = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$create_mission_profile_input <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(contactPostPassDurationSeconds = structure(logical(0), tags = list(type = "integer")), contactPrePassDurationSeconds = structure(logical(0), tags = list(type = "integer")), dataflowEdges = structure(list(structure(list(structure(logical(0), tags = list(type = "string"))), tags = list(type = "list"))), tags = list(type = "list")), minimumViableContactDurationSeconds = structure(logical(0), tags = list(type = "integer")), name = structure(logical(0), tags = list(type = "string")), tags = structure(list(structure(logical(0), tags = list(type = "string"))), tags = list(type = "map")), trackingConfigArn = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$create_mission_profile_output <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(missionProfileId = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$delete_config_input <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(configId = structure(logical(0), tags = list(location = "uri", locationName = "configId", type = "string")), configType = structure(logical(0), tags = list(location = "uri", locationName = "configType", type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$delete_config_output <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(configArn = structure(logical(0), tags = list(type = "string")), configId = structure(logical(0), tags = list(type = "string")), configType = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$delete_dataflow_endpoint_group_input <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(dataflowEndpointGroupId = structure(logical(0), tags = list(location = "uri", locationName = "dataflowEndpointGroupId", type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$delete_dataflow_endpoint_group_output <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(dataflowEndpointGroupId = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$delete_mission_profile_input <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(missionProfileId = structure(logical(0), tags = list(location = "uri", locationName = "missionProfileId", type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$delete_mission_profile_output <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(missionProfileId = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$describe_contact_input <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(contactId = structure(logical(0), tags = list(location = "uri", locationName = "contactId", type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$describe_contact_output <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(contactId = structure(logical(0), tags = list(type = "string")), contactStatus = structure(logical(0), tags = list(type = "string")), endTime = structure(logical(0), tags = list(type = "timestamp")), errorMessage = structure(logical(0), tags = list(type = "string")), groundStation = structure(logical(0), tags = list(type = "string")), maximumElevation = structure(list(unit = structure(logical(0), tags = list(type = "string")), value = structure(logical(0), tags = list(type = "double", box = TRUE))), tags = list(type = "structure")), missionProfileArn = structure(logical(0), tags = list(type = "string")), postPassEndTime = structure(logical(0), tags = list(type = "timestamp")), prePassStartTime = structure(logical(0), tags = list(type = "timestamp")), satelliteArn = structure(logical(0), tags = list(type = "string")), startTime = structure(logical(0), tags = list(type = "timestamp")), tags = structure(list(structure(logical(0), tags = list(type = "string"))), tags = list(type = "map"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$get_config_input <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(configId = structure(logical(0), tags = list(location = "uri", locationName = "configId", type = "string")), configType = structure(logical(0), tags = list(location = "uri", locationName = "configType", type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$get_config_output <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(configArn = structure(logical(0), tags = list(type = "string")), configData = structure(list(antennaDownlinkConfig = structure(list(spectrumConfig = structure(list(bandwidth = structure(list(units = structure(logical(0), tags = list(type = "string")), value = structure(logical(0), tags = list(type = "double", box = TRUE))), tags = list(type = "structure")), centerFrequency = structure(list(units = structure(logical(0), tags = list(type = "string")), value = structure(logical(0), tags = list(type = "double", box = TRUE))), tags = list(type = "structure")), polarization = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure"))), tags = list(type = "structure")), antennaDownlinkDemodDecodeConfig = structure(list(decodeConfig = structure(list(unvalidatedJSON = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure")), demodulationConfig = structure(list(unvalidatedJSON = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure")), spectrumConfig = structure(list(bandwidth = structure(list(units = structure(logical(0), tags = list(type = "string")), value = structure(logical(0), tags = list(type = "double", box = TRUE))), tags = list(type = "structure")), centerFrequency = structure(list(units = structure(logical(0), tags = list(type = "string")), value = structure(logical(0), tags = list(type = "double", box = TRUE))), tags = list(type = "structure")), polarization = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure"))), tags = list(type = "structure")), antennaUplinkConfig = structure(list(spectrumConfig = structure(list(centerFrequency = structure(list(units = structure(logical(0), tags = list(type = "string")), value = structure(logical(0), tags = list(type = "double", box = TRUE))), tags = list(type = "structure")), polarization = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure")), targetEirp = structure(list(units = structure(logical(0), tags = list(type = "string")), value = structure(logical(0), tags = list(type = "double", box = TRUE))), tags = list(type = "structure"))), tags = list(type = "structure")), dataflowEndpointConfig = structure(list(dataflowEndpointName = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure")), trackingConfig = structure(list(autotrack = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure")), uplinkEchoConfig = structure(list(antennaUplinkConfigArn = structure(logical(0), tags = list(type = "string")), enabled = structure(logical(0), tags = list(type = "boolean", box = TRUE))), tags = list(type = "structure"))), tags = list(type = "structure")), configId = structure(logical(0), tags = list(type = "string")), configType = structure(logical(0), tags = list(type = "string")), name = structure(logical(0), tags = list(type = "string")), tags = structure(list(structure(logical(0), tags = list(type = "string"))), tags = list(type = "map"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$get_dataflow_endpoint_group_input <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(dataflowEndpointGroupId = structure(logical(0), tags = list(location = "uri", locationName = "dataflowEndpointGroupId", type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$get_dataflow_endpoint_group_output <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(dataflowEndpointGroupArn = structure(logical(0), tags = list(type = "string")), dataflowEndpointGroupId = structure(logical(0), tags = list(type = "string")), endpointsDetails = structure(list(structure(list(endpoint = structure(list(address = structure(list(name = structure(logical(0), tags = list(type = "string")), port = structure(logical(0), tags = list(type = "integer", box = TRUE))), tags = list(type = "structure")), name = structure(logical(0), tags = list(type = "string")), status = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure")), securityDetails = structure(list(roleArn = structure(logical(0), tags = list(type = "string")), securityGroupIds = structure(list(structure(logical(0), tags = list(type = "string"))), tags = list(type = "list")), subnetIds = structure(list(structure(logical(0), tags = list(type = "string"))), tags = list(type = "list"))), tags = list(type = "structure"))), tags = list(type = "structure"))), tags = list(type = "list")), tags = structure(list(structure(logical(0), tags = list(type = "string"))), tags = list(type = "map"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$get_mission_profile_input <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(missionProfileId = structure(logical(0), tags = list(location = "uri", locationName = "missionProfileId", type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$get_mission_profile_output <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(contactPostPassDurationSeconds = structure(logical(0), tags = list(type = "integer")), contactPrePassDurationSeconds = structure(logical(0), tags = list(type = "integer")), dataflowEdges = structure(list(structure(list(structure(logical(0), tags = list(type = "string"))), tags = list(type = "list"))), tags = list(type = "list")), minimumViableContactDurationSeconds = structure(logical(0), tags = list(type = "integer")), missionProfileArn = structure(logical(0), tags = list(type = "string")), missionProfileId = structure(logical(0), tags = list(type = "string")), name = structure(logical(0), tags = list(type = "string")), region = structure(logical(0), tags = list(type = "string")), tags = structure(list(structure(logical(0), tags = list(type = "string"))), tags = list(type = "map")), trackingConfigArn = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$list_configs_input <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(maxResults = structure(logical(0), tags = list(location = "querystring", locationName = "maxResults", type = "integer", box = TRUE)), nextToken = structure(logical(0), tags = list(location = "querystring", locationName = "nextToken", type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$list_configs_output <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(configList = structure(list(structure(list(configArn = structure(logical(0), tags = list(type = "string")), configId = structure(logical(0), tags = list(type = "string")), configType = structure(logical(0), tags = list(type = "string")), name = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure"))), tags = list(type = "list")), nextToken = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$list_contacts_input <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(endTime = structure(logical(0), tags = list(type = "timestamp")), groundStation = structure(logical(0), tags = list(type = "string")), maxResults = structure(logical(0), tags = list(type = "integer", box = TRUE)), missionProfileArn = structure(logical(0), tags = list(type = "string")), nextToken = structure(logical(0), tags = list(type = "string")), satelliteArn = structure(logical(0), tags = list(type = "string")), startTime = structure(logical(0), tags = list(type = "timestamp")), statusList = structure(list(structure(logical(0), tags = list(type = "string"))), tags = list(type = "list"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$list_contacts_output <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(contactList = structure(list(structure(list(contactId = structure(logical(0), tags = list(type = "string")), contactStatus = structure(logical(0), tags = list(type = "string")), endTime = structure(logical(0), tags = list(type = "timestamp")), errorMessage = structure(logical(0), tags = list(type = "string")), groundStation = structure(logical(0), tags = list(type = "string")), maximumElevation = structure(list(unit = structure(logical(0), tags = list(type = "string")), value = structure(logical(0), tags = list(type = "double", box = TRUE))), tags = list(type = "structure")), missionProfileArn = structure(logical(0), tags = list(type = "string")), postPassEndTime = structure(logical(0), tags = list(type = "timestamp")), prePassStartTime = structure(logical(0), tags = list(type = "timestamp")), satelliteArn = structure(logical(0), tags = list(type = "string")), startTime = structure(logical(0), tags = list(type = "timestamp")), tags = structure(list(structure(logical(0), tags = list(type = "string"))), tags = list(type = "map"))), tags = list(type = "structure"))), tags = list(type = "list")), nextToken = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$list_dataflow_endpoint_groups_input <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(maxResults = structure(logical(0), tags = list(location = "querystring", locationName = "maxResults", type = "integer", box = TRUE)), nextToken = structure(logical(0), tags = list(location = "querystring", locationName = "nextToken", type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$list_dataflow_endpoint_groups_output <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(dataflowEndpointGroupList = structure(list(structure(list(dataflowEndpointGroupArn = structure(logical(0), tags = list(type = "string")), dataflowEndpointGroupId = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure"))), tags = list(type = "list")), nextToken = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$list_mission_profiles_input <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(maxResults = structure(logical(0), tags = list(location = "querystring", locationName = "maxResults", type = "integer", box = TRUE)), nextToken = structure(logical(0), tags = list(location = "querystring", locationName = "nextToken", type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$list_mission_profiles_output <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(missionProfileList = structure(list(structure(list(missionProfileArn = structure(logical(0), tags = list(type = "string")), missionProfileId = structure(logical(0), tags = list(type = "string")), name = structure(logical(0), tags = list(type = "string")), region = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure"))), tags = list(type = "list")), nextToken = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$reserve_contact_input <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(endTime = structure(logical(0), tags = list(type = "timestamp")), groundStation = structure(logical(0), tags = list(type = "string")), missionProfileArn = structure(logical(0), tags = list(type = "string")), satelliteArn = structure(logical(0), tags = list(type = "string")), startTime = structure(logical(0), tags = list(type = "timestamp")), tags = structure(list(structure(logical(0), tags = list(type = "string"))), tags = list(type = "map"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$reserve_contact_output <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(contactId = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$update_config_input <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(configData = structure(list(antennaDownlinkConfig = structure(list(spectrumConfig = structure(list(bandwidth = structure(list(units = structure(logical(0), tags = list(type = "string")), value = structure(logical(0), tags = list(type = "double", box = TRUE))), tags = list(type = "structure")), centerFrequency = structure(list(units = structure(logical(0), tags = list(type = "string")), value = structure(logical(0), tags = list(type = "double", box = TRUE))), tags = list(type = "structure")), polarization = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure"))), tags = list(type = "structure")), antennaDownlinkDemodDecodeConfig = structure(list(decodeConfig = structure(list(unvalidatedJSON = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure")), demodulationConfig = structure(list(unvalidatedJSON = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure")), spectrumConfig = structure(list(bandwidth = structure(list(units = structure(logical(0), tags = list(type = "string")), value = structure(logical(0), tags = list(type = "double", box = TRUE))), tags = list(type = "structure")), centerFrequency = structure(list(units = structure(logical(0), tags = list(type = "string")), value = structure(logical(0), tags = list(type = "double", box = TRUE))), tags = list(type = "structure")), polarization = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure"))), tags = list(type = "structure")), antennaUplinkConfig = structure(list(spectrumConfig = structure(list(centerFrequency = structure(list(units = structure(logical(0), tags = list(type = "string")), value = structure(logical(0), tags = list(type = "double", box = TRUE))), tags = list(type = "structure")), polarization = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure")), targetEirp = structure(list(units = structure(logical(0), tags = list(type = "string")), value = structure(logical(0), tags = list(type = "double", box = TRUE))), tags = list(type = "structure"))), tags = list(type = "structure")), dataflowEndpointConfig = structure(list(dataflowEndpointName = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure")), trackingConfig = structure(list(autotrack = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure")), uplinkEchoConfig = structure(list(antennaUplinkConfigArn = structure(logical(0), tags = list(type = "string")), enabled = structure(logical(0), tags = list(type = "boolean", box = TRUE))), tags = list(type = "structure"))), tags = list(type = "structure")), configId = structure(logical(0), tags = list(location = "uri", locationName = "configId", type = "string")), configType = structure(logical(0), tags = list(location = "uri", locationName = "configType", type = "string")), name = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$update_config_output <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(configArn = structure(logical(0), tags = list(type = "string")), configId = structure(logical(0), tags = list(type = "string")), configType = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$update_mission_profile_input <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(contactPostPassDurationSeconds = structure(logical(0), tags = list(type = "integer")), contactPrePassDurationSeconds = structure(logical(0), tags = list(type = "integer")), dataflowEdges = structure(list(structure(list(structure(logical(0), tags = list(type = "string"))), tags = list(type = "list"))), tags = list(type = "list")), minimumViableContactDurationSeconds = structure(logical(0), tags = list(type = "integer")), missionProfileId = structure(logical(0), tags = list(location = "uri", locationName = "missionProfileId", type = "string")), name = structure(logical(0), tags = list(type = "string")), trackingConfigArn = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$update_mission_profile_output <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(missionProfileId = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$get_minute_usage_input <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(month = structure(logical(0), tags = list(type = "integer", box = TRUE)), year = structure(logical(0), tags = list(type = "integer", box = TRUE))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$get_minute_usage_output <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(estimatedMinutesRemaining = structure(logical(0), tags = list(type = "integer", box = TRUE)), isReservedMinutesCustomer = structure(logical(0), tags = list(type = "boolean", box = TRUE)), totalReservedMinuteAllocation = structure(logical(0), tags = list(type = "integer", box = TRUE)), totalScheduledMinutes = structure(logical(0), tags = list(type = "integer", box = TRUE)), upcomingMinutesScheduled = structure(logical(0), tags = list(type = "integer", box = TRUE))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$get_satellite_input <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(satelliteId = structure(logical(0), tags = list(location = "uri", locationName = "satelliteId", type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$get_satellite_output <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(dateCreated = structure(logical(0), tags = list(type = "timestamp")), lastUpdated = structure(logical(0), tags = list(type = "timestamp")), noradSatelliteID = structure(logical(0), tags = list(type = "integer")), satelliteArn = structure(logical(0), tags = list(type = "string")), satelliteId = structure(logical(0), tags = list(type = "string")), tags = structure(list(structure(logical(0), tags = list(type = "string"))), tags = list(type = "map"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$list_ground_stations_input <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(maxResults = structure(logical(0), tags = list(location = "querystring", locationName = "maxResults", type = "integer", box = TRUE)), nextToken = structure(logical(0), tags = list(location = "querystring", locationName = "nextToken", type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$list_ground_stations_output <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(groundStationList = structure(list(structure(list(groundStationId = structure(logical(0), tags = list(type = "string")), groundStationName = structure(logical(0), tags = list(type = "string")), region = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure"))), tags = list(type = "list")), nextToken = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$list_satellites_input <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(maxResults = structure(logical(0), tags = list(location = "querystring", locationName = "maxResults", type = "integer", box = TRUE)), nextToken = structure(logical(0), tags = list(location = "querystring", locationName = "nextToken", type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$list_satellites_output <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(nextToken = structure(logical(0), tags = list(type = "string")), satellites = structure(list(structure(list(noradSatelliteID = structure(logical(0), tags = list(type = "integer")), satelliteArn = structure(logical(0), tags = list(type = "string")), satelliteId = structure(logical(0), tags = list(type = "string"))), tags = list(type = "structure"))), tags = list(type = "list"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$list_tags_for_resource_input <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(resourceArn = structure(logical(0), tags = list(location = "uri", locationName = "resourceArn", type = "string"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$list_tags_for_resource_output <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(tags = structure(list(structure(logical(0), tags = list(type = "string"))), tags = list(type = "map"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$tag_resource_input <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(resourceArn = structure(logical(0), tags = list(location = "uri", locationName = "resourceArn", type = "string")), tags = structure(list(structure(logical(0), tags = list(type = "string"))), tags = list(type = "map"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$tag_resource_output <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$untag_resource_input <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(resourceArn = structure(logical(0), tags = list(location = "uri", locationName = "resourceArn", type = "string")), tagKeys = structure(list(structure(logical(0), tags = list(type = "string"))), tags = list(location = "querystring", locationName = "tagKeys", type = "list"))), tags = list(type = "structure"))
  return(populate(args, shape))
}

.groundstation$untag_resource_output <- function(...) {
  args <- c(as.list(environment()), list(...))
  shape <- structure(list(), tags = list(type = "structure"))
  return(populate(args, shape))
}
