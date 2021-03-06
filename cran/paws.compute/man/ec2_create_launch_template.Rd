% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ec2_operations.R
\name{ec2_create_launch_template}
\alias{ec2_create_launch_template}
\title{Creates a launch template}
\usage{
ec2_create_launch_template(DryRun, ClientToken, LaunchTemplateName,
  VersionDescription, LaunchTemplateData, TagSpecifications)
}
\arguments{
\item{DryRun}{Checks whether you have the required permissions for the action, without
actually making the request, and provides an error response. If you have
the required permissions, the error response is \code{DryRunOperation}.
Otherwise, it is \code{UnauthorizedOperation}.}

\item{ClientToken}{Unique, case-sensitive identifier you provide to ensure the idempotency
of the request. For more information, see \href{https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html}{Ensuring Idempotency}.

Constraint: Maximum 128 ASCII characters.}

\item{LaunchTemplateName}{[required] A name for the launch template.}

\item{VersionDescription}{A description for the first version of the launch template.}

\item{LaunchTemplateData}{[required] The information for the launch template.}

\item{TagSpecifications}{The tags to apply to the launch template during creation.}
}
\description{
Creates a launch template. A launch template contains the parameters to
launch an instance. When you launch an instance using RunInstances, you
can specify a launch template instead of providing the launch parameters
in the request.
}
\section{Request syntax}{
\preformatted{svc$create_launch_template(
  DryRun = TRUE|FALSE,
  ClientToken = "string",
  LaunchTemplateName = "string",
  VersionDescription = "string",
  LaunchTemplateData = list(
    KernelId = "string",
    EbsOptimized = TRUE|FALSE,
    IamInstanceProfile = list(
      Arn = "string",
      Name = "string"
    ),
    BlockDeviceMappings = list(
      list(
        DeviceName = "string",
        VirtualName = "string",
        Ebs = list(
          Encrypted = TRUE|FALSE,
          DeleteOnTermination = TRUE|FALSE,
          Iops = 123,
          KmsKeyId = "string",
          SnapshotId = "string",
          VolumeSize = 123,
          VolumeType = "standard"|"io1"|"gp2"|"sc1"|"st1"
        ),
        NoDevice = "string"
      )
    ),
    NetworkInterfaces = list(
      list(
        AssociatePublicIpAddress = TRUE|FALSE,
        DeleteOnTermination = TRUE|FALSE,
        Description = "string",
        DeviceIndex = 123,
        Groups = list(
          "string"
        ),
        InterfaceType = "string",
        Ipv6AddressCount = 123,
        Ipv6Addresses = list(
          list(
            Ipv6Address = "string"
          )
        ),
        NetworkInterfaceId = "string",
        PrivateIpAddress = "string",
        PrivateIpAddresses = list(
          list(
            Primary = TRUE|FALSE,
            PrivateIpAddress = "string"
          )
        ),
        SecondaryPrivateIpAddressCount = 123,
        SubnetId = "string"
      )
    ),
    ImageId = "string",
    InstanceType = "t1.micro"|"t2.nano"|"t2.micro"|"t2.small"|"t2.medium"|"t2.large"|"t2.xlarge"|"t2.2xlarge"|"t3.nano"|"t3.micro"|"t3.small"|"t3.medium"|"t3.large"|"t3.xlarge"|"t3.2xlarge"|"t3a.nano"|"t3a.micro"|"t3a.small"|"t3a.medium"|"t3a.large"|"t3a.xlarge"|"t3a.2xlarge"|"m1.small"|"m1.medium"|"m1.large"|"m1.xlarge"|"m3.medium"|"m3.large"|"m3.xlarge"|"m3.2xlarge"|"m4.large"|"m4.xlarge"|"m4.2xlarge"|"m4.4xlarge"|"m4.10xlarge"|"m4.16xlarge"|"m2.xlarge"|"m2.2xlarge"|"m2.4xlarge"|"cr1.8xlarge"|"r3.large"|"r3.xlarge"|"r3.2xlarge"|"r3.4xlarge"|"r3.8xlarge"|"r4.large"|"r4.xlarge"|"r4.2xlarge"|"r4.4xlarge"|"r4.8xlarge"|"r4.16xlarge"|"r5.large"|"r5.xlarge"|"r5.2xlarge"|"r5.4xlarge"|"r5.8xlarge"|"r5.12xlarge"|"r5.16xlarge"|"r5.24xlarge"|"r5.metal"|"r5a.large"|"r5a.xlarge"|"r5a.2xlarge"|"r5a.4xlarge"|"r5a.8xlarge"|"r5a.12xlarge"|"r5a.16xlarge"|"r5a.24xlarge"|"r5d.large"|"r5d.xlarge"|"r5d.2xlarge"|"r5d.4xlarge"|"r5d.8xlarge"|"r5d.12xlarge"|"r5d.16xlarge"|"r5d.24xlarge"|"r5d.metal"|"r5ad.large"|"r5ad.xlarge"|"r5ad.2xlarge"|"r5ad.4xlarge"|"r5ad.8xlarge"|"r5ad.12xlarge"|"r5ad.16xlarge"|"r5ad.24xlarge"|"x1.16xlarge"|"x1.32xlarge"|"x1e.xlarge"|"x1e.2xlarge"|"x1e.4xlarge"|"x1e.8xlarge"|"x1e.16xlarge"|"x1e.32xlarge"|"i2.xlarge"|"i2.2xlarge"|"i2.4xlarge"|"i2.8xlarge"|"i3.large"|"i3.xlarge"|"i3.2xlarge"|"i3.4xlarge"|"i3.8xlarge"|"i3.16xlarge"|"i3.metal"|"i3en.large"|"i3en.xlarge"|"i3en.2xlarge"|"i3en.3xlarge"|"i3en.6xlarge"|"i3en.12xlarge"|"i3en.24xlarge"|"i3en.metal"|"hi1.4xlarge"|"hs1.8xlarge"|"c1.medium"|"c1.xlarge"|"c3.large"|"c3.xlarge"|"c3.2xlarge"|"c3.4xlarge"|"c3.8xlarge"|"c4.large"|"c4.xlarge"|"c4.2xlarge"|"c4.4xlarge"|"c4.8xlarge"|"c5.large"|"c5.xlarge"|"c5.2xlarge"|"c5.4xlarge"|"c5.9xlarge"|"c5.12xlarge"|"c5.18xlarge"|"c5.24xlarge"|"c5.metal"|"c5d.large"|"c5d.xlarge"|"c5d.2xlarge"|"c5d.4xlarge"|"c5d.9xlarge"|"c5d.12xlarge"|"c5d.18xlarge"|"c5d.24xlarge"|"c5d.metal"|"c5n.large"|"c5n.xlarge"|"c5n.2xlarge"|"c5n.4xlarge"|"c5n.9xlarge"|"c5n.18xlarge"|"cc1.4xlarge"|"cc2.8xlarge"|"g2.2xlarge"|"g2.8xlarge"|"g3.4xlarge"|"g3.8xlarge"|"g3.16xlarge"|"g3s.xlarge"|"g4dn.xlarge"|"g4dn.2xlarge"|"g4dn.4xlarge"|"g4dn.8xlarge"|"g4dn.12xlarge"|"g4dn.16xlarge"|"cg1.4xlarge"|"p2.xlarge"|"p2.8xlarge"|"p2.16xlarge"|"p3.2xlarge"|"p3.8xlarge"|"p3.16xlarge"|"p3dn.24xlarge"|"d2.xlarge"|"d2.2xlarge"|"d2.4xlarge"|"d2.8xlarge"|"f1.2xlarge"|"f1.4xlarge"|"f1.16xlarge"|"m5.large"|"m5.xlarge"|"m5.2xlarge"|"m5.4xlarge"|"m5.8xlarge"|"m5.12xlarge"|"m5.16xlarge"|"m5.24xlarge"|"m5.metal"|"m5a.large"|"m5a.xlarge"|"m5a.2xlarge"|"m5a.4xlarge"|"m5a.8xlarge"|"m5a.12xlarge"|"m5a.16xlarge"|"m5a.24xlarge"|"m5d.large"|"m5d.xlarge"|"m5d.2xlarge"|"m5d.4xlarge"|"m5d.8xlarge"|"m5d.12xlarge"|"m5d.16xlarge"|"m5d.24xlarge"|"m5d.metal"|"m5ad.large"|"m5ad.xlarge"|"m5ad.2xlarge"|"m5ad.4xlarge"|"m5ad.8xlarge"|"m5ad.12xlarge"|"m5ad.16xlarge"|"m5ad.24xlarge"|"h1.2xlarge"|"h1.4xlarge"|"h1.8xlarge"|"h1.16xlarge"|"z1d.large"|"z1d.xlarge"|"z1d.2xlarge"|"z1d.3xlarge"|"z1d.6xlarge"|"z1d.12xlarge"|"z1d.metal"|"u-6tb1.metal"|"u-9tb1.metal"|"u-12tb1.metal"|"u-18tb1.metal"|"u-24tb1.metal"|"a1.medium"|"a1.large"|"a1.xlarge"|"a1.2xlarge"|"a1.4xlarge"|"a1.metal"|"m5dn.large"|"m5dn.xlarge"|"m5dn.2xlarge"|"m5dn.4xlarge"|"m5dn.8xlarge"|"m5dn.12xlarge"|"m5dn.16xlarge"|"m5dn.24xlarge"|"m5n.large"|"m5n.xlarge"|"m5n.2xlarge"|"m5n.4xlarge"|"m5n.8xlarge"|"m5n.12xlarge"|"m5n.16xlarge"|"m5n.24xlarge"|"r5dn.large"|"r5dn.xlarge"|"r5dn.2xlarge"|"r5dn.4xlarge"|"r5dn.8xlarge"|"r5dn.12xlarge"|"r5dn.16xlarge"|"r5dn.24xlarge"|"r5n.large"|"r5n.xlarge"|"r5n.2xlarge"|"r5n.4xlarge"|"r5n.8xlarge"|"r5n.12xlarge"|"r5n.16xlarge"|"r5n.24xlarge"|"inf1.xlarge"|"inf1.2xlarge"|"inf1.6xlarge"|"inf1.24xlarge",
    KeyName = "string",
    Monitoring = list(
      Enabled = TRUE|FALSE
    ),
    Placement = list(
      AvailabilityZone = "string",
      Affinity = "string",
      GroupName = "string",
      HostId = "string",
      Tenancy = "default"|"dedicated"|"host",
      SpreadDomain = "string",
      HostResourceGroupArn = "string"
    ),
    RamDiskId = "string",
    DisableApiTermination = TRUE|FALSE,
    InstanceInitiatedShutdownBehavior = "stop"|"terminate",
    UserData = "string",
    TagSpecifications = list(
      list(
        ResourceType = "client-vpn-endpoint"|"customer-gateway"|"dedicated-host"|"dhcp-options"|"elastic-ip"|"fleet"|"fpga-image"|"host-reservation"|"image"|"instance"|"internet-gateway"|"key-pair"|"launch-template"|"natgateway"|"network-acl"|"network-interface"|"placement-group"|"reserved-instances"|"route-table"|"security-group"|"snapshot"|"spot-fleet-request"|"spot-instances-request"|"subnet"|"traffic-mirror-filter"|"traffic-mirror-session"|"traffic-mirror-target"|"transit-gateway"|"transit-gateway-attachment"|"transit-gateway-multicast-domain"|"transit-gateway-route-table"|"volume"|"vpc"|"vpc-peering-connection"|"vpn-connection"|"vpn-gateway",
        Tags = list(
          list(
            Key = "string",
            Value = "string"
          )
        )
      )
    ),
    ElasticGpuSpecifications = list(
      list(
        Type = "string"
      )
    ),
    ElasticInferenceAccelerators = list(
      list(
        Type = "string",
        Count = 123
      )
    ),
    SecurityGroupIds = list(
      "string"
    ),
    SecurityGroups = list(
      "string"
    ),
    InstanceMarketOptions = list(
      MarketType = "spot",
      SpotOptions = list(
        MaxPrice = "string",
        SpotInstanceType = "one-time"|"persistent",
        BlockDurationMinutes = 123,
        ValidUntil = as.POSIXct(
          "2015-01-01"
        ),
        InstanceInterruptionBehavior = "hibernate"|"stop"|"terminate"
      )
    ),
    CreditSpecification = list(
      CpuCredits = "string"
    ),
    CpuOptions = list(
      CoreCount = 123,
      ThreadsPerCore = 123
    ),
    CapacityReservationSpecification = list(
      CapacityReservationPreference = "open"|"none",
      CapacityReservationTarget = list(
        CapacityReservationId = "string"
      )
    ),
    LicenseSpecifications = list(
      list(
        LicenseConfigurationArn = "string"
      )
    ),
    HibernationOptions = list(
      Configured = TRUE|FALSE
    )
  ),
  TagSpecifications = list(
    list(
      ResourceType = "client-vpn-endpoint"|"customer-gateway"|"dedicated-host"|"dhcp-options"|"elastic-ip"|"fleet"|"fpga-image"|"host-reservation"|"image"|"instance"|"internet-gateway"|"key-pair"|"launch-template"|"natgateway"|"network-acl"|"network-interface"|"placement-group"|"reserved-instances"|"route-table"|"security-group"|"snapshot"|"spot-fleet-request"|"spot-instances-request"|"subnet"|"traffic-mirror-filter"|"traffic-mirror-session"|"traffic-mirror-target"|"transit-gateway"|"transit-gateway-attachment"|"transit-gateway-multicast-domain"|"transit-gateway-route-table"|"volume"|"vpc"|"vpc-peering-connection"|"vpn-connection"|"vpn-gateway",
      Tags = list(
        list(
          Key = "string",
          Value = "string"
        )
      )
    )
  )
)
}
}

\examples{
\dontrun{
# This example creates a launch template that specifies the subnet in
# which to launch the instance, assigns a public IP address and an IPv6
# address to the instance, and creates a tag for the instance.
svc$create_launch_template(
  LaunchTemplateData = list(
    ImageId = "ami-8c1be5f6",
    InstanceType = "t2.small",
    NetworkInterfaces = list(
      list(
        AssociatePublicIpAddress = TRUE,
        DeviceIndex = 0L,
        Ipv6AddressCount = 1L,
        SubnetId = "subnet-7b16de0c"
      )
    ),
    TagSpecifications = list(
      list(
        ResourceType = "instance",
        Tags = list(
          list(
            Key = "Name",
            Value = "webserver"
          )
        )
      )
    )
  ),
  LaunchTemplateName = "my-template",
  VersionDescription = "WebVersion1"
)
}

}
\keyword{internal}
