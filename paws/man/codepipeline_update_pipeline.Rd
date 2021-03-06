% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/codepipeline_operations.R
\name{codepipeline_update_pipeline}
\alias{codepipeline_update_pipeline}
\title{Updates a specified pipeline with edits or changes to its structure}
\usage{
codepipeline_update_pipeline(pipeline)
}
\arguments{
\item{pipeline}{[required] The name of the pipeline to be updated.}
}
\description{
Updates a specified pipeline with edits or changes to its structure. Use
a JSON file with the pipeline structure and \code{UpdatePipeline} to provide
the full structure of the pipeline. Updating the pipeline increases the
version number of the pipeline by 1.
}
\section{Request syntax}{
\preformatted{svc$update_pipeline(
  pipeline = list(
    name = "string",
    roleArn = "string",
    artifactStore = list(
      type = "S3",
      location = "string",
      encryptionKey = list(
        id = "string",
        type = "KMS"
      )
    ),
    artifactStores = list(
      list(
        type = "S3",
        location = "string",
        encryptionKey = list(
          id = "string",
          type = "KMS"
        )
      )
    ),
    stages = list(
      list(
        name = "string",
        blockers = list(
          list(
            name = "string",
            type = "Schedule"
          )
        ),
        actions = list(
          list(
            name = "string",
            actionTypeId = list(
              category = "Source"|"Build"|"Deploy"|"Test"|"Invoke"|"Approval",
              owner = "AWS"|"ThirdParty"|"Custom",
              provider = "string",
              version = "string"
            ),
            runOrder = 123,
            configuration = list(
              "string"
            ),
            outputArtifacts = list(
              list(
                name = "string"
              )
            ),
            inputArtifacts = list(
              list(
                name = "string"
              )
            ),
            roleArn = "string",
            region = "string",
            namespace = "string"
          )
        )
      )
    ),
    version = 123
  )
)
}
}

\keyword{internal}
