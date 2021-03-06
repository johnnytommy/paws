# This file is generated by make.paws. Please do not edit here.
#' @importFrom paws.common new_handlers new_service set_config
NULL

#' Amazon CodeGuru Reviewer
#'
#' @description
#' This section provides documentation for the Amazon CodeGuru Reviewer API
#' operations.
#'
#' @param
#' config
#' Optional configuration of credentials, endpoint, and/or region.
#'
#' @section Service syntax:
#' ```
#' svc <- codegurureviewer(
#'   config = list(
#'     credentials = list(
#'       creds = list(
#'         access_key_id = "string",
#'         secret_access_key = "string",
#'         session_token = "string"
#'       ),
#'       profile = "string"
#'     ),
#'     endpoint = "string",
#'     region = "string"
#'   )
#' )
#' ```
#'
#' @examples
#' \dontrun{
#' svc <- codegurureviewer()
#' svc$associate_repository(
#'   Foo = 123
#' )
#' }
#'
#' @section Operations:
#' \tabular{ll}{
#'  \link[=codegurureviewer_associate_repository]{associate_repository} \tab Associates an AWS CodeCommit repository with Amazon CodeGuru Reviewer \cr
#'  \link[=codegurureviewer_describe_repository_association]{describe_repository_association} \tab Describes a repository association \cr
#'  \link[=codegurureviewer_disassociate_repository]{disassociate_repository} \tab Removes the association between Amazon CodeGuru Reviewer and a repository\cr
#'  \link[=codegurureviewer_list_repository_associations]{list_repository_associations} \tab Lists repository associations 
#' }
#'
#' @rdname codegurureviewer
#' @export
codegurureviewer <- function(config = list()) {
  svc <- .codegurureviewer$operations
  svc <- set_config(svc, config)
  return(svc)
}

# Private API objects: metadata, handlers, interfaces, etc.
.codegurureviewer <- list()

.codegurureviewer$operations <- list()

.codegurureviewer$metadata <- list(
  service_name = "codegurureviewer",
  endpoints = list("*" = list(endpoint = "codegurureviewer.{region}.amazonaws.com", global = FALSE), "cn-*" = list(endpoint = "codegurureviewer.{region}.amazonaws.com.cn", global = FALSE)),
  service_id = "CodeGuru Reviewer",
  api_version = "2019-09-19",
  signing_name = "codeguru-reviewer",
  json_version = "1.1",
  target_prefix = ""
)

.codegurureviewer$service <- function(config = list()) {
  handlers <- new_handlers("restjson", "v4")
  new_service(.codegurureviewer$metadata, handlers, config)
}
