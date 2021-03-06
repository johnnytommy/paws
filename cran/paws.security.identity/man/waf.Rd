% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/waf_service.R
\name{waf}
\alias{waf}
\title{AWS WAF}
\usage{
waf(config = list())
}
\arguments{
\item{config}{Optional configuration of credentials, endpoint, and/or region.}
}
\description{
This is the \emph{AWS WAF API Reference} for using AWS WAF with Amazon
CloudFront. The AWS WAF actions and data types listed in the reference
are available for protecting Amazon CloudFront distributions. You can
use these actions and data types via the endpoint \emph{waf.amazonaws.com}.
This guide is for developers who need detailed information about the AWS
WAF API actions, data types, and errors. For detailed information about
AWS WAF features and an overview of how to use the AWS WAF API, see the
\href{https://docs.aws.amazon.com/waf/latest/developerguide/}{AWS WAF Developer Guide}.
}
\section{Service syntax}{
\preformatted{svc <- waf(
  config = list(
    credentials = list(
      creds = list(
        access_key_id = "string",
        secret_access_key = "string",
        session_token = "string"
      ),
      profile = "string"
    ),
    endpoint = "string",
    region = "string"
  )
)
}
}

\section{Operations}{

\tabular{ll}{
\link[=waf_create_byte_match_set]{create_byte_match_set} \tab Creates a ByteMatchSet \cr
\link[=waf_create_geo_match_set]{create_geo_match_set} \tab Creates an GeoMatchSet, which you use to specify which web requests you want to allow or block based on the country that the requests originate from \cr
\link[=waf_create_ip_set]{create_ip_set} \tab Creates an IPSet, which you use to specify which web requests that you want to allow or block based on the IP addresses that the requests originate from \cr
\link[=waf_create_rate_based_rule]{create_rate_based_rule} \tab Creates a RateBasedRule \cr
\link[=waf_create_regex_match_set]{create_regex_match_set} \tab Creates a RegexMatchSet \cr
\link[=waf_create_regex_pattern_set]{create_regex_pattern_set} \tab Creates a RegexPatternSet \cr
\link[=waf_create_rule]{create_rule} \tab Creates a Rule, which contains the IPSet objects, ByteMatchSet objects, and other predicates that identify the requests that you want to block \cr
\link[=waf_create_rule_group]{create_rule_group} \tab Creates a RuleGroup \cr
\link[=waf_create_size_constraint_set]{create_size_constraint_set} \tab Creates a SizeConstraintSet \cr
\link[=waf_create_sql_injection_match_set]{create_sql_injection_match_set} \tab Creates a SqlInjectionMatchSet, which you use to allow, block, or count requests that contain snippets of SQL code in a specified part of web requests \cr
\link[=waf_create_web_acl]{create_web_acl} \tab Creates a WebACL, which contains the Rules that identify the CloudFront web requests that you want to allow, block, or count \cr
\link[=waf_create_xss_match_set]{create_xss_match_set} \tab Creates an XssMatchSet, which you use to allow, block, or count requests that contain cross-site scripting attacks in the specified part of web requests \cr
\link[=waf_delete_byte_match_set]{delete_byte_match_set} \tab Permanently deletes a ByteMatchSet \cr
\link[=waf_delete_geo_match_set]{delete_geo_match_set} \tab Permanently deletes a GeoMatchSet \cr
\link[=waf_delete_ip_set]{delete_ip_set} \tab Permanently deletes an IPSet \cr
\link[=waf_delete_logging_configuration]{delete_logging_configuration} \tab Permanently deletes the LoggingConfiguration from the specified web ACL \cr
\link[=waf_delete_permission_policy]{delete_permission_policy} \tab Permanently deletes an IAM policy from the specified RuleGroup \cr
\link[=waf_delete_rate_based_rule]{delete_rate_based_rule} \tab Permanently deletes a RateBasedRule \cr
\link[=waf_delete_regex_match_set]{delete_regex_match_set} \tab Permanently deletes a RegexMatchSet \cr
\link[=waf_delete_regex_pattern_set]{delete_regex_pattern_set} \tab Permanently deletes a RegexPatternSet \cr
\link[=waf_delete_rule]{delete_rule} \tab Permanently deletes a Rule \cr
\link[=waf_delete_rule_group]{delete_rule_group} \tab Permanently deletes a RuleGroup \cr
\link[=waf_delete_size_constraint_set]{delete_size_constraint_set} \tab Permanently deletes a SizeConstraintSet \cr
\link[=waf_delete_sql_injection_match_set]{delete_sql_injection_match_set} \tab Permanently deletes a SqlInjectionMatchSet \cr
\link[=waf_delete_web_acl]{delete_web_acl} \tab Permanently deletes a WebACL \cr
\link[=waf_delete_xss_match_set]{delete_xss_match_set} \tab Permanently deletes an XssMatchSet \cr
\link[=waf_get_byte_match_set]{get_byte_match_set} \tab Returns the ByteMatchSet specified by ByteMatchSetId \cr
\link[=waf_get_change_token]{get_change_token} \tab When you want to create, update, or delete AWS WAF objects, get a change token and include the change token in the create, update, or delete request \cr
\link[=waf_get_change_token_status]{get_change_token_status} \tab Returns the status of a ChangeToken that you got by calling GetChangeToken \cr
\link[=waf_get_geo_match_set]{get_geo_match_set} \tab Returns the GeoMatchSet that is specified by GeoMatchSetId \cr
\link[=waf_get_ip_set]{get_ip_set} \tab Returns the IPSet that is specified by IPSetId \cr
\link[=waf_get_logging_configuration]{get_logging_configuration} \tab Returns the LoggingConfiguration for the specified web ACL \cr
\link[=waf_get_permission_policy]{get_permission_policy} \tab Returns the IAM policy attached to the RuleGroup \cr
\link[=waf_get_rate_based_rule]{get_rate_based_rule} \tab Returns the RateBasedRule that is specified by the RuleId that you included in the GetRateBasedRule request \cr
\link[=waf_get_rate_based_rule_managed_keys]{get_rate_based_rule_managed_keys} \tab Returns an array of IP addresses currently being blocked by the RateBasedRule that is specified by the RuleId \cr
\link[=waf_get_regex_match_set]{get_regex_match_set} \tab Returns the RegexMatchSet specified by RegexMatchSetId \cr
\link[=waf_get_regex_pattern_set]{get_regex_pattern_set} \tab Returns the RegexPatternSet specified by RegexPatternSetId \cr
\link[=waf_get_rule]{get_rule} \tab Returns the Rule that is specified by the RuleId that you included in the GetRule request \cr
\link[=waf_get_rule_group]{get_rule_group} \tab Returns the RuleGroup that is specified by the RuleGroupId that you included in the GetRuleGroup request \cr
\link[=waf_get_sampled_requests]{get_sampled_requests} \tab Gets detailed information about a specified number of requests--a sample--that AWS WAF randomly selects from among the first 5,000 requests that your AWS resource received during a time range that you choose\cr
\link[=waf_get_size_constraint_set]{get_size_constraint_set} \tab Returns the SizeConstraintSet specified by SizeConstraintSetId \cr
\link[=waf_get_sql_injection_match_set]{get_sql_injection_match_set} \tab Returns the SqlInjectionMatchSet that is specified by SqlInjectionMatchSetId \cr
\link[=waf_get_web_acl]{get_web_acl} \tab Returns the WebACL that is specified by WebACLId \cr
\link[=waf_get_xss_match_set]{get_xss_match_set} \tab Returns the XssMatchSet that is specified by XssMatchSetId \cr
\link[=waf_list_activated_rules_in_rule_group]{list_activated_rules_in_rule_group} \tab Returns an array of ActivatedRule objects \cr
\link[=waf_list_byte_match_sets]{list_byte_match_sets} \tab Returns an array of ByteMatchSetSummary objects \cr
\link[=waf_list_geo_match_sets]{list_geo_match_sets} \tab Returns an array of GeoMatchSetSummary objects in the response \cr
\link[=waf_list_ip_sets]{list_ip_sets} \tab Returns an array of IPSetSummary objects in the response \cr
\link[=waf_list_logging_configurations]{list_logging_configurations} \tab Returns an array of LoggingConfiguration objects \cr
\link[=waf_list_rate_based_rules]{list_rate_based_rules} \tab Returns an array of RuleSummary objects \cr
\link[=waf_list_regex_match_sets]{list_regex_match_sets} \tab Returns an array of RegexMatchSetSummary objects \cr
\link[=waf_list_regex_pattern_sets]{list_regex_pattern_sets} \tab Returns an array of RegexPatternSetSummary objects \cr
\link[=waf_list_rule_groups]{list_rule_groups} \tab Returns an array of RuleGroup objects \cr
\link[=waf_list_rules]{list_rules} \tab Returns an array of RuleSummary objects \cr
\link[=waf_list_size_constraint_sets]{list_size_constraint_sets} \tab Returns an array of SizeConstraintSetSummary objects \cr
\link[=waf_list_sql_injection_match_sets]{list_sql_injection_match_sets} \tab Returns an array of SqlInjectionMatchSet objects \cr
\link[=waf_list_subscribed_rule_groups]{list_subscribed_rule_groups} \tab Returns an array of RuleGroup objects that you are subscribed to \cr
\link[=waf_list_tags_for_resource]{list_tags_for_resource} \tab List tags for resource \cr
\link[=waf_list_web_ac_ls]{list_web_ac_ls} \tab Returns an array of WebACLSummary objects in the response \cr
\link[=waf_list_xss_match_sets]{list_xss_match_sets} \tab Returns an array of XssMatchSet objects \cr
\link[=waf_put_logging_configuration]{put_logging_configuration} \tab Associates a LoggingConfiguration with a specified web ACL \cr
\link[=waf_put_permission_policy]{put_permission_policy} \tab Attaches a IAM policy to the specified resource \cr
\link[=waf_tag_resource]{tag_resource} \tab Tag resource \cr
\link[=waf_untag_resource]{untag_resource} \tab Untag resource \cr
\link[=waf_update_byte_match_set]{update_byte_match_set} \tab Inserts or deletes ByteMatchTuple objects (filters) in a ByteMatchSet \cr
\link[=waf_update_geo_match_set]{update_geo_match_set} \tab Inserts or deletes GeoMatchConstraint objects in an GeoMatchSet \cr
\link[=waf_update_ip_set]{update_ip_set} \tab Inserts or deletes IPSetDescriptor objects in an IPSet \cr
\link[=waf_update_rate_based_rule]{update_rate_based_rule} \tab Inserts or deletes Predicate objects in a rule and updates the RateLimit in the rule \cr
\link[=waf_update_regex_match_set]{update_regex_match_set} \tab Inserts or deletes RegexMatchTuple objects (filters) in a RegexMatchSet \cr
\link[=waf_update_regex_pattern_set]{update_regex_pattern_set} \tab Inserts or deletes RegexPatternString objects in a RegexPatternSet \cr
\link[=waf_update_rule]{update_rule} \tab Inserts or deletes Predicate objects in a Rule \cr
\link[=waf_update_rule_group]{update_rule_group} \tab Inserts or deletes ActivatedRule objects in a RuleGroup \cr
\link[=waf_update_size_constraint_set]{update_size_constraint_set} \tab Inserts or deletes SizeConstraint objects (filters) in a SizeConstraintSet \cr
\link[=waf_update_sql_injection_match_set]{update_sql_injection_match_set} \tab Inserts or deletes SqlInjectionMatchTuple objects (filters) in a SqlInjectionMatchSet \cr
\link[=waf_update_web_acl]{update_web_acl} \tab Inserts or deletes ActivatedRule objects in a WebACL \cr
\link[=waf_update_xss_match_set]{update_xss_match_set} \tab Inserts or deletes XssMatchTuple objects (filters) in an XssMatchSet
}
}

\examples{
\dontrun{
svc <- waf()
# The following example creates an IP match set named MyIPSetFriendlyName.
svc$create_ip_set(
  ChangeToken = "abcd12f2-46da-4fdb-b8d5-fbd4c466928f",
  Name = "MyIPSetFriendlyName"
)
}

}
