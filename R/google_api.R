get_token <- function(credentials, credential_type, scopes) {
  # Args:
  #   credentials - list, from Google (parsed JSON), created in the Api
  #     Crendential interface, either "OAuth client ID" (`client_id`) or
  #     "Service account key" (`service_account_key`). See
  #     https://console.developers.google.com/apis/credentials but note
  #     carefully which project is active.
  #   credential_type - character, length 1, one of:
  #     'client_id' default, for interactive/console use by analysts
  #     'service_account_key' for automated code like RServe

  # Read about OAuth settings for google here:
  #   https://github.com/hadley/httr/blob/master/demo/oauth2-google.r
  #   https://developers.google.com/accounts/docs/OAuth2InstalledApp
  #   https://developers.google.com/identity/protocols/OAuth2WebServer
  # The httr library has the necessary stuff hard coded, accessible like this:
  #   oauth_endpoints("google")
  # https://cran.r-project.org/web/packages/httr/httr.pdf

  if (credential_type %in% 'client_id') {
    oauth_app <- httr::oauth_app("google", key = credentials$client_id,
                                 secret = credentials$client_secret)
    # Get OAuth access token to attach to API calls. Pauses execution
    # while the user goes to their browser, approves the permission, and
    # returns with a code.
    token <- httr::oauth2.0_token(
      httr::oauth_endpoints("google"),
      oauth_app,
      scopes
    )
  } else if (credential_type %in% 'service_account_key') {
    endpoint <- httr::oauth_endpoints("google")
    token <- httr::oauth_service_token(endpoint, credentials, scopes)
  } else {
    stop(paste0("Unknown value for `credential_type`: ", credential_type))
  }

  return(token)
}
