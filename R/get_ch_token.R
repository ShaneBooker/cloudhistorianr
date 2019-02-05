#' get_ch_token Function:
#'
#'A function to connect to an oauth2 server and return a security token
#' @param uri Oauth2 server address
#' @param resource Oauth2 server resource name
#' @param client_id Cloud Historian AppID
#' @param client_secret Cloud Historian AppID Secret
#' @return Security token

get_ch_token <- function(uri, resource, client_id, client_secret) {
    require(httr)
    require(lubridate)

    #raise error if the url is invalid
    if(http_error(uri)){
      stop(paste("URL:",uri,"is not valid."))
    }

    headers1 <- c('Cache-Control' = "no-cache")
    body <- list( grant_type = "client_credentials",
                  resource = resource,
                  client_id = client_id,
                  client_secret = client_secret)
    result <- POST(uri, add_headers(headers1), body = body)

    #standardise the results to known formats
    content <- content(result)
    http_error <- http_error(result)
    http_status <- http_status(result)

    #warn if an error is found
    if (http_error) {
      warning(paste("Rest API call failed to retrieve a token from the oAuth2 Server. \nReturned status code:",result$status_code,"\nError:",content$error,"\nMessage:",content$error_description))
    }

    #warn if no content is returned
    if (is.null(content)) { #no content exists
      warning(paste("Content of the http response is NULL."))
      content <- list()
    }

    content$url <- result$url
    content$http_status <- http_status
    content$status_code <- result$status_code
    return(content)

}
