#' getCHToken Function:
#'
#'A function to connect to an oauth2 server and return a security token
#' @param uri Oauth2 server address
#' @param resource Oauth2 server resource name
#' @param client_id is the Cloud Historian AppID
#' @param client_secret is the Cloud Historian AppID Secret
#' @return a security token

getCHToken <- function(uri, resource, client_id, client_secret) {
    require(httr)
    require(lubridate)

    headers1 <- c('Cache-Control' = "no-cache")
    body <- list( grant_type = "client_credentials",
                  resource = resource,
                  client_id = client_id,
                  client_secret = client_secret)

    result <- POST(uri, add_headers(headers1), body = body)
    content <- content(result)
    if (result$status_code != 200) {
      if (result$status_code == 404){
        warning(paste("Rest API call failed to retrieve a token from the oAuth2 Server. \nReturned status code:",result$status_code,"\nError: URL address is incorrect"))
        content <- list()
        content$url <- result$url
        content$status_code <- result$status_code
        content$error <- "URL address is incorrect"
        return(content)
        exit()
      }else{
        warning(paste("Rest API call failed to retrieve a token from the oAuth2 Server. \nReturned status code:",result$status_code,"\nError:",content$error,"\nMessage:",content$error_description))
      }
    }else{ #http status code is ok
      if (is.null(content) == FALSE) { #content Exisits
          #content$url <- result$url
          #content$status_code <- result$status_code
          #return(content)
      }else{
        warning(paste("Content of the http response is NULL. Returning the full http response"))
        #return(result)
      }
    }
    content$url <- result$url
    content$status_code <- result$status_code
    return(content)
}
