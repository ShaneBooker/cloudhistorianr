#' get_ch_data Function:
#'
#'A function to connect to Honewell Sentience Cloud Historian Server and return the requested timeseries data
#' @param uri Cloud Historian Server Address
#' @param token Oauth2 security token. Use getCHToken to acquire a token
#' @param start_time The start time to be used for data retrieval
#' @param end_time The start time to be used for data retrieval
#' @param tag_names A vector of tagnems to be used for data retrieval
#' @param down_sample The the downsampling function to be used in the data retrieval
#'    Valid responses:
#'    Downsample to 1 min average: "1m-avg",
#'    Downsample to 60 second average: "60s-avg",
#'    Downsample to 1 hour average: "1h-avg",
#'    Downsample to avg of all results: "0all-avg",
#'    Downsample to max of all results: "0all-max",
#'    return raw data: ""
#' @return An object with the request information and the formated data in the $data attribute.


get_ch_data <- function(uri, token, start_time, end_time, tag_names, down_sample,sys_guid) {

  require(jsonlite)
  require(httr)
  require(lubridate)

  headers <- c(
    'Content-Type' = 'application/json',
    'Accept' = 'application/json',
    'Authorization' = paste('Bearer', token)
  )

  #Build up the query
  request_tags <- "["
  for (i in seq_along(tag_names)) {
    request_tags <- paste(
      request_tags,
      paste('{"pointId":  "',tolower(tag_names[i]),
            '","systemGuid": "',sys_guid,'",
            "aggregator": "none","downsample": "', down_sample,
            '","pointAttributes": { "Quality": "Good" }}',
            sep = ""),
      sep = "")

    if (i != length(tag_names)) {
      request_tags = paste(request_tags, ",", sep = "")
    }
  }
  request_tags <- paste(request_tags, "]", sep = "")

  #Build the body of the query
  body <- toJSON(
    list(
      "startTime" = start_time,
      "endTime" = end_time,
      queries = fromJSON(request_tags)
    ) ,
    auto_unbox = TRUE)

  #REST API Call
  result <- POST(uri, add_headers(headers), body=body)
  content <- content(result)
  http_error <- http_error(result)
  http_status <- http_status(result)

  #holding list for the returned data
  return_data <- list()

  #warn if an error is found
  if (http_error) {
    warning(paste("Rest API call failed to retrieve data from Honeywell Sentience Cloud Historian. \nReturned status code:",result$status_code,"\nError:",content$error,"\nMessage:",content$message))
    return_data$error <- content

    #400 errors are often due to incorect tag and time data
    if (result$status_code == 404){
      warning(paste("Status code:",result$status_code,". Check that the URL is correct"))
    }else if (result$status_code == 400){
      warning(paste("Status code:",result$status_code,". Check that the tagname(s), timestamps and downsample are valid"))
    }
  }else{
    # Convert to a data matrix
    point_id <- c()
    value <- c()
    sample_time <- c()
    for (j in seq_along(content)) {
      x <- content[[j]]$pointValues
      y <- content[[j]]$pointId
      for (i in seq_along(x)) {
        point_id <- c(point_id, y)
        value <- c(value, as.numeric(x[[i]]))
        sample_time <- c(sample_time, as.integer(names(x[i])))
      }
    }

    return_data$data <- data.frame(
      time = as_datetime(sample_time),
      value = value,
      point_id = point_id
    )

  }


  return_data$url <- result$url
  return_data$status_code <- result$status_code
  return(return_data)

  }