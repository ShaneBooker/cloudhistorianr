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

  }else{ #http success

    #check that content of the request includes data
    if(length(content) == 0 ){
      warning(paste("No data was returned. Search criteria:\nStart time:",start_time,
        "\nEnd time:",end_time,
        "\nDown sampling:",down_sample,
        "\nTag names:",paste(tag_names,collapse=", ")
        ))

    }else{
      # Convert to a data matrix
      point_id <- c()
      value <- c()
      sample_time <- c()

      #data is returned grouped by pointid. This will break up that grouping and create 3 vectors: pointid, pointvalue and timestamp
      for (j in seq_along(content)) {
        temp_point_values <- content[[j]]$pointValues
        temp_point_id <- content[[j]]$pointId

        if(length(temp_point_values) > 0){
          for (i in seq_along(temp_point_values)) {
            #create the 3 holding vectors
            point_id <- c(point_id, temp_point_id)
            value <- c(value, as.numeric(temp_point_values[[i]]))
            sample_time <- c(sample_time, as.integer(names(temp_point_values[i])/1000000)) #2019-12-2 API now responds in nano time, this is to take the result back to mili-seconds
          }
        }else{
          warning(paste("no data values found for tag",temp_point_id))
        }
      }

      if(length(point_id)>0){
        #repackage the 3 vectors as a data frame
        return_data$data <- data.frame(
            time = as_datetime(sample_time),
            value = value,
            point_id = point_id
          )
      }
    }

  }


  return_data$url <- result$url
  return_data$status_code <- result$status_code
  return(return_data)

  }
